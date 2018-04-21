#STATISTICAL ANALYSIS
library(tidyverse)
library(scales)
library(DT)
library(forcats)
library(lubridate)
library(gridExtra)
library(broom)
library(caret)

source('helpers.R')

# already filtered to only interesting divisions
stops <- read_rds('processed_data/filt_stops.rds')
# filter to only BLACKS, ASIANS, HISPANICS, and WHITES
# only during INTERTWILIGHT TIME
# only VEHICLE STOPS

filt_stops <- stops %>%
  filter(
    !weekdays(STOP_DT) %in% c('Saturday', 'Sunday')
  )

dim(filt_stops) # ~ 43k stops

#GROUP DATA
race_sum <- filt_stops %>%
  group_by(DIV1_DESC, is_daylight) %>%
  summarize(
    total_stops = n(),
    white_stops = sum(DESCENT_DESC == 'WHITE'),
    black_stops = sum(DESCENT_DESC == 'BLACK'),
    hispanic_stops = sum(DESCENT_DESC == 'HISPANIC'),
    # calculate distribution % of race across day / night stops
    white_perc = white_stops / total_stops,
    black_perc = black_stops / total_stops,
    hispanic_perc = hispanic_stops / total_stops
  )

race_diffs <- race_sum %>%
  select(div = DIV1_DESC, contains('perc')) %>%
  mutate(
    white_diff = lag(white_perc) - white_perc,
    black_diff = lag(black_perc) - black_perc,
    hisp_diff = lag(hispanic_perc) - hispanic_perc
  ) %>%
  filter(!is.na(white_diff)) %>%
  ungroup() %>%
  mutate(
    div_by_white = fct_reorder(div, white_diff, .desc = T),
    div_by_black = fct_reorder(div, black_diff, .desc = T),
    div_by_hisp = fct_reorder(div, hisp_diff, .desc = T)
  )

white_p <- race_diffs %>%
  gen_race_diff_plot(race_diffs$div_by_white, race_diffs$white_diff, 'Whites')
black_p <- race_diffs %>%
  gen_race_diff_plot(race_diffs$div_by_black, race_diffs$black_diff, 'Blacks')
hispanic_p <- race_diffs %>%
  gen_race_diff_plot(race_diffs$div_by_hisp, race_diffs$hisp_diff, 'Hispanics')

prop_diff_plot <- grid.arrange(
  black_p, hispanic_p, white_p, nrow = 1,
  top = "Differences in Proportion of Night Stops - Day Stops", 
  bottom = 'Divisions at the top stop more during visible hours than non-visible'
)

#PERFORM PROPORTION TEST
total_df <- race_sum %>% 
  ungroup() %>% 
  group_by(is_daylight) %>% 
  summarize(
    total_stops = sum(total_stops),
    white_stops = sum(white_stops),
    black_stops = sum(black_stops),
    hispanic_stops = sum(hispanic_stops)
  ) %>%
  mutate(
    white_percent = white_stops / total_stops,
    black_percent = black_stops / total_stops,
    hispanic_percent = hispanic_stops / total_stops
  )

total_df %>%
  prop_test_all('black_stops')
purrr::map_df(c('white_stops','black_stops','hispanic_stops'),
              prop_test_all, total_df = total_df)

white_prop_tests <- prop_test_all_divs(race_sum, 'white_stops') %>%
  mutate(is_significant = p.value < 0.05)
black_prop_tests <- prop_test_all_divs(race_sum, 'black_stops') %>%
  mutate(is_significant = p.value < 0.05)
hisp_prop_tests <- prop_test_all_divs(race_sum, 'hispanic_stops') %>%
  mutate(is_significant = p.value < 0.05)

white_plot <- gen_race_prop_test_plot(white_prop_tests, 'white_stops', 'White')
black_plot <- gen_race_prop_test_plot(black_prop_tests, 'black_stops', 'Blacks')
hisp_plot <- gen_race_prop_test_plot(hisp_prop_tests, 'hispanic_stops', 'Hispanics')


grid.arrange(
  black_plot, hisp_plot, white_plot,
  nrow = 1, top = "P Value Distributions",
  bottom = 'Divisions near the top have the smallest, most significant p values'
)


white_df <- white_prop_tests %>%
  select(division, race, p.value) %>%
  mutate(is_bonferroni_significant = p.value < 0.05 / length(unique(division)))
black_df <- black_prop_tests  %>%
  select(division, race, p.value) %>%
  mutate(is_bonferroni_significant = p.value < 0.05 / length(unique(division)))
hisp_df <- hisp_prop_tests %>%
  select(division, race, p.value) %>%
  mutate(is_bonferroni_significant = p.value < 0.05 / length(unique(division)))


# bonferroni correction
bonferroni_divisions <- bind_rows(white_df, black_df, hisp_df) %>%
  filter(is_bonferroni_significant) %>%
  select(-is_bonferroni_significant) %>%
  arrange(p.value) %>%
  mutate(division = fct_reorder(division, p.value),
         race = fct_recode(race,
                           blacks = 'black_stops',
                           whites = 'white_stops',
                           hispanics = 'hispanic_stops'))

bonferroni_divisions %>%
  ggplot(aes(division, p.value, fill = race)) + 
  geom_bar(stat = 'identity', color = 'black') + 
  theme_bw() + 
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  coord_flip() + 
  scale_y_continuous(trans = 'log10') + 
  labs(
    x = '', y = 'log10(p value)', 
    title = 'Whites in Traffic Divisions Would Appear the Most Targeted', 
    subtitle = 'smaller, more significant p values <--> larger, less significant p values')

##LOGISTIC REGRESSION
hour_min_labels <- c(
  "4:44 - 5:04", "5:04 - 5:25", "5:25 - 5:45", "5:45 - 6:05", 
  "6:05 - 6:26", "6:26 - 6:46", "6:46 - 7:07", 
  "7:07 - 7:27", "7:27 - 7:47", "7:47 - 8:08"
)
lr_stops <- filt_stops %>%
  mutate(
    stop_time_hour = STOP_TM / 3600, 
    stop_time_group = cut(stop_time_hour, breaks = 10, 
                          labels = hour_min_labels),
    is_black = DESCENT_DESC == 'BLACK',
    is_hispanic = DESCENT_DESC == 'HISPANIC',
    is_white = DESCENT_DESC == 'WHITE'
  )

lr_group <- lr_stops %>%
  group_by(is_daylight, stop_time_group) %>%
  summarize(
    total_stops = n(),
    black_stops = sum(is_black),
    hisp_stops = sum(is_hispanic),
    white_stops = sum(is_white)
  ) %>%
  mutate(
    black_percent = black_stops / total_stops,
    hisp_percent = hisp_stops / total_stops,
    white_percent = white_stops / total_stops
  )

# green = blacks, brown = hispanics, orange = whites
lr_group %>%
  ggplot(aes(stop_time_group, group = is_daylight, linetype = is_daylight)) + 
  geom_line(aes(y = black_percent), color = '#3E5641',
            size = 1.2, show.legend = F) +
  geom_line(aes(y = hisp_percent), color = '#A24936',
            size = 1.2, show.legend = F) +
  geom_line(aes(y = white_percent), color = '#D36135',
            size = 1.2, show.legend = F) +
  theme_bw() + 
  labs(x = '', y = 'Percentage of Stops',
       title = 'Stop Percentages in Daylight and Darkness in Inter-Twilight Period',
       subtitle = 'Solid Lines: Night %,       Dashed Lines: Day %') +
  scale_y_continuous(limits = c(0, 1), label = scales::percent) + 
  geom_label(aes('5:25 - 5:45', .57, label = 'Hispanic Motorists'),
             nudge_x = .5) +
  geom_label(aes('6:26 - 6:46', .35, label = 'Black Motorists'),
             nudge_x = .5) + 
  geom_label(aes('7:27 - 7:47', .12, label = 'White Motorists'),
             nudge_x = 0)

##TEST LOGISTIC REGRESSION
lr_ok <- lr_stops %>%
  mutate(
    is_black = as.factor(ifelse(is_black, 'black', 'not black')),
    is_hispanic = ifelse(is_hispanic, 'hispanic', 'not hispanic'),
    is_white = ifelse(is_white, 'white', 'not white'),
    is_daylight = ifelse(is_daylight, 'visible', 'not visible')
  )
set.seed(999)
lr_train_index <- 
  createDataPartition(lr_ok$is_black, list = F, p = 0.7)
lr_train <- lr_ok[lr_train_index, ]
lr_test <- lr_ok[-lr_train_index, ]

black_model <- glm(
  is_black ~ is_daylight + stop_time_group,
  data = lr_ok, family = binomial(link = 'logit')
)
summary(black_model)

lr_test_probs <- 
  predict(black_model, newdata = lr_test, type = 'response')
lr_test_preds <- ifelse(lr_test_probs > 0.72, 'black', 'not black')
confusionMatrix(lr_test_preds, lr_test$is_black, positive = 'black')