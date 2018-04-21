area_divs <- c(
  'CENTRAL', 'HOLLENBECK', 'NEWTON', 'NORTH EAST', 'RAMPART',
  'SEVENTY-SEVENTH', 'HARBOR', 'SOUTH EAST', 'SOUTH WEST',
  'DEVONSHIRE', 'FOOTHILL', 'MISSION', 'NORTH HOLLYWOOD', 'VAN NUYS',
  'WEST VALLEY', 'TOPANGA',
  'HOLLYWOOD', 'OLYMPIC', 'PACIFIC', 'WEST LA', 'WILSHIRE'
)

traffic_divs <- c(
  'CENTRAL TRAFFIC', 'SOUTH TRAFFIC', 'VALLEY TRAFFIC', 'WEST TRAFFIC'
)

selected_divs <- as.factor(c(area_divs, traffic_divs))

races_for_analysis <- c("BLACK","HISPANIC","WHITE")


get_min_max_dates <- function(stops) {
  date_pivot <- stops %>%
    group_by(STOP_DT) %>%
    summarize(sunset_time = min(Sunset)) %>%
    arrange(sunset_time)
  
  min_part <- date_pivot %>%
    head(1)
  max_part <- date_pivot %>%
    tail(1)
  return(bind_rows(min_part, max_part))
}

# define intertwilight
# we will only analyze stops that fall within intertwilight range
mutate_is_intertwilight <- function(stops, min_max_sunsets) {
  stops %>%
    mutate(
      is_intertwilight = 
        (STOP_TM >= min_max_sunsets[[1,2]] & STOP_TM <= min_max_sunsets[[2,2]]) &
        (STOP_TM >= Sunset + 30 * 60 | STOP_TM < Sunset)  
      # ^ 30 min buffer for uncertain visibility near sundown
    )
}

mutate_is_daylight <- function(stops) {
  stops %>%
    mutate(
      is_daylight = Sunrise <= STOP_TM & STOP_TM <= Sunset
    )
}


# plot helpers ------


gen_race_diff_plot <- function(race_diffs, div_by_race, race_diff, race_name) {
  race_diffs %>%
    mutate(is_positive = race_diff > 0 ) %>%  # for color
    ggplot(aes(div_by_race, race_diff, fill = is_positive)) + 
    geom_bar(stat = 'identity', color = 'black') + 
    coord_flip() + 
    theme_bw() + 
    scale_y_continuous(label = percent, limits = c(-.3, .3)) + 
    labs(x = '', y = '', title = '', subtitle = race_name) + 
    theme(legend.position = 'none')
}

gen_race_prop_test_plot <- function(race_prop_tests, race, subtitle) {
  race_prop_tests %>%
    filter(race == race) %>%
    mutate(division = fct_reorder(division, p.value, .desc = T)) %>%
    ggplot(aes(division, p.value, fill = is_significant)) + 
    geom_bar(stat = 'identity', color = 'black') + 
    coord_flip() + 
    theme_bw() +
    scale_y_continuous(label = percent, limits = c(0,1)) +
    theme(legend.position = 'none') + 
    labs(x = '', y = '', title = '', subtitle = subtitle)
}

# testing helpers -----
prop_test_all_divs <- function(race_df, race_diff_string) {
  race_results <- vector('list', length(unique(filt_stops$DIV1_DESC)))
  identifier_cols <- race_df %>%
    select(division = DIV1_DESC) %>%
    slice(seq(1, nrow(race_df), by = 2)) %>%
    mutate(race = race_diff_string)
  
  i <- 1
  for (row in seq(1, nrow(race_df), by = 2)) {
    race_results[[i]] <- tidy(
      prop.test(
        x = race_df[[race_diff_string]][seq(row,row+1)], 
        n = race_df[['total_stops']][seq(row,row+1)],
        alternative = 'less'
      )
    )
    i <- i + 1
  }
  
  bind_rows(race_results) %>%
    bind_cols(identifier_cols)
}

prop_test_all <- function(total_df, race_string) {
  tidy(
    prop.test(
      x = total_df[[race_string]],
      n = total_df[['total_stops']],
      alternative = 'less'
    )
  )
}
