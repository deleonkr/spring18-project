library(tidyverse)

source('helpers.R')

stop_coltypes <- cols(
  STOP_NBR = 'c',
  FORM_REF_NBR = 'c',
  PERSN_GENDER_CD = 'c',
  PERSN_DESCENT_CD = 'c',
  STOP_DT = col_date(format = '%m/%d/%Y'),
  STOP_TM = col_time(),
  OFCR1_SERL_NBR = 'c',
  DIV1_DESC = 'c',
  OFCR2_SERL_NBR = 'c',
  OFCR2_DIV_NBR = 'c',
  DIV2_DESC = 'c',
  RPT_DIST_NBR = 'c',
  STOP_TYPE = 'c',
  POST_STOP_ACTV_IND = 'c'
)

stops <- read_csv(
  'raw_data/stop_data.csv',
  col_types = stop_coltypes
)

sunsets <- read_rds('processed_data/sunset_times.rds')

# join in sunrise/sunset times by data
stops_with_sun <- left_join(
  stops, sunsets,
  by = c('STOP_DT' = 'Date')
)

write_rds(stops_with_sun, 'processed_data/stops_with_sun.rds')

# filter to divisions we will test later ----
stops <- stops_with_sun %>%
  mutate(
    STOP_TM = as.numeric(STOP_TM),
    Sunrise = as.numeric(Sunrise),
    Sunset = as.numeric(Sunset),
    day_length_secs = as.numeric(day_length_secs),
    # clean up labels
    PERSN_GENDER_CD = fct_recode(
      PERSN_GENDER_CD,
      Male = 'M', Female = 'F'
    ),
    STOP_TYPE = fct_recode(
      STOP_TYPE, 
      `Vehicle Stop` = 'VEH', `Pedestrian Stop` = 'PED' 
    )
  )

# get earliest/latest sunset times
(min_max_sunsets <- get_min_max_dates(stops))

# thus, intertwilight time is between
# 16:43:31 - 20:08:30
stops <- stops %>%
  mutate_is_intertwilight(min_max_sunsets) %>%
  mutate_is_daylight()

write_rds(stops, 'processed_data/prepared_stops.rds')

# filter to these stops for testing purposes -----
# may want to control for days of the week or weekends
filt_stops <- stops %>%
  filter(
    DIV1_DESC %in% selected_divs,
    DESCENT_DESC %in% races_for_analysis,
    is_intertwilight,
    STOP_TYPE == 'Vehicle Stop'
  )

write_rds(filt_stops, 'processed_data/filt_stops.rds')



# tmp
# check_stops <- filt_stops %>%
#   select(DIV1_DESC, STOP_DT, STOP_TM, Sunrise, Sunset, contains('is_'),
#          everything())
#   
# check_stops %>%
#   datatable()
