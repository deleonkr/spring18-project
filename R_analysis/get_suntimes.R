library(tidyverse)
library(rvest)

sunset_coltypes <- cols(
  Date = col_date(format = '%m/%d/%Y'),
  Sunrise = col_time(format = '%H:%M:%S %p'),
  Sunset = col_time(format = '%H:%M:%S %p'),
  Daylength = col_character()
)

sunsets <- read_tsv(
  'raw_data/la_suntimes.txt',
  col_types = sunset_coltypes
) %>%
  mutate(day_length_secs = Sunset - Sunrise)

write_rds(sunsets, 'processed_data/sunset_times.rds')
