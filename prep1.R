
library(tidyverse)

crime_coltypes <- cols(
  year_id = 'c',
  dr_no = 'c',
  date_occ = col_date(format = '%Y-%m-%d'),
  date_rptd = col_date(format = '%Y-%m-%d'),
  area_name = 'c',
  rd = 'c',
  crm_cd_desc = 'c',
  status_desc = 'c',
  location = 'c',
  cross_st = 'c',
  lat = 'd',
  long = 'd',
  year = 'i',
  month = 'i',
  day_of_month = 'i',
  hour_of_day = 'i',
  day_of_week = 'c',
  simple_bucket = 'c',
  
  time_occ = '_', area = '_', crm_cd = '_', status = '_',
  intersection = '_'
  
)

crimes <- read_csv(
  'stop_data.csv',
  col_types = crime_coltypes
)

write_rds(crimes, 'processed_data/crimes.rds')