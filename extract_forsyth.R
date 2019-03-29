###################################################################
#
# This script extracts Forsyth County child care facilities
# for the most recent month, so that addresses can be added manually.
#
###################################################################

library(tidyverse)

# import the most recent month and year of data
forsyth <- read_csv('data/nc_january_2019.csv') %>%
  # we only need these three columns
  select(id, name, county) %>%
  # we only need Forsyth County
  filter(county == 'Forsyth')

