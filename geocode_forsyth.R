##################################################################
#
# This script creates a data set of Forsyth County
# child care facilites that are geo coded by adding the lat / long
# to the address.  It feeds the address to the Google Maps API
# to retrieve the lat / long
#
# Note: Users will need to set up a Google API key!!
# use `register_google(key = '')` to enter your Google Maps API key into R
#
####################################################################

library(tidyverse)
library(ggmap)

# import facility data for whole state and most recent month
facilities <- read_csv('data/nc_child_care_addresses.csv') %>%
  # create a single column with address information;
  # this is needed to feed addresses to the Google API to get lat and long
  mutate(address = str_c(street, ', ', city, ', NC ', zip))

# we want to limit the number of addresses we feed to the Google API,
# so create a dataset of only Forsyth addresses to feed to the API
forsyth_addresses <- facilities %>%
  # remove any facilities that we do not have addresses for
  filter(!is.na(address)) %>%
  # select(id, address) %>%
  # each facility is in the dataset three times (one for each shift)
  # only keeping unique values will ensure each facility's addresses is only in dataset once
  unique() %>%
  # find lat / long og the address by calling the Google Maps API
  mutate_geocode(address)

write_csv(forsyth_addresses, 'data/forsyth_addresses_geocode.csv')

