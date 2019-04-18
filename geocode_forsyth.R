##################################################################
#
# This script creates a data set of Forsyth County
# child care facilites that are geo coded by adding the lat / long
# to the address.  It feeds the address to the Google Maps API
# to retrieve the lat / long
#
# Note: Users will need to set up a Google API key!!
#
####################################################################

library(tidyverse)
library(ggmap)

# create objects to store folder url to data; this will make for easier to read code
address_url <- 'https://raw.githubusercontent.com/forsythfuture/child-care-pdf-scraper/master/data/'

# import facility data for whole state and most recent month
facilities <- read_csv(str_c(address_url, 'data_2019/nc_march_2019.csv')) %>%
  # bind with address data
  left_join(read_csv(str_c(address_url, 'addresses/nc_child_care_addresses.csv')), by = 'id') %>%
  # create a single column with address information;
  # this is needed to feed addresses to the Google API to get lat and long
  mutate(address = str_c(street, ', ', city, ', NC ', zip))

# we want to limit the number of addresses we feed to the Google API,
# so create a dataset of only Forsyth addresses to feed to the API
forsyth_addresses <- facilities %>%
  # only keep forsyth county addresses
  filter(county == 'Forsyth' ,
         # remove any facilities that we do not have addresses for
         !is.na(address)) %>%
  select(id, address) %>%
  # each facility is in the dataset three times (one for each shift)
  # only keeping unique values will ensure each facility's addresses is only in dataset once
  unique() %>%
  # find lat / long og the address by calling the Google Maps API
  mutate_geocode(address)

write_csv(forsyth_address, 'forsyth_addresses_geocode.csv')
