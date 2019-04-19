##########################################################################
#
# This script takes all the datasets, merges them into one dataset,
# saves the object as an zipped csv file.
#
# The zip file will then be saved in an Amazon s3 bucket.
#
########################################################################

library(tidyverse)
library(R.utils)

# create list of all monthly data file names
file_paths <- list.files('data', 
                         pattern = "[0-9]{4}[.]csv", # don't keep address files
                         recursive = T, full.names = T)

# import all bind together all months and years
all_years <- map(file_paths, read_csv) %>%
  bind_rows()

# write out csv files
write_csv(all_years, 'data/nc_prek_all.csv')

# zip csv file
gzip('data/nc_prek_all.csv')
