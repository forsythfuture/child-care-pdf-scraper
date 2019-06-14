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
library(aws.s3)

# create list of all monthly data file names
file_paths <- list.files('data', 
                         pattern = "[0-9]{4}[.]csv", # don't keep address files
                         recursive = T, full.names = T)

# import all bind together all months and years
updates <- map_df(file_paths, read_csv) %>%
  # counties are title case in some years, and all upper case in others
  # make entire column title case
  mutate(county = str_to_title(county),
         star = str_to_title(star),
         name = str_to_title(name),
         # some years the star column is worded differently,
         # replace the different wording with the common wording
         star = str_replace_all(star, '-Star.*', " Star"),
         # remove center or house designation from star, 
         # since this is captured in a different column
         star = str_replace_all(star, "Star.*", "Star"),
         # various other clean ups to star
         star = str_replace_all(star, "^Gs.*", "GS"),
         star = str_replace_all(star, "^Prov.*", "Prov"),
         star = str_replace_all(star, "^Prob.*", "Prob"),
         star = str_replace_all(star, "^Sprov.*", "Prov"),
         star = str_replace_all(star, "^Spprov.*", "Prov"),
         star = str_replace_all(star, "^Temp.*", "Temp"),
         star = str_replace_all(star, "^Pgs.*", "GS"),
         star = str_replace_all(star, ".*Sdc.*", "SDC"))

# import master file from AWS
master <- read_csv("https://nc-prek.s3.amazonaws.com/nc_prek_all.csv.gz",
                   col_types = cols(name = "c", ind_month = "d",
                                    category = "c", site = "c"))

# bind updated monthly datasets to master
master <- bind_rows(updates, master) %>%
  arrange(desc(year), month)

# write out csv files
write_csv(master, 'data/nc_prek_all.csv')

# zip csv file
gzip('data/nc_prek_all.csv')

# send to s3
put_object(file = 'data/nc_prek_all.csv.gz', acl = "public-read",
           object = "nc_prek_all.csv.gz", bucket = "nc-prek")

file.remove("data/nc_prek_all.csv.gz")