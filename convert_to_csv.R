##############################################################
#
# This script converts the rds files to csv for permanent storage
# The files were originally stored as RDS files during scrapping
# because file I/O operations are faster with RDS files.
#
###############################################################

library(tidyverse)

# get a list of all rds files
rds_files <- list.files(path = 'data', pattern = '.rds', full.names = T)

# create csv file names by replacing '.rds' with '.csv'
# and adding the data folder to the file paths
csv_files <- str_replace_all(rds_files, 'rds', 'csv')

# create function that imports RDS file and exports CSV file
# we will then iterate through all RDS files, using this function
convert_csv <- function(rds_file_name, csv_file_name) {

  read_rds(rds_file_name) %>%
    write_csv(., csv_file_name)

}

# iterate through each rds file, and save output as csv file
pwalk(list(rds_files, csv_files), convert_csv)
