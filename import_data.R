########################################################################
#
# This script contains functions that wrangle each block into a dataframe
#
##########################################################################

library(tidyverse)
library(tabulizer)

#url <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_january_2019.pdf"

#locate_areas(url, 1)

source('scrapper_functions.R')

# enter year that we want to extract data for
year <- 2019

# each PDF file is a different month,
# so we will iterate through months, extracting files
# month names must be lower case, because they are lower case in file names
#months <- str_to_lower(month.name)[9:12]

months <- 'january'

# each monthly file is at a different url, with a consistent style
# the string below represents the base URL, with month and year added at the end
url_base <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_"

# coordinates for new format
coords <- list(c(106.92, 5.18, 546.07, 797.49), # table with data
               c(42.69, 71.78, 57.87, 303.17), # county
               c(45.02, 355.76, 59.04, 477.30), # type
               # test for whether it is a detail page we want to extract
               # this block will equal 'Child Care Analysis Detail' if it is
               c(31.67, 290.82, 46.28, 532.50)) 
               #c(579.94, 1.66, 597.46, 40.22), # test for summary 1
               #c(564.73, 2.36, 580.32,34.52)) # test for summary 2

# iterate through each month, which is a different PDF file
for (month in months) {

  # create url to this month's PDF file
  url <- paste0(url_base, month, '_', year, '.pdf')
  
  # create file name to save output
  # save to 'data' folder
  # save as RDS file because this file type has faster I/O operations
  output_file <- paste0('data/nc_', month, '_', year, '.rds')
  
  # initialize dataframe to store data
  single_month <- data.frame()
  
  # get the number of pages in the PDF, so we know how many pages to iterate through
  num_pages <- get_n_pages(url)
  
  for (i in seq_len(num_pages)) {
    
    # print out update on progress
    print(year)
    print(month)
    print(i)
    
    table_text <- extract_tables(url, pages = rep(i, length(coords)), 
                                 area = coords, guess = F,
                                 method = 'lattice')
    
    # skip pages that don't have the details per facility
    if (table_text[[4]][,1] != "Child Care Analysis Detail") next
    
    single_month <- ff_create_df(table_text) %>%
      bind_rows(single_month, .)
    
  }
  
  # add year and month
  single_month <- single_month %>%
    mutate(month = str_to_title(month),
           year = year)
  
  # write out data to 'data' folder
  write_rds(single_month, output_file)
  
  rm(single_month)
  
  gc()
  
}