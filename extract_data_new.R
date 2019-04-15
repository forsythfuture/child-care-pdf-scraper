####################################################################################
#
# This script extracts data from the pdf files containing the most recent style
#
####################################################################################

library(tidyverse)
library(tabulizer)

source('scrapper_functions.R')

# enter year that we want to extract data for
year <- 2018

# each PDF file is a different month,
# so we will iterate through months, extracting files
# month names must be lower case, because they are lower case in file names
months <- str_to_lower(month.name)

# each monthly file is at a different url, with a consistent style
# the string below represents the base URL, with month and year added at the end
url_base <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_"

# coordinates to extract
coords <- list(c(103.399115, 0, 550.213274, 796.402597), # data table
               c(0, 245.45809,  64.58496, 604.02361), # header
               c(0, 0, 74.514159, 209.330579)) # county

# initialize empty vector that will hold errors
errors <- vector()

# iterate through each month, which is a different PDF file
for (month in months) {

  # create url to this month's PDF file
  url <- paste0(url_base, month, '_', year, '.pdf')
  
  # create a temporary file and download the monthly pdf into the temp file
  # this will save the file into memory, and will not write it on to the hard drive
  temp <- tempfile()
  download.file(url,temp)
  
  # create file name to save output
  # save to 'data' folder
  # save as RDS file because this file type has faster I/O operations
  output_file <- paste0('data/data_', year, '/nc_', month, '_', year, '.csv')
  
  # initialize dataframe to store data
  single_month <- data.frame()
  
  # get the number of pages in the PDF, so we know how many pages to iterate through
  num_pages <- get_n_pages(url)
  
  for (i in seq_len(num_pages)) {
    
    # print out update on progress
    print(year)
    print(month)
    print(i)
    
    # extract the text blocks from the PDF file, whiich is stored in memory as a temp file
    table_text <- extract_tables(temp, pages = rep(i, length(coords)), 
                                 area = coords, guess = F,
                                 method = 'lattice')
    
    # identify whether page has detailed facility information by searching for
    # whether the header information includes the phrase "Child Care Analysis Detail"
    # if this phrase is not included, the page does not contain detailed facility info,
    # so, skip this page
    if (!(any(str_detect(table_text[[2]], 'Child Care Analysis Detail')))) next
    
    # create dataset from extract
    # use try so that loop continues if error
    single_page <- try(ff_create_df(table_text))
    
    # if there is an error, save the page number
    if(inherits(single_page, "try-error")) {
      errors <- c(errors, paste(i, month, year, sep = ", "))
      next
    }

    # bind single page with month    
    single_month <- bind_rows(single_month, single_page)
    
  }
  
  # add year and month
  single_month <- single_month %>%
    mutate(month = str_to_title(month),
           year = year)
  
  # write out data to 'data' folder
  write_csv(single_month, output_file)
  
  # remove temp file
  unlink(temp)
  
  rm(single_month)
  
  gc()
  
}
