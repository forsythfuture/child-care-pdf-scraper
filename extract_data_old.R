####################################################################################
#
# This script extracts data from the pdf files containing the old style
#
####################################################################################

library(tidyverse)
library(naniar)
library(pdftools)
library(qpdf)

source('scrapper_functions.R')

# vector of column names for the column in the number of children matrix
num_children_cols <- c('num_infant', 'num_one', 'num_two', 'num_three', 'num_four', 
                       'num_five','num_twelve', 'num_total', 'children_lic', 'children_max')

# each monthly file is at a different url, with a consistent style
# the string below represents the base URL, with month and year added at the end
url_base <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_"

# enter years that we want to extract data for
years <- seq(2012, 2006, -1)

# each PDF file is a different month,
# so we will iterate through months, extracting files
# month names must be lower case, because they are lower case in file names
months <- str_to_lower(month.name)

# iterate through years
for (year in years) {

  # iterate through each month, which is a different PDF file
  for (month in months) {
    
    # initialize dataframe to store single month results
    single_month <- data.frame()
  
    # create url to this month's PDF file
    url <- paste0(url_base, month, '_', year, '.pdf')
    
    # create a temporary file and download the monthly pdf into the temp file
    # this will save the file into memory, and will not write it on to the hard drive
    temp <- tempfile()
    download.file(url, temp)
    
    # create file name to save output
    # save to 'data' folder
    # save as RDS file because this file type has faster I/O operations
    output_file <- paste0('data/data_', year, '/nc_', month, '_', year, '.csv')
    
    # initialize dataframe to store data
    single_month <- data.frame()
    
    # get the number of pages in the PDF, so we know how many pages to iterate through
    num_pages <- pdf_length(url) - 9
    
    for (i in seq_len(num_pages)) {
      
      # print out update on progress
      print(year)
      print(month)
      print(i)
      
      # create temporary file to store the single page of the pdf file
      temp_page <- tempfile()
      pdf_subset(temp, pages = i, output = temp_page)
      
      # extract the text into list, but if it is a summary page, skip to the next page
      list_extracts <- ff_extract_old_format(temp_page)
      
      # skip if summary page
      if (list_extracts[[1]] == "summary page") next
      
      # convert list of extracts to columns in dataframe, and
      # combine with reset of the month
      single_month <- list_extracts[[6]] %>%
        # rename the number of children columns
        rename_at(vars(V1:V10), ~num_children_cols) %>%
        mutate(id = rep(list_extracts[[1]], each = 3),
               county = list_extracts[[9]],
               #name = rep(list_extracts[[2]], each = 3),
               type = list_extracts[[10]],
               star = rep(list_extracts[[3]], each = 3),
               #ind_month = rep(list_extracts[[4]], each = 3),
               shift = rep(c(1, 2, 3), 
                           times = length(list_extracts[[1]])),
               cat_op = rep(list_extracts[[8]], each = 3),
               scc = rep(list_extracts[[7]], each = 3),
               employees = rep(list_extracts[[5]], each = 3)
               ) %>%
        bind_rows(single_month, .)
        
      # unlink temp file for single page
      unlink(temp_page)
      
    }
    
    # write out data to 'data' folder after reordering columns
    single_month %>%
      mutate(month = month,
             year = year) %>%
      select(id:shift,num_infant:children_max, cat_op:employees, month, year) %>%
      write_csv(output_file)
    
    rm(single_month)
    
    gc()
    
    # unlink temp file for monthly file
    unlink(temp)
    
  }

}