########################################################################
#
# This script contains functions that wrangle each block into a dataframe
#
##########################################################################

library(tidyverse)
library(tabulizer)

source('scrapper_functions.R')

url <- 'https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_january_2019.pdf'

coords <- list(c(106.92, 5.18, 546.07, 797.49), # table with data
               c(42.69, 71.78, 57.87, 303.17), # county
               c(45.02, 355.76, 59.04, 477.30), # type
               c(579.94, 1.66, 597.46, 40.22)) # test for summary

single_month <- data.frame()

for (i in seq_len(10)) {

  print(i)
  table_text <- extract_tables(url, pages = rep(1, length(coords)), 
                               area = coords, guess = F,
                               method = 'lattice')
  
  # skip county summary pages, which will have 'ALL' under 4
  if (table_text[[4]][,1] == "ALL") next
  
  single_month <- ff_create_df(table_text) %>%
    bind_rows(single_month, .)
  
}



df <- select(df, id, county, name, type, star, everything())
