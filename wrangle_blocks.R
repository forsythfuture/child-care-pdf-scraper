########################################################################
#
# This script contains functions that wrangle each block into a dataframe
#
##########################################################################

library(tidyverse)
library(tabulizer)

# create coordinates by running function
source('create_coordinates.R')
coords <- extract_coordinates()

url <- 'https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_january_2019.pdf'

table <- extract_tables(url, pages = rep(1, length(coords)), 
                        area = coords, guess = F,
                        method = 'lattice')

table[[1]]

single_page <- tibble()

for (i in seq(0, 11)) {
  
  single_page <- tibble(id = table[[1+i]][1],
                       stars = table[[1+i]][2],
                       name = table[[13+i]][1],
                       ind_month = table[[25+i]][1],
                       emp_category = table[[49+i]][1],
                       operation_site = table[[61+i]][1],
                       ssc = table[[73+i]][1],
                       shift = seq(1, dim(table[[41+i]])[1]),
                       num_infant = table[[41+i]][,1],
                       num_one = table[[41+i]][,2],
                       num_two = table[[41+i]][,3],
                       num_three = table[[41+i]][,4],
                       num_four = table[[41+i]][,5],
                       num_five = table[[41+i]][,6],
                       num_twelve = table[[41+i]][,7],
                       children_lic = table[[41+i]][,9],
                       children_max = table[[41+i]][,10]) %>%
    bind_rows(single_page, .)
  
  print(i)
}
