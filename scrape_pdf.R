library(tabulizer)
library(tidyverse)

url <- 'https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_january_2019.pdf'

locate_areas(url, pages = 3)

# coordinates to extract --------------------

# create vectors of the top and bottom pixel of each row
# these will be repeatedly used
top_row <- c(111.51, 145.56, 179.21, 218.02, 255.53, 290.47,
             326.70, 364.22, 399.16, 436.68, 474.20, 510.43)

# now that we have the top and bottom boundary for each row,
# we just need the right and left boundaries for each row and cell
lr_id <- c(7.24, 56.71)
lr_name <- c(71.56, 260.52)
lr_month <- c(261.59, 290.828)
lr_children <- c(310.31, 588.05)
lr_operation <- c(592, 677.7134)
lr_site <- c(663.5962, 773.5751)
lr_scc <- c(771.2675, 794.6561)

# create a single lsit of all left right coordinates to iterate through
lr_coords <- list(lr_id, lr_name, lr_month, lr_children, lr_operation, lr_site, lr_scc)

# knit the top / bottom and left / right together to create coordinates for each block --------
# order: top, left, bottom, right

# we need to do this for each block, so we will create a function
extract_block <- function(lr, top_row) {
  
  # lr: the left and right coordinates of the column blocks to extract
  #     should be vector of two numbers: c(6, 45)
  # top_row: the coordinate to the top of each row
  
  # number of rows in the pdf, we will iterate through each row seperately
  num_rows <- 12
  
  # height of each row
  row_height <- 28
  
  # initialize list to store coordinates
  coords <- list()
  
  # iterate through rows, extracting coordinates of each row
  for (i in seq_len(num_rows)) {
    
    top <- top_row[i]
    bottom <- top + row_height
    left <- lr[1]
    right <- lr[2]
    
    coords[[i]] <- c(top, left, bottom, right)
    
  }
  
  return(coords)

}

# iterate thorugh each left / right coordinate, creating all coordinates
# each major list item represents a left / right coordinate (column),
# while each nested list represents a row
coord_blocks <- map(lr_coords, extract_block, top_row = top_row) %>%
  # flatten list so each element is a different block
  flatten()

# pull isolated blocks: county, date, type (center / home)
county <- c(43.32598,  71.90792,  57.32867, 283.65267)
date <- c(30.97066, 686.54443,  46.62073, 728.56381)
type <- c(44.97335, 347.91761,  58.97604, 473.97577)

# the reports contain county summary pages after each county's listing of individual homes
# create an extract that extracts a portion of the summary page, to check and see if the 
# page is a summary page; if it is, we will skip pulling from the page
# this extract will contain the word 'ALL' if the page is a summary page
summary_page <- c(581.194078,   4.347334, 595.196770,  28.240711)

# append isolated blocks to the end of the coordinates list
coord_blocks <- list.append(coord_blocks, county, date, type, summary_page)

c()

table <- extract_tables(url, pages = rep(seq(1, 900), each = length(coord_blocks)), 
                        area = rep(coord_blocks, 900), guess = F,
                        method = 'lattice')
