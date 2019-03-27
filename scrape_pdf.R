library(tabulizer)
library(tidyverse)

url <- 'https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_january_2019.pdf'

all_names <- list(c(111.5828, 328.8344, 139.8439, 587.0828))

extract_tables(url, pages = 2, area = all_names, guess = F)

locate_areas(url, pages = 1)

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

cols <- list(c(7.24, 71.56, 261.59, 310.31, 592, 663.59, 771.26))

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

table <- extract_tables(url, pages = rep(1,length(coord_blocks)), 
                        area = coord_blocks, guess = F,
                        method = 'lattice')
