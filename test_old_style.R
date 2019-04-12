library(tidyverse)
library(tabulizer)
library(pdftools)
library(qpdf)

file_path <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_april_2017.pdf"

temp <- tempfile()
download.file(file_path, temp)

locate_areas(file_path, pages = 1)

coords <- list(c(95, 10, 700, 88), # stars
               c(95, 88, 700, 887), # middle: name to category
               c(95, 887, 700, 1095)) # operation site and scc

full <- extract_tables(temp, pages = rep(14, length(coords)), 
                       area = coords, guess = F, method = 'lattice')

full

pdf_text('test_page.pdf')

