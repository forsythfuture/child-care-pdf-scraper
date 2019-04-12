library(tidyverse)
library(tabulizer)
library(pdftools)
library(qpdf)

file_path <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_april_2017.pdf"

temp <- tempfile()
download.file(file_path, temp)

locate_areas(file_path, pages = 1)

coords <- list(c(125.63363, 24.31169, 678.16460, 70.55844), # stars
               c(125.63363, 67.55844, 688.67788, 493.01299), # middle: name to category
               c(125.6336, 493.0130, 680.5009, 584.1818)) # operation site and scc

full <- extract_tables(file_path, pages = rep(1, length(coords)), 
                       area = coords, guess = F, method = 'lattice')

pdf_text('test_page.pdf')

pdf_subset(temp, pages = 1, output = 'test_page.pdf')
