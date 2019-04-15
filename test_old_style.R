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

pdf_subset(temp, pages = 7, output = "page7.pdf")
text <- pdf_text('test_page.pdf')

# id and name
id_name <- str_match_all(text, "\n([0-9]{4,}) ([A-Z| ]*)")

# separate ID and name
id <- id_name[[1]][,2] # id
name <- str_trim(id_name[[1]][,3]) # name: trim whitespace (name will only show first line)

# star
star_name <- str_extract_all(text, "(?<=\n)[0-9]-STAR|GS[0-9]+|SPPROV")

# ind. month and number of employees
ind_month_emp <- str_match_all(text, "([0-9]{1,2}) +\\([0-9]\\) + [0-9 ]*([0-9]+) +[A-Z]")
ind_month <- ind_month_emp[[1]][,2]
emp <- ind_month_emp[[1]][,3]

# number of children
number_children <- str_extract_all(text, "(?<=\\([0-9]\\)) +([0-9]+ +){9}[0-9]+") %>%
  .[[1]] %>%
  str_trim() %>%
  # each age group column in the pdf is separated by a space in the R object;
  # separate each age group column into a difference column in the R object
  str_split_fixed(" +", n = 10) %>%
  # convert to data frame
  as.data.frame() %>%
  # make all teh values integers
  mutate_all(funs(as.integer))

# SCC
scc <- str_extract_all(text, "[YN](?=\n)")

# category oper. and oper. site
# both columns will be combined into one and cleaned later
oper <- str_match_all(text, '(\\([0-9]\\)).*[0-9] +([A-Za-z ]+)\n')[[1]][,3]
oper <- str_replace_all(oper, " +[YN]", "")

# in the vector, the even numbers are the top lien of the PDF, while the odd numbers
# are the bottom lines
# add both to dataframe, with even and odd numbers as different rows
oper <- data.frame(even = oper[seq.int(1, length(oper), by = 2)],
                   odd = oper[seq.int(2, length(oper), by = 2)]) %>%
  # combine both columns into one
  unite(oper, even, odd, sep = " ", remove = TRUE)
