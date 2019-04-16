library(tidyverse)
library(tabulizer)
library(pdftools)
library(qpdf)

file_path <- "https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/S/statistical_detail_report_april_2017.pdf"
pdf_length(file_path)

temp <- tempfile()
download.file(file_path, temp)

temp_page <- tempfile()
pdf_subset(temp, pages = 14, output = temp_page)
text <- pdf_text(temp_page)

a <- extract_old_format(text)

extract_old_format <- function(pdf) {
  
  # convert pdf to text
  text <- pdf_text(pdf)

  # check to see if page contains individual facility information or summary county information
  # we only want to keep individual facility information
  # This regular expression looks for the line "CHILDREN BY AGES/ ANALYSIS CATEGORY"
  # that only shows up in the summary county information pages
  # it will be true if the page is a summary page
  summary_page <- str_detect(text, "\nCHILDREN BY AGES[/] ANALYSIS OF CATEGORY")
  
  # if the page is a summary exit the function and return the value true 
  if (summary_page == TRUE) return(summary_page)
  
  # id and name
  id_name <- str_match_all(text, "\n([0-9]{4,}) ([A-Z| ]*)")
  
  # separate ID and name
  id <- id_name[[1]][,2] # id
  name <- str_trim(id_name[[1]][,3]) # name: trim whitespace (name will only show first line)
  
  # star
  star <- str_extract_all(text, "(?<=\n)[0-9]-STAR|GS[0-9]+|SPPROV|PROBLIC|PROB|LIC SDC|TEMP C|TEMP FCCH")[[1]]
  
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
    as_tibble() %>%
    # make all teh values integers
    mutate_all(funs(as.integer))
  
  # SCC
  scc <- str_extract_all(text, "  [YN](?=\n)")[[1]]
  scc <- str_trim(scc)
  
  # category oper. and oper. site
  # both columns will be combined into one and cleaned later
  oper <- str_match_all(text, '(\\(1\\)).*[0-9] +([A-Za-z /]+)\n')[[1]][,3]
  oper <- str_replace_all(oper, " +[YN]", "")
  
  # in the vector, the even numbers are the top lien of the PDF, while the odd numbers
  # are the bottom lines
  # add both to dataframe, with even and odd numbers as different rows
  oper <- data.frame(even = oper[seq.int(1, length(oper), by = 2)],
                     odd = oper[seq.int(2, length(oper), by = 2)]) %>%
    # combine both columns into one
    unite(oper, even, odd, sep = " ", remove = TRUE) %>%
    .[[1]]
  
  # county
  # this is at the bottom left of the header
  county <- str_match_all(text, "\nCOUNTY: +[0-9]+ +(.*)CHILD")[[1]][,2]
  # trim whitespace
  county <- str_trim(county)
  
  # homes or centers (type)
  # this is indicated by text stating either "HOMES" or "CENTERS" at the bottom of the header
  type <- str_match_all(text, "\n +(HOMES|CENTERS)\n")[[1]][,2]
  
  extracts <- list(id, name, star, ind_month, emp, number_children, scc, oper, county, type)
  
  return(extracts)
  
}

list_extracts <- extract_old_format('test_page.pdf')

# vector of column names for the column in the number of children matrix
num_children_cols <- c('num_infant', 'num_one', 'num_two', 'num_three', 'num_four', 
                       'num_five','num_twelve', 'num_total', 'children_lic', 'children_max')

# id,county,name,type,star,ind_month,shift,num_infant,num_one,num_two,num_three,num_four,num_five,num_twelve,num_total,
# children_lic,children_max,cat_op,scc,employees,month,year
# the dataframe that has the number of children will serve as the base dataframe 
# that we will attached the other columns to
# it will serve as the base dataframe because it has the max number of rows for the final dataframe
list_extracts[[6]] %>%
  # rename the number of children columns
  rename_at(vars(V1:V10), ~num_children_cols) %>%
  mutate(id = rep(list_extracts[[1]], each = 3),
         county = list_extracts[[9]],
         name = rep(list_extracts[[2]], each = 3),
         type = list_extracts[[10]],
         star = rep(list_extracts[[3]], each = 3),
         ind_month = rep(list_extracts[[4]], each = 3),
         shift = rep(c(1, 2, 3), 
                     times = length(list_extracts[[1]])),
         cat_op = rep(list_extracts[[8]], each = 3),
         scc = rep(list_extracts[[7]], each = 3),
         employees = rep(list_extracts[[5]], each = 3))

rep(list_extracts[[1]], each = 3)
