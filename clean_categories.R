###############################################################################
#
# In the scraper, the `Category Operation` and `Operation Site` columns were
# merged.  This files cleans the merged column.
#
###############################################################################

library(tidyverse)

# create a function that outputs a vector of unique categories for the monthly dataset
# we will use this function to iterate through each monthly dataset to extract unique categories
unique_categories <- function(file) {

  read_csv(file) %>%
    select(cat_op) %>%
    unique() %>%
    .[[1]]

}

# create vector of file names for all 2019 monthly files
file_paths <- list.files('data/data_2019', full.names = TRUE)

# create vectors of unique categories for each monthly datasets
monthly_categories <- lapply(file_paths, unique_categories) %>%
  # each month will be a different element in the list
  # and the element will be a vector of that month's unique categories
  # combine all monthly elements into one vector
  unlist() %>%
  # take the unqiue values of this combined vector of categories
  unique()

# look at unique monthly categories and separate into category and site
# make into function so it can be used on all months
separate_categories <- function(file_name){

  read_csv(file_name) %>%
    # create category column, and fill it with recoded values from cat_op
    mutate(category = ifelse(str_detect(cat_op, "^Independent"), "Independent",
                      ifelse(str_detect(cat_op, "^Head Start"), "Head Start", 
                      ifelse(str_detect(cat_op, "^Religious"), "Religious Sponsored",
                      ifelse(str_detect(cat_op, "^Franchise"), "Franchise",
                      ifelse(str_detect(cat_op, "^Local Public"), "Local Public School",
                      ifelse(str_detect(cat_op, "^Park"), "Park and Recreation",
                      ifelse(str_detect(cat_op, "^Community College"), "Community College",
                      ifelse(str_detect(cat_op, "^Employer"), "Employer Sponsored",
                      ifelse(str_detect(cat_op, "^Community Service"), "Community Service Agency",
                      ifelse(str_detect(cat_op, "^Mental"), "Mental Health",
                      ifelse(str_detect(cat_op, "^Private School"), "Private School",
                      ifelse(str_detect(cat_op, "^Contracted"), "Contracted Provider",
                      ifelse(str_detect(cat_op, "^Other"), "Other",
                      ifelse(str_detect(cat_op, "^College"), "College",
                      ifelse(str_detect(cat_op, "^Federal"), "Federal",
                      ifelse(str_detect(cat_op, "^Parent Co"), "Parent Group", NA
                      )))))))))))))))),
           # create site column and fill with recoded values from cat_op
           site = ifelse(str_detect(cat_op, "Residence$"), "Family Residence",
                  ifelse(str_detect(cat_op, "Modular$"), "Modular",
                  ifelse(str_detect(cat_op, "Converted Building$"), "Converted Building",
                  ifelse(str_detect(cat_op, "for Child Care$"), "Constructed for Child Care",
                  ifelse(str_detect(cat_op, "Religious$"), "Religious",
                  ifelse(str_detect(cat_op, "School$"), "School",
                  ifelse(str_detect(cat_op, "Government Building$"), "Government Building",
                  ifelse(str_detect(cat_op, "Community Building$"), "Community Building",
                  ifelse(str_detect(cat_op, "Converted House$"), "Converted House",
                  ifelse(str_detect(cat_op, "Doublewide$"), "Single or Doublewide",
                  ifelse(str_detect(cat_op, "for Child$"), "Constructed for Child Care",
                  ifelse(str_detect(cat_op, "Care Facility$"), "Health Care Facility",
                  ifelse(str_detect(cat_op, "Business$"), "Industry or Business",
                  ifelse(str_detect(cat_op, "Singlewide$"), "Single or Doublewide",
                  ifelse(str_detect(cat_op, "Other$"), "Other",
                  ifelse(str_detect(cat_op, "Apartment$"), "Apartment", NA
                  )))))))))))))))))
  
}

# read in 2019 datasets, create category and site columns
updated_dfs <- map(file_paths, separate_categories)
  
# save out updated 2019 datasets
pwalk(list(updated_dfs, file_paths), write_csv)
