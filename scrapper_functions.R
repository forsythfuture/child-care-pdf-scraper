##################################################################
#
# This script contains a function that are used to scrape the pdfs
#
##################################################################

# this function takes the extracted text from a single page of the PDF and
# wrangles it into a dataframe
ff_create_df <- function(table_text) {
  
  # table_text: the output from the extract_tables calls
  
  # convert the extract that contains the bulk of the data into a dataframe
  # this extract contains all columns and rows of the pdf
  df <- as.data.frame(table_text[[1]], stringsAsFactors = F)
  
  # for some of the pages, the 'Category Operation' and 'Operation Site' columns
  # merge into one column when extracted
  # therefore, we will merge the columns for all pages, so there is consistency
  # this means, however, that we will only merge for the pages where the columns did not merge
  # if the extracted page has 17 columns then it did not merge,
  # so only perform merge if page had 17 columns
  if (ncol == 17) {
    
    df <- df %>%
      unite(col = 'cat_op', V15, V16, remove = T, sep = " ")
    
  }
  
  # vector of column names for the datafram
  matrix_col_names <- c('id_abb', 'name', 'ind_month', 'shift',
                        'num_infant', 'num_one', 'num_two', 'num_three', 'num_four',
                        'num_five', 'num_twelve', 'num_total', 
                        'children_lic', 'children_max', 'cat_op', 'scc')
  
  colnames(df) <- matrix_col_names
  
  # the ID / Abbrev column contains the IS and star rating on alteranting rows
  # extract the star rating and place it in its own column
  
  # make column for star rating
  df$star <- NA
  
  # star rating is on every third row, starting at row 2,
  # so extract these rows and place in the star column
  # to ensure the first row for each facility is complete, place
  # the star rating on every third row, starting at the first row
  df$star[seq(1, nrow(df), 3)] <- df$id_abb[seq(2, nrow(df), 3)]
  
  # convert to NA the rows that contained the original star ratings in the ID / Abb column
  df$id_abb[seq(2, nrow(df), 3)] <- NA
  
  # except for the number of students and shitf columns, all columns should only have text
  # in the first row for teh school; text in other rows will mess up the process of filling NA
  # values with the previous value
  # therefore, ensure all non-number of kids rows only have text in first row of school;
  # in other words, do not have text in second and third rows
  
  # create vectors for second and third rows, we will convert all values in these rows to NA
  second_row <- seq(2, nrow(df), 3)
  third_row <- seq(3, nrow(df), 3)
  
  # column numbers of columns we will make NA for second and third rows
  na_cols <- c(1, 2, 3, 15, 16, 17)
  
  # make columns and rows NA
  df[second_row, na_cols] <- NA
  df[third_row, na_cols] <- NA
  
  # clean up data frame
  df <- df %>%
    # rename id column since it now only contains the ID
    rename(id = id_abb) %>%
    # forward fill NA values with most recent value
    fill(everything()) %>%
    # remove parenthesis '(' and ')' from shift column
    mutate(shift = str_replace_all(.$shift, "[()]", "")) %>%
    # separate employees and category columns
    mutate(employees = str_extract(.$cat_op, '^[0-9]* '),
           # remove number from category column
           cat_op = str_replace(.$cat_op, '^[0-9]* ', "")) %>%
    # convert numeric columns to integers
    mutate_at(vars(matches('num|children'), 'id', 'ind_month', 'shift', 'employees'), 
              as.integer) %>%
    # add county and type, which are located in different extract areas
    # county has a leading number, so remove it
    mutate(county = str_extract(table_text[[2]][,1], '[a-zA-Z ]*$'),
           type = table_text[[3]][,1],
           # convert facility name from upper case to title case
           name = str_to_title(.$name, locale = "en")) %>%
    # trim padded whitespace from all columns
    mutate_all(funs(str_trim(., side = 'both'))) %>%
    # reorder columns
    select(id, county, name, type, star, everything())
  
  return(df) 
}