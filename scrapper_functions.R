##################################################################
#
# This script contains a function that are used to scrape the pdfs
#
##################################################################

# this function takes the extracted text from a single page of the PDF and
# wrangles it into a dataframe
ff_create_df <- function(table_text) {
  
  # table_text: the output from the extract_tables calls
  
  # the table with data is situated differently vertically on different pages
  # some pages include the headers because they are further down vertically;
  # The header rows start with 'ID/' or 'Abbrev'
  # remove any row that has one of these words in the first column
  
  # identify rows that contain 'ID/' or 'Abbrev'
  remove_rows <- which(table_text[[1]][,1] %in% c("ID/", "Abbrev"))
  
  # remove these rows only if there are rows to remove
  if (length(remove_rows > 0)) {
    table_text[[1]] <- table_text[[1]][-remove_rows,]
  }
  
  # create data table ------------
  
  # convert the extract that contains the bulk of the data into a dataframe
  # this extract contains all columns and rows of the pdf
  df <- as.data.frame(table_text[[1]], stringsAsFactors = F)
  
  # for some extracts, the 'Ind Month' and 'Shift' columns are merged;
  # check to see if these two column are merged, and separate if needed
  
  # items in 'shift' column have parenthesis around them, so check and see
  # if there are any parenthesis in the column that should only contain Ind Month,
  # but might also contain shift and parenthesis
  if (str_detect(df$V3[1], "[(]")) {
    
    df <- df %>%
      # separate column at parenthesis "("
      separate(V3, into = c('ind_month', 'shift'), sep = "[(]", remove = T) %>%
      # add open parenthesis back to column, so there is consistency with other pages
      mutate(shift = paste0("(", shift))
    
  }
  
  # some columns extract with nothing in them; all values are ""
  # delete these columns

  # create empty vector that will store column numbers of empty columns
  vec <- vector()

  # iterate through each column, and if it is empty store the number in the vector
  for (col_num in seq(1, ncol(df))) {
   vec <- if (all(df[[col_num]] == "")) c(vec, col_num) else vec
  }
  
  # remove empty columns only if there are columns to remove
  if (length(vec) > 0) {
    df <- df[-vec]
  }
  
  # some columns merge the MAX, num employees, and category operations columns;
  # check to see if these columns are merged, and separate MAX and num employees.
  # we will separate num employees and category operations later
  # if the columns are merged,it will take the form number, number, word
  # so, we will search for this structure and separate if it is detected
  if (str_detect(df[[1, 14]], "[0-9]+ [0-9]+ [A-Z]")) {
    
    df <- df %>%
      separate(14, into = c('children_max', 'cat'), sep = " ", remove = T, extra = "merge")
    
  }
  
  # for some of the pages, the 'Category Operation' and 'Operation Site' columns
  # merge into one column when extracted
  # therefore, we will merge the columns for all pages, so there is consistency
  # this means, however, that we will only merge for the pages where the columns did not merge
  # if the extracted page has 17 columns then it did not merge,
  # so only perform merge if page had 17 columns
  if (ncol(df) == 17) {
    
    df <- df %>%
      unite(col = 'cat_op', 15, 16, remove = T, sep = " ")
    
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
  # in the first row for the school; text in other rows will mess up the process of filling NA
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
  
  # identify type and county ----------
  
  # identify whether the type is home or center
  # do this by first converting column header to a single string
  # and then extracting either 'Homes' or 'Center' from string, whichever occurs
  type <- str_c(table_text[[2]], collapse = ' ') %>%
    str_extract(., 'Home|Center')
  
  # extract county
  # convert the column that contains the county information to a string
  county <- str_c(table_text[[3]], collapse = " ") %>%
    # use a regular expression to exgract teh county from the string
    str_match(., 'County:(?: [0-9]+ | )([A-Z][a-z]+)') %>%
    .[2]
  
  # clean up data frame -----------
  
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
    # add county and type, which are located in different extract areas
    # county has a leading number, so remove it
    mutate(county = county,
           type = type,
           # convert facility name from upper case to title case
           name = str_to_title(.$name, locale = "en")) %>%
    # trim padded whitespace from all columns
    mutate_all(funs(str_trim(., side = 'both'))) %>%
    # convert numeric columns to integers
    mutate_at(vars(matches('num|children'), 'ind_month', 'shift', 'employees'), 
              as.integer) %>%
    # reorder columns
    select(id, county, name, type, star, everything())
  
  return(df) 
}
