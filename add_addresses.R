######################################################################
#
# This script builds a web-scrapper to automatically search for, and add,
# the addresses of all facilities. It performs the operations through the
# following steps:
# 
# Iterate through each facility ID and do the following:
#   1. navigate to http://ncchildcaresearch.dhhs.state.nc.us/search.asp;
#   2. enter the facility ID in the top search box;
#   3. press enter; and
#   4. scrape the address from the resulting page.
#
# The script uses RSelenium, which is based on the selenium web driver,
# to enter search terms into a text box. The selenium web server is started
# through a docker image. Installing docker and running the image is beyond
# the scope of this script. But, information cna be found here:
#  http://ropensci.github.io/RSelenium/articles/docker.html
#
#########################################################################

library(tidyverse)
library(RSelenium)

# enter the month and year that you want facility addresses for
# month should be in lower case
month <- 'january'
year <- 2019

# create url to the month / year dataset hosted on github
base_url <- "https://raw.githubusercontent.com/forsythfuture/child-care-pdf-scraper/master/data/nc_"
data_url <- str_c(base_url, month, "_", year, ".csv")

# import data
facilities <- read_csv(data_url) %>%
  # only keep ID column
  select(id) %>%
  distinct()

# initialize dataframe to contain addresses
all_addresses <- data.frame()

# the selenium docker image must be started prior to running this script
# for more information, see http://ropensci.github.io/RSelenium/articles/docker.html

# establish connect to selenium web server
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

# open server
remDr$open()

# iterate though each facility ID, and find address
for (i in seq_len(nrow(facilities))) {

  print(i)
  
  # save the facility ID as an object so it can easily be used later
  facility_id <- facilities$id[i]

  # navigate to the page that allows users to search tfor child care facilities
  # and find their addresses
  remDr$navigate("http://ncchildcaresearch.dhhs.state.nc.us/search.asp")
  
  # we now need to get the name of the search box on this page
  # this can be done manually in Chrome
  # go to the page, right click, and click inspect;
  # then highlight the search box, go to the source code panel on right, find the section
  # that contains the search bar's source code, view all the code and find it's name
  # as of April 2019, the name was 'Sel_ID'
  # save the search bar element using its name
  webElem <- remDr$findElement(using = "name", value = "Sel_ID")
  
  # search for facility in try block so that if no facility is returned,
  # program does not crash
  try({
  
    # type the facility ID into the serach bar and push enter
    # this takes us to the page with the address of the facility whose ID was entered
    webElem$sendKeysToElement(list(facility_id, key = "enter"))
    
    # extract the address from the page
    address <- XML::readHTMLTable(remDr$getPageSource()[[1]])[[3]][2,2] %>%
      as.character() %>%
      # clean up by removing line breaks
      str_replace_all("[\n]" , "") %>%
      # separate street address, city, state, and zip using regular expressions
      # the existing format is street, city, state, zip, and phone number
      str_match("^(.+)  (.+), NC ([0-9]+)") %>%
      # trim whitespace from left and right of all entries
      str_trim("both")
    
    # create one line dataframe with the address of one facility
    # convert street and city from all uppercase to title case
    one_address <- data.frame(id = facility_id,
                              street = str_to_title(address[2], locale = "en"),
                              city = str_to_title(address[3], locale = "en"),
                              zip = address[4])
    
    # add the data frame with one address to the dataframe with all addresses
    all_addresses <- bind_rows(all_addresses, one_address)
  
  })
    
}

write_csv(all_addresses, 'data/addresses/nc_child_care_addresses.csv')
