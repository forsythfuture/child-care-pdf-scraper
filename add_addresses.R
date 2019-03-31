######################################################################
#
# This script builds a web-scrapper to automatically serch for, and add,
# the addresses of all facilities.
#
#########################################################################

library(tidyverse)
library(RSelenium)

library(RSelenium)
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)
