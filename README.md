# North Carolina Daycare PDF Scrapper

The North Carolina Division of Child Development and Early Education collects information on all daycares in North Carolina. The information includes the daycare's name and number of children, and is updated monthly. Unfortunately, the data is imprisoned within PDF files.

The PDF files can be found here: (https://ncchildcare.ncdhhs.gov/County/Child-Care-Snapshot/Child-Care-Statistical-Report).

This repo contains R scripts that scrape the PDF files, enter the data into data frames, and output the data as csv files.  The PDF files are pulled straight from the internet into RAM, and then scraped and discarded.  They do not touch the hard drive.

The script produces a distinct csv file for each month's data, and the data includes all licensed daycare facilities within North Carolina.  The columns in the csv file follow the columns in the original PDF. One difference, however, is that the `Category Operation` and `Operation Site` columns in the PDF are merged in the same column within the csv file. The is a by-product of how the data is extracted, and a follow-up script will clean and separate these column so they mirror the PDF files.

In 2017, the state changed the format of the PDF files.  This change broke the scraper, so two scrapers are required. The file `extract_data_new.R` handles the new format, while `extract_data_old.R` scrapes the old format.

### Data files

The data files from the scrapped PDFs are [also on github](https://github.com/forsythfuture/child-care-pdf-scraper/tree/master/data). Each file is a different month and can be imported directly into R with the following command:

```r
library(tidyverse)

# import Janaury 2019 data
read_csv('https://raw.githubusercontent.com/forsythfuture/child-care-pdf-scraper/master/data/nc_january_2019.csv')
```

### Addresses

Facility addresses are not contained in the PDF files, but the NC DHHS maintains a log of addresses at this site: http://ncchildcaresearch.dhhs.state.nc.us/search.asp.

The repo contains a web scraper in the `add_address.R` file that pulls the address for facilities.

### Missing data

The following months are missing from the scraped data due to either the raw PDF file missing, or the PDF file being unscrapable: February 2017 and November 2016.