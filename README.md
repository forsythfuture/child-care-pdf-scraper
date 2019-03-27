# North Carolina Daycare PDF Scrapper

The North Carolina Division of Child Development and Early Education collects information on all daycares in North Carolina. The information includes the daycare's name and numebr of children; and is updated monthly. Unfortunately, the data is trapped within PDF files.

The PDF files can be found here: (https://ncchildcare.ncdhhs.gov/County/Child-Care-Snapshot/Child-Care-Statistical-Report).

This repo contains an R script that scrapes the PDF files and enters the data into a single data frame.  The PDF files are pulled straight from the internet into RAM, and then scraped and discarded.  They do not touch the hard drive.  The only output is a csv file with the daycare data.