## Reproducible Research Project 2

# load libraries

library(dplyr)
library(ggplot2)
library(stringr)


# Check to see if data exists in working directory and download it if it is not

if(!file.exists("./repdata%2Fdata%2FStormData.csv.bz2"))
  
  {
    fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileURL, destfile = "./repdata%2Fdata%2FStormData.csv.bz2")
}

# Read data file into data file d1

d1<-read.csv(bzfile("./repdata%2Fdata%2FStormData.csv.bz2"))

# Create a vector of the columns I want to keep

keep<-c(2,7,8,23,24,25,26,27,28)

d2<-d1[,keep]

# check for missing data

length(complete.cases(d2))

nrow(d2)

# trim off excess characters on dates

# convert from factor to character

d2$BGN_DATE<-as.character(d2$BGN_DATE)

# trim last 8 spaces

d2$BGN_DATE<-substr(d2$BGN_DATE, 1, nchar(d2$BGN_DATE)-8)

# convert to dates

d2$BGN_DATE<-as.Date(d2$BGN_DATE, "%m/%d/%y")










