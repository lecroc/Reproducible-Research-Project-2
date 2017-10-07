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

d2$BGN_DATE<-as.Date(d2$BGN_DATE, "%m/%d/%Y")

# convert date to year

d2$BGN_DATE<-format(d2$BGN_DATE, format="%Y")

# Update Names

names(d2)<-c("Year", "State", "Event", "Deaths", "Injuries", "PropertyDamage", "PExp", "CropDamage", "CExp")

# Look at levels of propergy damage multiplier

Plvllist<-levels(d2$PExp)
Clvllist<-levels(d2$CExp)


d2$PExp<-as.character(d2$PExp)

d2<-d2 %>% 
    mutate(PExp = ifelse(PExp == "H" | PExp == "h", "2", PExp)) %>%
    mutate(PExp = ifelse(PExp == "K" | PExp == "k", "3", PExp)) %>%
    mutate(PExp = ifelse(PExp == "M" | PExp == "m", "6", PExp)) %>%
    mutate(PExp = ifelse(PExp == "B" | PExp == "b", "9", PExp)) %>%
    mutate(CExp = ifelse(CExp == "H" | CExp == "h", "2", CExp)) %>%
    mutate(CExp = ifelse(CExp == "K" | CExp == "k", "3", CExp)) %>%
    mutate(CExp = ifelse(CExp == "M" | CExp == "m", "6", CExp)) %>%
    mutate(CExp = ifelse(CExp == "B" | CExp == "b", "9", CExp))


d2$PExp<-as.numeric(d2$PExp)
d2$CExp<-as.numeric(d2$CExp)

d2 <- d2 %>%
  mutate(PExp = ifelse(is.na(PExp),0,PExp)) %>%
  mutate(CExp = ifelse(is.na(CExp),0,CExp))

d2$PropertyDamage<-d2$PropertyDamage*10^d2$PExp
d2$CropDamage<-d2$CropDamage*10^d2$CExp

d3<-d2 %>%
  group_by(Event) %>%
  summarize(PropertyDamage=sum(PropertyDamage), CropDamage = sum (CropDamage))

View(d3)


