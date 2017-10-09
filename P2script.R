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

# Show columns I've kept

names(d2)

# check for missing data

length(complete.cases(d2))

nrow(d2)

# trim off excess characters on dates

# convert from factor to character

d2$BGN_DATE<-as.character(d2$BGN_DATE)

# look at what dates look like

head(d2$BGN_DATE)

# trim last 8 spaces

d2$BGN_DATE<-substr(d2$BGN_DATE, 1, nchar(d2$BGN_DATE)-8)

# convert to dates

d2$BGN_DATE<-as.Date(d2$BGN_DATE, "%m/%d/%Y")

# convert date to year

d2$BGN_DATE<-format(d2$BGN_DATE, format="%Y")

# Update Names

names(d2)<-c("Year", "State", "Event", "Deaths", "Injuries", "PropertyDamage", "PExp", "CropDamage", "CExp")

head(d2)

# Look at levels of propergy damage multiplier

Plvllist<-levels(d2$PExp)
Clvllist<-levels(d2$CExp)

Plvllist
Clvllist

#Convert Exponent columns to character

d2$PExp<-as.character(d2$PExp)
d2$CExp<-as.character(d2$CExp)

# Convert exponent flags to numeric characters

d2<-d2 %>% 
    mutate(PExp = ifelse(PExp == "H" | PExp == "h", "2", PExp)) %>%
    mutate(PExp = ifelse(PExp == "K" | PExp == "k", "3", PExp)) %>%
    mutate(PExp = ifelse(PExp == "M" | PExp == "m", "6", PExp)) %>%
    mutate(PExp = ifelse(PExp == "B" | PExp == "b", "9", PExp)) %>%
    mutate(PExp = ifelse(PExp == "+", "0", PExp))               %>%
    mutate(CExp = ifelse(CExp == "H" | CExp == "h", "2", CExp)) %>%
    mutate(CExp = ifelse(CExp == "K" | CExp == "k", "3", CExp)) %>%
    mutate(CExp = ifelse(CExp == "M" | CExp == "m", "6", CExp)) %>%
    mutate(CExp = ifelse(CExp == "B" | CExp == "b", "9", CExp)) %>%
    mutate(CExp = ifelse(CExp == "+", "0", CExp))

# Convert numeric flags to actual numbers - creates NAs for non numerics

d2$PExp<-as.numeric(d2$PExp)
d2$CExp<-as.numeric(d2$CExp)

# Convert NAs to -1

d2 <- d2 %>%
  mutate(PExp = ifelse(is.na(PExp),-1,PExp)) %>%
  mutate(CExp = ifelse(is.na(CExp),-1,CExp))

# Turn off scientific notation

options(scipen = 999)

# Convert Property Damage and Crop Damage estimates by multiplying by the exponents

d2<- d2 %>%
  mutate(PropertyDamage = ifelse(PExp == -1, 0, PropertyDamage*10^PExp)) %>%
  mutate(CropDamage = ifelse(CExp == -1, 0, CropDamage*10^CExp))

# drop exponent columns - don't need anymore

keep2<-c(1,2,3,4,5,6,8)

d2<-d2[,keep2]

d3<-d2 %>%
  group_by(Event) %>%
  summarize(PropertyDamage=sum(PropertyDamage), CropDamage = sum (CropDamage), Deaths=sum(Deaths), Injuries=sum(Injuries))

View(d3)


