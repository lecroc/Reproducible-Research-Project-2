## Reproducible Research Project 2

if(!file.exists("./repdata%2Fdata%2FStormData.csv.bz2"))
  
  {
    fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileURL, destfile = "./repdata%2Fdata%2FStormData.csv.bz2")
  }

d1<-read.csv(bzfile("./repdata%2Fdata%2FStormData.csv.bz2"))







