## Reproducible Research Project 2

# load libraries

if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Turn off scientific notation

options(scipen = 999)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



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

d2$BGN_DATE<-year(d2$BGN_DATE)

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


# Convert Property Damage and Crop Damage estimates by multiplying by the exponents

d2<- d2 %>%
  mutate(PropertyDamage = ifelse(PExp == -1, 0, PropertyDamage*10^PExp)) %>%
  mutate(CropDamage = ifelse(CExp == -1, 0, CropDamage*10^CExp))

# Group by Events and total damage and health statistics

d3<-d2 %>%
  group_by(Event) %>%
  summarize(PropertyDamage=sum(PropertyDamage), CropDamage = sum (CropDamage), Deaths=sum(Deaths), Injuries=sum(Injuries))

# Add Deaths + Injuries and Crop + Property Damage

d3$TotalHealth<-with(d3, Deaths+Injuries)
d3$TotalDamage<-with(d3, PropertyDamage+CropDamage)

# Select only the columns I need for plots

keep3<-c(1,6,7)

d3<-d3[,keep3]

# Arrange on table by Health Impact and another by Damage

p1<-arrange(d3, desc(TotalHealth))

p2<-arrange(d3, desc(TotalDamage))

# Select the top 15 Health Impact Events and the top 10 Damage Events

p1<-p1[1:15,]

p2<-p2[1:15,]

# Check to see what percentage of totals are represented by the top 15

TH<-sum(d3$TotalHealth)
TD<-sum(d3$TotalDamage)
PH<-sum(p1$TotalHealth)
PD<-sum(p2$TotalDamage)

PH/TH # Percent of total health impact from top 10

PD/TD # Percent of total damage from top 10

# Add percentage column to p1 and p2

p1$percent<-p1$TotalHealth/TH

p2$percent<-p2$TotalDamage/TD



# Create plots of top 15s for Health and Damage Impact

plot1<-ggplot(aes(x=Event, y=TotalHealth),data=p1)+geom_bar(fill="blue", stat="identity")+
  labs(x="Event", y="Deaths + Injuries", title="Top 15 Weather Events in Terms of Deaths+Injuries")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  scale_x_discrete(limits=p1$Event)+scale_y_continuous(labels = comma)

plot2<-ggplot(aes(x=Event, y=TotalDamage),data=p2)+geom_bar(fill="blue", stat="identity")+
  labs(x="Event", y="Total Damage in Dollars", title="Top 15 Weather Events in Terms of Property + Crop Damage")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  scale_x_discrete(limits=p2$Event)+scale_y_continuous(labels = comma)


multiplot(plot1, plot2, cols=2)



# Create a vector of uniqe Events from both top 15 lists

keepnames<-unique(c(as.character(p1$Event), as.character(p2$Event)))

# Create a new data table for time series plot of impactful events

year<-d2

year$Event<-as.character(year$Event)

year<-subset(year, year$Event %in% keepnames)

yearplot<-year %>%
  group_by(Year) %>%
  summarize(HealthEffects=sum(Deaths+Injuries), FinancialEffects=sum(PropertyDamage+CropDamage)) %>%
  arrange(Year)

plot3<-ggplot(yearplot, aes(x=Year, y=HealthEffects))+geom_line(col="blue")+
  labs(x="Year", y="Deaths + Injuries", title="Health Effects of Weather Events by Year")+
  geom_smooth(method = "auto")+scale_y_continuous(labels = comma)

plot4<-ggplot(yearplot, aes(x=Year, y=FinancialEffects))+geom_line(col="blue")+
  labs(x="Year", y="Property + Crop Damage", title="Financial Effects of Weather Events by Year")+
  geom_smooth(method = "auto")+scale_y_continuous(labels = comma)

multiplot(plot3, plot4, cols=2)

