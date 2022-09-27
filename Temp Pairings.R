library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)
library(chron)
library(readxl)
require(suncalc)

data_wide <- read_excel("master_temp.xlsx")

data_wide <- data_wide %>% select(order(colnames(.))) #alphabetize columns
data_wide <- data_wide %>% select(Date, CH1:15) 
#set exact column order. I think I could do columns :14 instead of setting all
data_wide

meanCH1 <- mean(data_wide$CH1, na.rm=TRUE)
meanCH1
meanIN1 <- mean(data_wide$IN1, na.rm=TRUE)
meanIN1

#unsure if this is necessary
Hours <- format(as.POSIXct(data_wide$`Date`, "%m/%d/%Y %H:%M", tz = ""), format = "%H:%M")
Hours
Dates <- as.Date(data_wide$`Date`)
Dates

#add dates and hours columns to end of dataset!
data_wide$Hours <- format(as.POSIXct(data_wide$`Date`, "%m/%d/%Y %H:%M", tz = ""), format = "%H:%M")
#Dates expression had an unknown timezone error but still went through
data_wide$Dates <- format(as.Date(data_wide$`Date`,"%m/%d/%Y"), format = "%m/%d/%Y")
glimpse(data_wide)

data_long <- gather(data_wide, Chamber, Temperature, CH1:IN7, factor_key=TRUE) 
#gives warning: attributes are not identical across measure variables; they will be dropped 
data_long

#find daily max/min temps
sumDat <- data_long %>%
  group_by(Chamber) %>%
  group_by(Dates) %>% #how do I get it to group by both?
  summarise(dailyHi = max(data_long$Temperature, na.rm=TRUE),
            dailyLo = min(data_long$Temperature, na.rm=TRUE))
sumDat #gives same value for every day

#Summary Stats, all data including on and off seasons
sumDat <- data_long %>%
  group_by (Chamber) %>%
  summarise(
    nObservations = n(),	#note empty parenthesis
    meanTemp = mean(Temperature, na.rm=TRUE),
    sdTemp = sd(Temperature, na.rm=TRUE),
    se = sdTemp/sqrt(n()),
    maxTemp = max(Temperature, na.rm=TRUE),
    minTemp = min(Temperature, na.rm=TRUE),
    #calls column of daily high temps, unsure how to make this work
    #meanHigh = mean(dailyHi = max(group_by(Dates(data_long$Temperature, na.rm=TRUE))), 
    #meanLow = mean(dailyLo))
  )
sumDat #1: why did it resort table after I had gathered data_long above to exclude 
##the combined date/time column?
#2: why aren't dailyHi and dailyLo considered variables?



#Graphing

#Pairwise Comparisons
#barchart with standard error bars!!
ggplot(sumDat) +
  geom_bar(aes(x = Chamber, y = meanTemp), stat="identity") +
  geom_errorbar(aes(x = Chamber, ymin=meanTemp-se, ymax=meanTemp+se), width=.3, color="turquoise", alpha=.9, size=1)+
  ylab("Chamber") +
  xlab("Mean Temperature") +
  theme_bw()


