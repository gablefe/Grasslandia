library(tidyverse)
library(ggplot2)
library(reshape2)

#Hey. You're doing great, kid
data_wide <- read.csv("combined_temps.csv")
data_wide

data_wide %>% select(order(colnames(.))) #alphabetize columns
data_wide <- data_wide %>% select(Date, CO1, CO4, IN2, IN3, IN7, CH1, CH2, CH3, 
                                  CH4, CH5, CH6, CH7,CH8) 
#set exact column order. I think I could do columns :14 instead of setting all

data_long <- gather(data_wide, Chamber, Temperature, CO1:CH8, factor_key=TRUE) 
  #I don't think the factor key argument matters
data_long

data_long %>% filter(Date >= "10/28/2021 0:00")
data_long_omitted <- na.omit(data_long) #hopefully this truncates data to where we 
#have temps for all plots (after 10/28 when chambers placed)

#let's visualize!
#Individual plots
ggplot(data = data_long_omitted, aes(x=Date, y=Temperature, color = Chamber)) +
  geom_point(size=.5) +
  facet_wrap(~Chamber)
ggsave("IndivPlots.png")

#Combined plot
ggplot(data = data_long_omitted, aes(x=Date, y=Temperature, color = Chamber)) +
  geom_point(size=.5)
ggsave("CombinedPlots.png")


#Summary Stats
data_long_omitted %>%
  group_by (Chamber) %>%
  summarise(
    nObservations = n(),	#note empty parenthesis
    meanTemp = mean(Temperature),
    sdTemp = sd(Temperature))

#separating date from time...
data <- data_long_omitted %>% select(Date)
as.Date(data)


#Pairwise Comparisons
data_1 <- select(data_wide, Date, CO1, CH1) 
data_1 <- gather(data_1, Chamber, Temperature, CO1, CH1, factor_key=TRUE)
data_1 <- na.omit(data_1)
data_1

ggplot(data = data_1, aes(x=Date, y=Temperature, color = Chamber)) +
  geom_point(size=.5)
ggsave("CO1VsCH1.png")

data_1 <- mutate(data_1, Diff = CO1 - CH1) #trying to compare differences

data_1 %>% summarise(
  nObservations = n(),	#note empty parenthesis
  meanTemp = mean(Temperature),
  sdTemp = sd(Temperature),
  difference = CO1 - CH1) #can't get difference to work


data_1 %>% 
  dcast(Chamber ~ Temperature) %>% 
  mutate(Diff = CO1 - CH1) %>% 
  select(Temperature, Chamber) #can't figure out why this doesn't work


##other pairwise comparisons: IN2vs CH2, IN3 vs CH3, CO4 vs CH4, IN7 vs CH7

data_2 <- select(data_wide, Date, IN2, CH2) 
data_2 <- gather(data_2, Chamber, Temperature, IN2, CH2, factor_key=TRUE)
data_2 <- na.omit(data_2)
data_2

ggplot(data = data_2, aes(x=Date, y=Temperature, color = Chamber)) +
  geom_point(size=.5)
ggsave("IN2VsCH2.png")


data_2 <- select(data_wide, Date, IN2, CH2) 
data_2 <- gather(data_2, Chamber, Temperature, IN2, CH2, factor_key=TRUE)
data_2 <- na.omit(data_2)
data_2

ggplot(data = data_2, aes(x=Date, y=Temperature, color = Chamber)) +
  geom_point(size=.5)
ggsave("IN2VsCH2.png")