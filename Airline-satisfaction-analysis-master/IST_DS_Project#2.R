getwd
setwd("~/Desktop/Syracuse/0_IST687_Intro2DS")

install.packages("gdata")
library(gdata)
install.packages("dplyr")
library(dplyr)
install.packages("arule")
library(arules)
install.packages("aruleviz")
library(arulesViz)

airline.raw <- read.csv("Satisfaction Survey.csv")
airline1 <- read.csv("airplaneSurvey.csv") #this is fixed dataset
airline <- read.csv("airplaneSurvey (1).csv") #this is also fixed dataset
airline.cs <- read.csv("cheapseats.csv") #the airline we want to analyze
airline.wa <- read.csv("westAirways.csv") #the reference airline we want to analyze
age.hr.cs <- read.csv("age.csv")

which(is.na(airline.raw$Age))
summary(airline)
summary(airline.raw)
which(is.na(airline.raw$Satisfaction))
length(which(airline.raw$Airline.Name == "Cheapseats Airlines Inc. "))
str(airline.raw)

hist(airline.cs$satisfaction)
hist(airline$satisfaction)

#creating new column with month
airline$month <- 0
airline$month <- substr(airline$date, 1,1)
airline$month[which(airline$month == "1")] <- "JAN"
airline$month[which(airline$month == "2")] <- "FEB"
airline$month[which(airline$month == "3")] <- "MAR"
View(airline)

#same thing for cs: creating new column with month
airline.cs$month <- 0
airline.cs$month <- substr(airline.cs$date, 1,1)
airline.cs$month[which(airline.cs$month == "1")] <- "JAN"
airline.cs$month[which(airline.cs$month == "2")] <- "FEB"
airline.cs$month[which(airline.cs$month == "3")] <- "MAR"
View(airline.cs)

#and same thing for wa: creating new column with month
airline.wa$month <- 0
airline.wa$month <- substr(airline.wa$date, 1,1)
airline.wa$month[which(airline.wa$month == "1")] <- "JAN"
airline.wa$month[which(airline.wa$month == "2")] <- "FEB"
airline.wa$month[which(airline.wa$month == "3")] <- "MAR"
View(airline.wa)

#creating age groups for all 3 files
labs <- c(paste(seq(0, 95, by = 10), seq(0 + 10 - 1, 100 - 1, by = 10),
                sep = "-"), paste(100, "+", sep = ""))
labs

airline$AgeGroup <- cut(airline$age, breaks = c(seq(0, 100, by = 10), Inf), labels = labs, right = FALSE)
airline.cs$AgeGroup <- cut(airline.cs$age, breaks = c(seq(0, 100, by = 10), Inf), labels = labs, right = FALSE)
airline.wa$AgeGroup <- cut(airline.wa$age, breaks = c(seq(0, 100, by = 10), Inf), labels = labs, right = FALSE)


#for 3 files - create new column - num.yrs - showing number of years a customer has been
airline$num.yrs <- 2014 - airline$firstYearFlight
str(airline)
summary(airline$num.yrs)
length(which(is.na(airline$satisfaction))) #no customers are less than 1 year
#median is 7 -->50% of the customers have about 7 years of being customers
View(airline)

#create new column - num.yrs - showing number of years a customer has been
airline.cs$num.yrs <- 2014 - airline.cs$firstYearFlight
str(airline.cs)
summary(airline.cs$num.yrs)

#create new one for wa
airline.wa$num.yrs <- 2014 - airline.wa$firstYearFlight
str(airline.wa)
summary(airline.wa$num.yrs)

#not helpful plot 
plot(airline.cs$num.yrs,airline.cs$satisfaction)

hist(airline.cs$num.yrs)
hist(airline.wa$num.yrs)
hist(airline$num.yrs)
#median is 7 -->50% of the customers have about 7 years of being customers

sum(is.na(airline$flightCancelled))
#no NA values
summary(airline$flightCancelled)
table(airline.cs$flightCancelled)
can.cs <- filter(airline.cs, flightCancelled == "Yes")
hist(can.cs$satisfaction)
mean(can.cs$satisfaction) #3.13
mean(airline.cs$satisfaction) #3.36 - the satisfaction in cancelled flight dropped 7.27%
((mean(airline.cs$satisfaction)-mean(can.cs$satisfaction))/mean(can.cs$satisfaction))

nocan.cs <- filter(airline.cs, flightCancelled == "No")
hist(nocan.cs$satisfaction)
mean(nocan.cs$satisfaction) 
mean(airline.cs$satisfaction) #3.36 - the satisfaction in cancelled flight dropped 7.27%
((mean(airline.cs$satisfaction)-mean(nocan.cs$satisfaction))/mean(nocan.cs$satisfaction))


#create a new column - num.flightbyYr - showing the number of flights per year 
airline$num.flightbyYr <- airline$numberOfFlights/airline$num.yrs
summary(airline$num.flightbyYr)
airline.cs$num.flightbyYr <- airline.cs$numberOfFlights/airline.cs$num.yrs
summary(airline.cs$num.flightbyYr)

#customers who have never taken any flights
nonflight <- filter(airline,numberOfFlights == 0)
nonflight.cs <- filter(airline.cs,numberOfFlights == 0)
hist(nonflight.cs$satisfaction)
hist(nonflight$satisfaction)
mean(nonflight.cs$satisfaction) #3.558847 vs. 3.357318 (overall)
mean(nonflight$satisfaction) #3.574304    vs. 3.379409 (overall)
nrow(nonflight.cs) # wow there are 1249 customers never take any flights
nrow(nonflight)    # wow there are 6211 customers never take any flights
View(nonflight)
View(nonflight.cs)

#median is only 2.67 flights per year which is pretty low
nrow(airline) #total we have 129889 observations
hist(airline$num.flightbyYr)

#did the same thing for both airlines
#create a new column - num.flightbyYr - showing the number of flights per year 
airline.cs$num.flightbyYr <- airline.cs$numberOfFlights/airline.cs$num.yrs
summary(airline.cs$num.flightbyYr) #max decreases to 41
#median is only 2.67 flights per year which is pretty low
hist(airline.cs$num.flightbyYr)
plot(airline.cs$num.flightbyYr, airline.cs$satisfaction)

airline.wa$num.flightbyYr <- airline.wa$numberOfFlights/airline.wa$num.yrs
summary(airline.wa$num.flightbyYr) #max decreases to 36.5
#median is only 2.7 flights per year which is pretty low
hist(airline.wa$num.flightbyYr)

length(which(airline$num.flightbyYr > 10))
#10132 of customers who have over 20 flights per year
10132/26058
length(which(airline$num.flightbyYr > 20))
#1509 of customers who have over 20 flights per year
length(which(airline$num.flightbyYr > 30))
#142 of customers who have over 30 per year
length(which(airline$num.flightbyYr > 40))
#10 of customers who have over 30 per year

(3.357-3.487)/3.487
(3.487-3.357)/3.357


#airlineName
df.airN <- group_by(airline, airlineName)
airlineName <- summarise(df.airN, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                         percent = n()/129889*100)
airlineName <- filter(airlineName, count >10000)
View(airlineName)
summary(airlineName$avg_sat)
plot(airlineName$count, airlineName$avg_sat)

#for overall - origin city
df.sta1 <- group_by(airline, originCity)
oricity <- summarise(df.sta1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(oricity)
oricity$perc <- oricity$count/129889*100

#for overall - dest city
df.sta2 <- group_by(airline, destCity)
descity <- summarise(df.sta2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
descity$perc <- descity$count/129889*100
View(descity)

#for cs - gender + airlinestatus
df.sta.cs <- group_by(airline.cs, gender)
summarise(df.sta.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                Percent = count/26058*100)
14666+11392

#for cs - class
df.cla.cs <- group_by(airline.cs, class)
summarise(df.cla.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
          Percent = count/26058*100)

df.cla.cs1 <- group_by(airline.cs, arriveDelay.5, month)
summarise(df.cla.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
          Percent = count/26058*100)

df.rat.cs1 <- group_by(airline.cs, satisfaction)
summarise(df.rat.cs1, count = n(), Percent = count/26058*100)


#for cs - age
df.age.cs <- group_by(airline.cs, AgeGroup) #satisfaction score = 3.35
age.cs <- summarise(df.age.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                    aboveMean = mean(satisfaction)<3.35, PopPerc = n()/26058*100)
View(age.cs)
hist(age.cs$avg_sat)
hist(age.cs$count)

airline.cs$AgeGroup
airline.cs$satisfaction

df.age.cs1 <- group_by(pt.cs, agegroup1) #satisfaction score = 2.5
age.cs1 <- summarise(df.age.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                    PopPerc = n()/8069*100)

df.age.cs2 <- group_by(bt.cs, agegroup1) #satisfaction score = 3.75
age.cs2 <- summarise(df.age.cs2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                     PopPerc = n()/15996*100)


#graph for age groups - personal travel
g.age.cs1 <- ggplot(age.cs1,aes(x=agegroup1,y=count, fill= avg_sat, group=1))
g.age.cs1 <- g.age.cs1 + geom_bar(width=.5, stat="identity")
g.age.cs1 <- g.age.cs1 + theme(axis.text.x = element_text(hjust = 1, size = 10))
g.age.cs1 <- g.age.cs1 + xlab("Age Groups") + ylab("percentage") + 
            ggtitle("Personal travel: Satisfaction by Age")
g.age.cs1

#graph for age groups - business travel
g.age.cs2 <- ggplot(age.cs2,aes(x=agegroup1,y=PopPerc, fill= avg_sat, group=1))
g.age.cs2 <- g.age.cs2 + geom_bar(width=.5, stat="identity")
g.age.cs2 <- g.age.cs2 + theme(axis.text.x = element_text(hjust = 1, size = 10))
g.age.cs2 <- g.age.cs2 + xlab("Age Groups") + ylab("percentage") + 
  ggtitle("Business travel: Satisfaction by Age")
g.age.cs2

g.age.cs <- ggplot(age.cs,aes(x=AgeGroup, count, y=count, fill=avg_sat, group=1))
g.age.cs <- g.age.cs + geom_bar(width=.5, stat="identity")
g.age.cs <- g.age.cs + theme(axis.text.x = element_text(hjust = 1, size = 10))
g.age.cs <- g.age.cs + xlab("Age Groups") + ylab("Number of customers") + ggtitle("Satisfaction by Age")
g.age.cs

g.age.cs5 <- ggplot(df.ty.cs1,aes(x=travelType, avg_food, y=avg_food, fill=avg_sat, group=1))
g.age.cs5 <- g.age.cs5 + geom_bar(width=.3, stat="identity")
g.age.cs5 <- g.age.cs5 + theme(axis.text.x = element_text(hjust = 1, size = 12))
g.age.cs5 <- g.age.cs5 + xlab("Travel Type") + ylab("Avg food spending") + 
              ggtitle("Food spending by travel type")
g.age.cs5

df.ty.cs <- group_by(airline.cs, travelType,gender)
df.ty.cs1 <- summarise(df.ty.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                       avg_food = mean(foodAtAirport,na.rm = TRUE), 
                       PopPerc = n()/26058*100)

df.ty.cs1 <- group_by(airline.cs, travelType, airlineStatus)
df.ty.cs2 <- summarise(df.ty.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE), 
                       avg_food = mean(foodAtAirport,na.rm = TRUE), 
                       PopPerc = n()/26058*100)


g.ty.cs <- ggplot(df.ty.cs2,aes(x=airlineStatus, y=avg_sat, group=travelType))
g.ty.cs <- g.ty.cs + geom_line(aes(colour=travelType)) + geom_point()
g.ty.cs <- g.ty.cs + theme(axis.text.x = element_text(hjust = 1, size = 10))
g.ty.cs <- g.ty.cs + xlab("Airline Status") + ylab("Average satisfaction ratings") + ggtitle("Average Satisfaction by travel types and airline status")
g.ty.cs

View(df.ty.cs2)
str(df.ty.cs2)
plot()

g.age.cs4 <- ggplot(data=df.ty.cs1, aes(x=travelType, y=avg_food, group =gender)) +
  geom_line(aes(color=gender))+geom_point()
g.age.cs4

g <- ggplot(oricity.cs,aes(x=reorder(originCity, count), y=count, fill=avg_sat, group=1))
g <- g + geom_bar(colour="white", width=.8, stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))
g <- g+ xlab("City Name") + ylab("Number of Flight") +
  ggtitle("Most flights of original City vs. avg satisfaction")
g

View(oricity.cs)
summary(airline.cs$satisfaction)

#bad graph omg
g1 <- ggplot(oricity.cs,aes(x=reorder(originCity, avg_sat), y=avg_sat, fill=count, group=1))
g1 <- g1 + geom_bar(colour="white", width=.8, stat="identity")
g1 <- g1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))
g1 <- g1 + xlab("City Name") + ylab("Number of Flight") +
  ggtitle("Most flights of original City vs. avg satisfaction")
g1

distCity.cs$destCity

g.d <- ggplot(distCity.cs,aes(x=reorder(destCity, count), y=count, fill=avg_sat, group=1))
g.d <- g.d + geom_bar(colour="white", width=.8, stat="identity")
g.d <- g.d + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))
g.d <- g.d + xlab("City Name") + ylab("Number of Flight") +
  ggtitle("Most flights of destination City vs. avg satisfaction")
g.d

mid_age <- filter(age.cs, aboveMean == FALSE)
sum(mid_age$PopPerc) #70.19% for age 20 to 59
sum(mid_age$count) #18291 for age 20 to 59

#age 10-19 and 60 and up in cheapseats
high_age.cs <- filter(airline.cs, !(age %in% (20:59)))
summary(high_age.cs$satisfaction)
hist(high_age.cs$satisfaction, main = "Histogram of Satisfaction Ratings for Age 10-19 & 60 and up")
nrow(high_age.cs) #7767 people

#age 20-59 in cheapseats
mid_age.cs <- filter(airline.cs, age %in% (20:59))
summary(mid_age.cs$satisfaction)
hist(mid_age.cs$satisfaction, main = "Histogram of Satisfaction Ratings for Age 20 to 59")
nrow(mid_age.cs) #18291 people 70.19% for age 20 to 59

#for cs - origin city
df.sta.cs1 <- group_by(airline.cs, originCity)
oricity.cs <- summarise(df.sta.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(oricity.cs)
oricity.cs$perc <- oricity.cs$count/26058*100

#for cs - dist city
df.sta.cs2 <- group_by(airline.cs, destCity)
distCity.cs <- summarise(df.sta.cs2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(distCity.cs)
distCity.cs$perc <- distCity.cs$count/26058*100

#adding new column with the high/low
summary(airline.cs$num.flightbyYr)
hist(airline.cs$num.flightbyYr)
hist(airline.cs$numberOfFlights)
hist(airline.cs$num.yrs)
hist(airline.cs$percentOfOtherFlight)
hist(airline.cs$percentOfOtherFlight)
hist(airline.cs$firstYearFlight)
table(airline.cs$gender)
table(airline.wa$gender)

categ1.satis <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >= 4] <- "High"
  vBuckets[vec <= 3] <- "Low"
  return(vBuckets)
}

quantile(airline$satisfaction,0.6)

categ1 <- function(vec) {
  q <- quantile(vec, c(0.35, 0.65))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

summary(airline$satisfaction)
summary(airline.cs$satisfaction)
summary(airline.wa$satisfaction)
View(airline.cs)
#creating category - sat.cs
str(airline.cs)
age.cs <- categ1(airline.cs$age)

names(airline.cs)

happyCust <- categ1.satis(airline.cs$satisfaction)
avg.fl <- categ1(airline.cs$num.flightbyYr)
yr.cs <- categ1(airline.cs$num.yrs)
food.cs <- categ1(airline.cs$foodAtAirport)
shopping.cs <- categ1(airline.cs$shoppingAtAirport)
airline.cs$pri.sen <- as.factor(airline.cs$priceSensitivity)
numOfFl.cs <- categ1(airline.cs$numberOfFlights)
loyCarNum.cs <- categ1(airline.cs$loyaltyCardsNum)
percOtherFl.cs <- categ1(airline.cs$percentOfOtherFlight)
flDis.cs <- categ1(airline.cs$flightDistance)
mon.cs <- as.factor(airline.cs$month)

str(airline.cs)

#making sure every variables are the same length
length(happyCust)
length(yr.cs)
length(food.cs)
length(shopping.cs)
length(numOfFl.cs)
length(loyCarNum.cs)
length(percOtherFl.cs)
length(flDis.cs)


str(airline.cs)
#creating new dataframe
ruleDF.cs <- data.frame(happyCust, avg.fl, yr.cs, food.cs, shopping.cs, numOfFl.cs, loyCarNum.cs,percOtherFl.cs, flDis.cs)
View(ruleDF.cs)
str(ruleDF.cs)
#add columns
ruleDF.cs$airlineStatus <- airline.cs$airlineStatus
ruleDF.cs$gender <- airline.cs$gender
ruleDF.cs$travelType <- airline.cs$travelType
ruleDF.cs$class <- airline.cs$class
ruleDF.cs$originCity <- airline.cs$originCity
ruleDF.cs$flightCancelled <- airline.cs$flightCancelled
ruleDF.cs$arriveDelay.5  <- airline.cs$arriveDelay.5 
ruleDF.cs$pri.sen <- airline.cs$pri.sen
ruleDF.cs$AgeGroup <- airline.cs$AgeGroup
ruleDF.cs$month <- as.factor(airline.cs$month)
ruleDF.cs$destCity <- airline.cs$destCity

ruleDF.cs2 <- ruleDF.cs1
ruleDF.cs2$scheduledDepHr <- airline.cs$scheduledDepHr

View(ruleDF.cs)
names(ruleDF.cs)
str(ruleDF.cs)
ruleDF.cs1 <- ruleDF.cs[,-20]
ruleDF.cs1 <- ruleDF.cs1[,-14]

install.packages("arules")
library(arules)

str(ruleDF.cs) #all 20 variables
str(ruleDF.cs1) #no city
str(ruleDF.cs2)
happycust.t1 <- filter(ruleDF.cs2,happyCust =="High")
unhappycust.t1 <- filter(ruleDF.cs2,happyCust =="Low")
hist(happycust.t1$scheduledDepHr)
hist(unhappycust.t1$scheduledDepHr)
unique(ruleDF.cs2$scheduledDepHr)

airline.cs$agegroup1 <- as.character(airline.cs$AgeGroup)
airline.cs$agegroup1[which(airline.cs$agegroup1 == "60-69")] <- "60 and up"
airline.cs$agegroup1[which(airline.cs$agegroup1 == "70-79")] <- "60 and up"
airline.cs$agegroup1[which(airline.cs$agegroup1 == "80-89")] <- "60 and up"
View(airline.cs)

df.hr.all <- group_by(airline.cs, agegroup1, scheduledDepHr)
df.hr.all1 <- summarise(df.hr.all, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(df.hr.all1)

df.hr <- group_by(happycust.t1, scheduledDepHr)
df.hr.happy <- summarise(df.hr, count = n())

View(ruleDF.cs2)

df.hr.all1$percent[which(df.hr.all1$AgeGroup == "10-19")] <- df.hr.all1$count/1594
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "20-29")] <- df.hr.all1$count/3130
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "30-39")] <- df.hr.all1$count/5295
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "40-49")] <- df.hr.all1$count/5599
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "50-59")] <- df.hr.all1$count/4267
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "60-69")] <- df.hr.all1$count/3122
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "70-79")] <- df.hr.all1$count/1994
df.hr.all1$percent[which(df.hr.all1$AgeGroup == "80-89")] <- df.hr.all1$count/1057

df.hr1 <- group_by(unhappycust.t1, scheduledDepHr)
df.hr.unhappy <- summarise(df.hr1, count = n())

merge.hr <- merge(df.hr.happy, df.hr.unhappy, by.x="scheduledDepHr", by.y="scheduledDepHr")
g.hr <- ggplot(data=merge.hr, aes(x=scheduledDepHr)) +
                geom_line(aes(y=count.x), colour ="blue") +
               geom_line(aes(y=count.y), colour ="black")
g.hr <- g.hr + ggtitle("Departure time for happy and unhappy groups")
g.hr <- g.hr + labs(x = "Scheduled Departure time", y = "Number of customers")
g.hr

g.hr1 <- ggplot(data=df.hr.all1, aes(x=scheduledDepHr, y=count, group =agegroup1)) +
  geom_line(aes(color=agegroup1))+geom_point()
g.hr1

g.hr2 <- ggplot(data=df.hr.all1, aes(x=scheduledDepHr, y=avg_sat, group =agegroup1)) +
  geom_line(aes(color=agegroup1))+geom_point()
g.hr2

mt.cs <- filter(airline.cs, travelType == "Mileage tickets")
bt.cs <- filter(airline.cs, travelType == "Business travel")
pt.cs <- filter(airline.cs, travelType == "Personal Travel")
hist(pt.cs$satisfaction)
hist(mt.cs$satisfaction)
hist(bt.cs$satisfaction)

mean(pt.cs$satisfaction)  #mean = 2.520882
mean(mt.cs$satisfaction)  #mean = 3.513297
mean(bt.cs$satisfaction)  #mean = 3.759815

table(pt.cs$class)
table(bt.cs$class)

table(pt.cs$agegroup1) #mostly 60 and up
table(bt.cs$agegroup1) #mostly 30-49

hist(pt.cs$agegroup1) #mostly 60 and up
hist(bt.cs$agegroup1) #mostly 30-49


table(pt.cs$priceSensitivity)
table(bt.cs$priceSensitivity)

table(pt.cs$scheduledDepHr) 
table(bt.cs$scheduledDepHr) 

mean(pt.cs$foodAtAirport) #71.44752
mean(bt.cs$foodAtAirport) #66.67186

mean(pt.cs$num.flightbyYr) #5.158664
mean(bt.cs$num.flightbyYr) #3.211661

mean(pt.cs$shoppingAtAirport) #26.33238
mean(bt.cs$shoppingAtAirport) #26.32208

table(pt.cs$arriveDelay.5) #59% no
table(bt.cs$arriveDelay.5) #58% no

airlineX.cs <- as(ruleDF.cs,"transactions")
airlineX.cs1 <- as(ruleDF.cs1,"transactions")
airlineX.cs2 <- as(ruleDF.cs2,"transactions")
unique(ruleDF.cs$pri.sen)

high.rating.cs <- filter(airline.cs,satisfaction >=3)
low.rating.cs <- filter(airline.cs,satisfaction <3)
table(low.rating.cs$airlineStatus)
table(low.rating.cs$gender)
table(low.rating.cs$AgeGroup)
table(low.rating.cs$travelType)
table(low.rating.cs$scheduledDepHr)
hist(low.rating.cs$scheduledDepHr)
hist(high.rating.cs$scheduledDepHr)
hist(high.rating.cs$priceSensitivity)
hist(low.rating.cs$priceSensitivity)
hist(low.rating.cs$percentOfOtherFlight)
hist(high.rating.cs$percentOfOtherFlight)
table(high.rating.cs$arriveDelay.5)
table(low.rating.cs$arriveDelay.5)

blue.cs <- filter(airline.cs, airlineStatus == "Blue")
silver.cs <- filter(airline.cs, airlineStatus == "Silver")
hist(blue.cs$satisfaction)
hist(silver.cs$satisfaction)
hist(silver.cs$numberOfFlights)
hist(blue.cs$numberOfFlights)
mean(silver.cs$num.flightbyYr)
mean(blue.cs$num.flightbyYr)
table(blue.cs$AgeGroup)
mean(silver.cs$satisfaction)
mean(blue.cs$satisfaction)


nrow(filter(airline.cs,satisfaction >= 4)) #13050
nrow(filter(airline.cs,satisfaction <= 3)) #13008

13008+13050
summary(airline.cs$satisfaction)

itemFrequency(airlineX.cs) #this tells you the frequencies for each items
itemFrequencyPlot(airlineX.cs, support=0.01, cex.names=0.4)
itemFrequencyPlot(airlineX.cs1, support=0.01, cex.names=0.7)

airlinerule.cs <- apriori(airlineX.cs, parameter = list(support=0.2, confidence = 0.2))
inspect(airlinerule.cs) #1724 rules
inspect(head(airlineX.cs, n = 5, by ="lift"))

airlinerule.cs1 <- apriori(airlineX.cs1, parameter = list(support=0.3, confidence = 0.3))
inspect(airlinerule.cs1) #358 rules
inspect(head(airlinerule.cs1, n = 5, by ="lift"))

plot(airlinerule.cs1)
head(quality(airlinerule.cs1))

plot(airlinerule.cs)
head(quality(airlinerule.cs))


happycust.rule.cs <- apriori(airlineX.cs, parameter=list(support=0.09, confidence=0.09), 
                          appearance = list(default="lhs", rhs=("happyCust=High")))
inspect(happycust.rule.cs) #886 rules
summary(happycust.rule.cs) #886 rules

happycust.rule.cs1 <- apriori(airlineX.cs, parameter=list(support=0.1, confidence=0.1), 
                             appearance = list(default="lhs", rhs=("happyCust=High")))
inspect(happycust.rule.cs1) #660 rules
summary(happycust.rule.cs1) #660 rules

happycust.rule.cs1 <- apriori(airlineX.cs, parameter=list(support=0.15, confidence=0.15), 
                              appearance = list(default="lhs", rhs=("happyCust=High")))
inspect(happycust.rule.cs1) #180 rules
summary(happycust.rule.cs1) #180 rules LIFT Q3 = 1.3760

happycust.rule.cs2 <- apriori(airlineX.cs1, parameter=list(support=0.15, confidence=0.15), 
                              appearance = list(default="lhs", rhs=("happyCust=High")))
inspect(happycust.rule.cs2) #180 rules
summary(happycust.rule.cs2) #57 rules LIFT Q3 = 1.3897
plot(happycust.rule.cs2)

unhappycust.rule.cs <- apriori(airlineX.cs, parameter=list(support=0.05, confidence=0.05), 
                             appearance = list(default="lhs", rhs=("happyCust=Low")))
inspect(unhappycust.rule.cs) #4887 rules
summary(unhappycust.rule.cs) #4887 rules

unhappycust.rule.cs1 <- apriori(airlineX.cs, parameter=list(support=0.09, confidence=0.09), 
                               appearance = list(default="lhs", rhs=("happyCust=Low")))
inspect(unhappycust.rule.cs1) #916 rules
summary(unhappycust.rule.cs1) #916 rules

unhappycust.rule.cs2 <- apriori(airlineX.cs, parameter=list(support=0.12, confidence=0.12), 
                                appearance = list(default="lhs", rhs=("happyCust=Low")))
inspect(unhappycust.rule.cs2) #374 rules
summary(unhappycust.rule.cs2) #374 rules

unhappycust.rule.cs3 <- apriori(airlineX.cs, parameter=list(support=0.15, confidence=0.15), 
                                appearance = list(default="lhs", rhs=("happyCust=Low")))
inspect(unhappycust.rule.cs3) #175 rules
summary(unhappycust.rule.cs3) #175 rules


goodrule.cs <- happycust.rule.cs1[quality(happycust.rule.cs1)$lift >1.3760] #LIFT Q3 = 1.3760
inspect(goodrule.cs) #45 rules left
top.lift <- sort(goodrule.cs, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, n=15))

badrule.cs <- unhappycust.rule.cs3[quality(unhappycust.rule.cs3)$lift >1.3096] #LIFT Q3 = 1.3096
summary(badrule.cs) #44 rules left
inspect(badrule.cs) #44 rules left
top.lift.bad <- sort(badrule.cs, decreasing = TRUE, na.last = NA, by = "lift")
top.lift.bad1 <- inspect(head(top.lift.bad, n=20))

plot.goodrule.cs <- apriori(airlineX.cs,parameter=list(support=0.2, confidence=0.2))
plot(plot.goodrule.cs) #1561 rules

plot.goodrule.cs1 <- apriori(airlineX.cs,parameter=list(support=0.1, confidence=0.1))
plot(plot.goodrule.cs1) #15731 rules

plot.goodrule.cs2 <- apriori(airlineX.cs1,parameter=list(support=0.2, confidence=0.2))
plot(plot.goodrule.cs2) #1561 rules

plot(goodrule.cs, measure = c("support", "lift"), shading = "confidence")
plot(happycust.rule.cs1, method = "two-key plot")
plot(goodrule.cs, method = "grouped")
plot(badrule.cs, method = "grouped")
plot(happycust.rule.cs, method = "grouped")
plot(unhappycust.rule.cs2, method = "grouped") #704
plot(happycust.rule.cs1, method = "grouped") #660
plot(airlinerule.cs, method = "grouped") #general 15731
plot(plot.goodrule.cs, method = "grouped") #1561
plot(happycust.rule.cs5, method = "grouped") #1823
plot(unhappycust.rule.cs3, method = "grouped") #3195

plot(top.lift.bad, method = "paracoord") #704
plot(happycust.rule.cs1, method = "paracoord") #180
plot(unhappycust.rule.cs3, method = "paracoord") #374
plot(badrule.cs, method = "paracoord") #374

plot(badrule.cs, method = "paracoord", control = list(reorder = TRUE)) #36

plot(unhappycust.rule.cs, method = "matrix", measure = "lift")

plot(goodrule.cs, method = "paracoord") #172
plot(unhappycust.rule.cs1, method = "paracoord") #142

happycust.rule.cs5 <- apriori(airlineX.cs, parameter=list(support=0.07, confidence=0.07), 
                             appearance = list(default="lhs", rhs=("happyCust=High")))
inspect(happycust.rule.cs5) #886 rules
summary(happycust.rule.cs5) #886 rules

unhappycust.rule.cs <- apriori(airlineX.cs, parameter=list(support=0.03, confidence=0.03), 
                               appearance = list(default="lhs", rhs=("happyCust=Low")))
inspect(unhappycust.rule.cs) #704 rules
summary(unhappycust.rule.cs) #704 rules

sel <- plot(airlineX.cs1, measure=c("support", "lift"), 
            shading = "confidence",
            interactive = TRUE)
plot(sel)
image(sel)

plot(badrule.cs, method = "matrix3D", measure = "lift")

#setting up arule with WA
happyCust.w <- categ1(airline.wa$satisfaction)
avg.fl.w <- categ1(airline.wa$num.flightbyYr)
yr.cs.w <- categ1(airline.wa$num.yrs)
food.cs.w <- categ1(airline.wa$foodAtAirport)
shopping.cs.w <- categ1(airline.wa$shoppingAtAirport)
age.cs.w <- categ1(airline.wa$age)
airline.wa$pri.sen <- as.factor(airline.wa$priceSensitivity)
airline.wa$month <- as.factor(airline.wa$month)

str(airline.wa)
#creating new dataframe
ruleDF.wa <- data.frame(happyCust.w, food.cs.w, shopping.cs.w, age.cs.w)
View(ruleDF.wa)
#add columns
ruleDF.wa$airlineStatus <- airline.wa$airlineStatus
ruleDF.wa$gender <- airline.wa$gender
ruleDF.wa$travelType <- airline.wa$travelType
ruleDF.wa$class <- airline.wa$class
ruleDF.wa$originCity <- airline.wa$originCity
ruleDF.wa$flightCancelled <- airline.wa$flightCancelled
ruleDF.wa$arriveDelay.5  <- airline.wa$arriveDelay.5 
ruleDF.wa$pri.sen <- airline.wa$pri.sen
ruleDF.wa$AgeGroup <- airline.wa$AgeGroup
ruleDF.wa$month <- airline.wa$month
ruleDF.wa$destCity <- airline.wa$destCity

str(ruleDF.wa)
airlineX.wa <- as(ruleDF.wa,"transactions")

itemFrequency(airlineX.wa) #this tells you the frequencies for each items
itemFrequencyPlot(airlineX.wa, support=0.01, cex.names=0.5)

happycust.rule.wa <- apriori(airlineX.wa, parameter=list(support=0.03, confidence=0.03), 
                             appearance = list(default="lhs", rhs=("happyCust.w=High")))
inspect(happycust.rule.wa)
summary(happycust.rule.wa) #246 rules

badcust.rule.wa <- apriori(airlineX.wa, parameter=list(support=0.1, confidence=0.1), 
                             appearance = list(default="lhs", rhs=("happyCust.w=Low")))
inspect(badcust.rule.wa)
summary(badcust.rule.wa) 

badrule.wa <- badcust.rule.wa[quality(badcust.rule.wa)$lift >1.53] #Q3 is 1.5237
inspect(badrule.wa) #108 rules

goodrule.wa <- happycust.rule.wa[quality(happycust.rule.cs)$lift >1.6] #median is 1.5206
inspect(goodrule.wa)

goodrule.wa1 <- happycust.rule.wa[quality(happycust.rule.cs)$lift >1.86] #Q3 is 1.8529
inspect(goodrule.wa1)


#MAKE SURE IT'S HAPPY
#delay - cs
del.cs <- categ1(airline.cs$arriveDelayMinute, na.rm = TRUE)
del.cs
na.cs.del <- airline.cs[is.na(airline.cs$arriveDelayMinute),]
length(na.cs.del$flightCancelled == "Yes")
#we know that there are 389 na in delay.min but 316 of them are cancelled
table(airline.cs$arriveDelay.5)
15263/(15263+10795)
table(airline.cs$flightCancelled)
316/(316+25742)

delay.cs <- filter(airline.cs,arriveDelay.5 == "yes")
no.delay.cs <- filter(airline.cs,arriveDelay.5 == "no")

mean(delay.cs$satisfaction) # mean = 3.167578
mean(no.delay.cs$satisfaction) #mean =3.491515

(3.4915-3.1676)/3.1676

str(airline.cs)
summary(airline.cs$satisfaction)

#for wa - gender + airlinestatus
df.sta.wa <- group_by(airline.wa, gender, airlineStatus)
summarise(df.sta.wa, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#for wa - origin city
df.sta.wa1 <- group_by(airline.wa, originCity)
oricity.wa <- summarise(df.sta.wa1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
oricity.wa$perc <- oricity.wa$count/1688*100
View(oricity.wa)

#for wa - dist city
df.sta.wa2 <- group_by(airline.wa, destCity)
distCity.wa <- summarise(df.sta.wa2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
distCity.wa$perc <- distCity.wa$count/1688*100
View(distCity.wa)

na.wa.del <- airline.wa[is.na(airline.wa$arriveDelayMinute),]
sum(is.na(airline.wa$arriveDelayMinute))
# there are only 3 na in delay.min and they are not cancelled

#gender + airlinestatus
df.sta <- group_by(airline, gender, airlineStatus)
summarise(df.sta, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#travel type
df.typ.cs <- group_by(airline.cs, travelType)
summarise(df.typ.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

airline.cs$travelType
sum(length(airline.cs$originCity))

#gender + num.flightbyYr
df.fl.yr <- group_by(airline, gender, num.flightbyYr)
summarise(df.fl.yr, count = n(), avg_sat = median(satisfaction,na.rm = TRUE))
air.num.flibyYr <- filter(airline, num.flightbyYr <=2.67)  #65780 (50.64%)
air.num.flibyYr2 <- filter(airline, num.flightbyYr >2.67, num.flightbyYr <=5) #32714 (25.19%)
air.num.flibyYr3 <- filter(airline, num.flightbyYr >5, num.flightbyYr <=10) #21263 (16.37%)
air.num.flibyYr4 <- filter(airline, num.flightbyYr >10) #10132 (7.8%)

1509/26058

#1) see avg satis score with different gender groups and num.yrs
df.fl.yr <- group_by(air.num.flibyYr, gender, num.yrs)
airline.yr <- as.data.frame(summarise(df.fl.yr, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE)))
View(airline.yr)
hist(airline.yr$avg_sat, breaks = 10, col ="red")
hist(airline$satisfaction, breaks = 20)

g <- ggplot(airline, aes(x=airline$num.yrs, y=airline$satisfaction))
g <- g+ geom_point()
g

#see avg satis score with different gender groups
df.fl.yr1 <- group_by(air.num.flibyYr, gender)
summarise(df.fl.yr1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#2) see avg satis score with different gender groups and num.yrs
df.fl.yr2 <- group_by(air.num.flibyYr2, gender, num.yrs)
summarise(df.fl.yr2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
#see avg satis score with different gender groups
df.fl.yr3 <- group_by(air.num.flibyYr2, gender)
summarise(df.fl.yr3, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

df.fl.yr10 <- group_by(air.num.flibyYr2, gender, flightCancelled)
summarise(df.fl.yr10, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))


#3) see avg satis score with different gender groups and num.yrs
df.fl.yr4 <- group_by(air.num.flibyYr3, gender, num.yrs)
summarise(df.fl.yr4, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
#see avg satis score with different gender groups
df.fl.yr5 <- group_by(air.num.flibyYr3, gender)
summarise(df.fl.yr5, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#4) see avg satis score with different gender groups and num.yrs
df.fl.yr6 <- group_by(air.num.flibyYr4, gender, num.yrs)
summarise(df.fl.yr6, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
#see avg satis score with different gender groups
df.fl.yr7 <- group_by(air.num.flibyYr4, gender)
summarise(df.fl.yr7, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

airline$NumFlightGr <- NULL
summary(airline$num.flightbyYr)
airline$num.yrs

median(airline$numberOfFlights)
median(airline$num.yrs)
yr.cust <- filter(airline, num.yrs >7 & numberOfFlights > 17)
mean(yr.cust$satisfaction)
yr.cust.1 <- group_by(yr.cust, gender)
summarise(yr.cust.1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

m.all <- lm(formula = satisfaction ~ airlineStatus + age + gender + priceSensitivity + 
     firstYearFlight + numberOfFlights + travelType + shoppingAtAirport + 
     foodAtAirport + class + day + airlineCode + scheduledDepHr + 
     scheduledDepDelay + flightCancelled + flightDistance, data = airline)
summary(m.all)
#Multiple R-squared:  0.4248,	Adjusted R-squared:  0.4247 
str(airline.cs)

m.all.cs <- lm(formula = satisfaction ~ airlineStatus + age + gender + priceSensitivity + 
              firstYearFlight + numberOfFlights + travelType +
              scheduledDepDelay + flightDistance + 
              AgeGroup +loyaltyCardsNum + month, data = airline.cs)
summary(m.all.cs)
#Multiple R-squared:  0.4303,	Adjusted R-squared:   0.43

m.flight.cs <- lm(formula = satisfaction ~ firstYearFlight + numberOfFlights + 
                 flightCancelled + num.flightbyYr + num.yrs, 
               data = airline.cs)
summary(m.flight.cs)
#Multiple R-squared:  0.05605,	Adjusted R-squared:  0.05591

m.flight.wa <- lm(formula = satisfaction ~ firstYearFlight + numberOfFlights + 
                    flightCancelled + num.flightbyYr + num.yrs, 
                  data = airline.wa)
summary(m.flight.wa)
#Multiple R-squared:  0.05605,	Adjusted R-squared:  0.05591

airline.cs$month <- as.Date(airline.cs$date, format = "%m/%d/%Y")

airline.cs.w <- filter(airline.cs, originCity == "Las Vegas, NV" | originCity == "Phoenix, AZ" 
                    | originCity == "Los Angeles, CA" | originCity == "Oakland, CA")
View(airline.cs.w)
summary(airline.cs.w$satisfaction)
summary(airline.cs$satisfaction)
summary(airline.wa$satisfaction)

table(airline.cs.w$satisfaction)
table(airline.cs$satisfaction)

View(table(airline.cs$date))
View(table(airline.wa$date))
View(table(airline$date))
table(airline.cs$flightCancelled)
table(airline.wa$flightCancelled)
table(airline$flightCancelled)

quantile(airline.cs$satisfaction, 0.35)

happycust.cs <- filter(airline.cs, satisfaction >= 3.5)
nrow(happycust.cs)
unhappycust.cs <- filter(airline.cs, satisfaction < 3.5)
mean(happycust.cs$satisfaction)
mean(unhappycust.cs$satisfaction)
hist(happycust.cs$satisfaction)
hist(unhappycust.cs$satisfaction)

elderly.cs <- filter(airline.cs, age >=60)
young.cs <- filter(airline.cs, age< 60)
hist(elderly.cs$satisfaction)
hist(young.cs$satisfaction)
plot(elderly.cs$age, elderly.cs$satisfaction)

plot(happycust.cs$travelType, happycust.cs$satisfaction)

m.flight1 <- lm(formula = satisfaction ~ firstYearFlight + numberOfFlights + 
                 flightCancelled + num.flightbyYr +arriveDelay.min. + age +gender, data = airline)
summary(m.flight1)
#Multiple R-squared:  0.06488,	Adjusted R-squared:  0.06485

m.flight2 <- lm(formula = satisfaction ~  numberOfFlights + 
                   num.flightbyYr + age +gender, data = airline)
summary(m.flight2)
#Multiple R-squared:  0.06488,	Adjusted R-squared:  0.06485

m1 <- lm(formula = jitter(satisfaction) ~ jitter(num.flightbyYr), data = airline)
abline(m1)

plot(airline$num.flightbyYr, jitter(airline$satisfaction))
plot(airline$num.yrs, airline$satisfaction)

#trying out numof flight plot
m1 <- ggplot(airline, aes(x=num.flightbyYr, y=jitter(satisfaction)))
m1 <- m1 + geom_point() + ggtitle("NumOfFlight by Yr w/ Satis")
m1 <- m1 + labs(x = "num.flightbyYr", y = "Satisfaction Score")
m1

airline$num.yrs

m2 <- ggplot(airline, aes(x=numberOfFlights, y=jitter(satisfaction)))
m2 <- m2 + geom_point() + ggtitle("NumOfFlight w/ Satis")
m2 <- m2 + labs(x = "numberOfFlights", y = "Satisfaction Score")
m2

m3 <- ggplot(airline, aes(x=firstYearFlight, y=jitter(satisfaction)))
m3 <- m3 + geom_point() + ggtitle("firstYearFlight w/ Satis")
m3 <- m3 + labs(x = "firstYearFlight", y = "Satisfaction Score")
m3

m4 <- ggplot(airline, aes(x=num.yrs, y=jitter(satisfaction)))
m4 <- m4 + geom_point() + ggtitle("num.yrs w/ Satis")
m4 <- m4 + labs(x = "num.yrs", y = "Satisfaction Score")
m4

m5 <- ggplot(air.num.flibyYr2, aes(x=num.yrs, y=satisfaction))
m5 <- m5 + geom_col() + ggtitle("num.yrs w/ Satis")
m5 <- m5 + labs(x = "num.yrs", y = "Satisfaction Score")
m5

mean(air.num.flibyYr2$satisfaction)
air.num.flibyYr2$m.satis <- mean(air.num.flibyYr2$satisfaction)

write.csv(air.num.flibyYr, 'group1.csv')
write.csv(air.num.flibyYr2, 'group2.csv')
write.csv(air.num.flibyYr3, 'group3.csv')
write.csv(air.num.flibyYr4, 'group4.csv')

hist(airline$num.flightbyYr, breaks = 50)
hist(airline$num.yrs, breaks = 100)

hist(airline$satisfaction, breaks = 20, main= "Overall histogram")
hist(air.num.flibyYr$satisfaction, breaks = 20, main= "Group1 (50%) histogram")
hist(air.num.flibyYr2$satisfaction, breaks = 20, main= "Group2 (25.19%) histogram")
hist(air.num.flibyYr3$satisfaction, breaks = 20, main= "Group3 (16.37%) histogram")
hist(air.num.flibyYr4$satisfaction, breaks = 20, main= "Group4 (7.8%) histogram")

#remove gender - Multiple R-squared:  0.08538,	Adjusted R-squared:  0.08536
#remove age - Multiple R-squared:  0.05687,	Adjusted R-squared:  0.05686

model.numflight <- lm(formula= satisfaction ~ num.flightbyYr, data=airline)
summary(model.numflight)
abline(model.numflight)
#Multiple R-squared:  0.02678,	Adjusted R-squared:  0.02677


m.test <- lm(formula= satisfaction ~ numberOfFlights, data = airline)
summary(m.test)
#Multiple R-squared:  0.05671,	Adjusted R-squared:  0.0567 

m.test10 <- lm(formula= satisfaction ~ gender+ age+numberOfFlights+ num.flightbyYr+ firstYearFlight+num.yrs,
                      data = airline)
summary(m.test10)
#removing age and gender: Multiple R-squared:  0.057,	Adjusted R-squared:  0.05698 
#removing only age: Multiple R-squared:  0.07956,	Adjusted R-squared:  0.07953
#Multiple R-squared:  0.1051,	Adjusted R-squared:  0.1051


airline$firstYearFlight

m.test11 <- lm(formula= satisfaction ~ numberOfFlights+num.yrs+age
               , data = airline)
summary(m.test11)
#Multiple R-squared:  0.08538,	Adjusted R-squared:  0.08536 

m.test1 <- lm(formula= satisfaction ~ numberOfFlights+percentOfOtherFlight+arriveDelay.min.
             , data = airline)
summary(m.test1)
#Multiple R-squared:  0.06439,	Adjusted R-squared:  0.06437 
plot(airline$arriveDelay.min., airline$satisfaction)
plot(airline$numberOfFlights, airline$satisfaction)
plot(airline$day, airline$satisfaction)

m.test2 <- lm(formula= satisfaction ~ numberOfFlights+percentOfOtherFlight+arriveDelay.min.
              +num.yrs+age
              , data = airline)
summary(m.test2)
#Multiple R-squared:  0.09277,	Adjusted R-squared:  0.09273 

m.test3 <- lm(formula= satisfaction ~ numberOfFlights+percentOfOtherFlight+arriveDelay.min.
              +num.yrs+age+priceSensitivity
              , data = airline)
summary(m.test3)
#Multiple R-squared:  0.1027,	Adjusted R-squared:  0.1026 

m.test4 <- lm(formula= satisfaction ~ numberOfFlights+age, data = airline)
summary(m.test4)
#Multiple R-squared:  0.08503,	Adjusted R-squared:  0.08502

m.test4 <- lm(formula= satisfaction ~ percentOfOtherFlight+age, data = airline)
summary(m.test4)
abline(m.test4)
#Multiple R-squared:  0.0494,	Adjusted R-squared:  0.04939


class(airline$flightCancelled)

cran <- sapply(airline$flightCancelled,table)
cran

airline$priceSensitivity

model.1 <- lm(formula= satisfaction ~ age, data=airline)
abline(m.1)

plot(airline$age, airline$satisfaction, col = "blue", main = "Plot age vs. satisfaction", xlab = "Age", ylab = "Satisfaction")
lm(airline$satisfaction ~ airline$age)

abline(lm(airline$Satisfaction ~ airline$Age))



install.packages("dplyr")
library(dplyr)

install.packages("sqldf")
library(sqldf)

#f.airline$Flight.date <- factor("1/10/2014")
#as.Date(f.airline$Flight.date, format = "%m/%d/%Y")
#mean and median of satisfaction by groups

mean(airline$satisfaction)
#overall mean = 3.379

sqldf("select mean(Satisfaction), Gender from f.airline where Gender ='male'")
View(airline)

#create gender group
df.gd <- group_by(airline, gender)
summarise(df.gd, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
#Female 73374    3.27 & Male   56515    3.53

# business questions we need to answer #

#checking which date has the most flights
statis.date <- table(airline$date)
View(statis.date)
df.fre.fl <- group_by(airline, date)
statis.date <- summarise(df.fre.fl, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(statis.date)

df.fre.fl.cs <- group_by(airline.cs, date, month)
statis.date1 <- summarise(df.fre.fl.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(statis.date1)
mean(statis.date1$count)  #avg. 289.5333 flights per day
summary(statis.date1$count)
cs.fre.fl <- filter(statis.date1, count >= 300) # 35 out of 90 days are over avg flights per day
table(cs.fre.fl$month)

df.fre.fl.cs1 <- group_by(airline.cs, month)
statis.date0 <- summarise(df.fre.fl.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(statis.date0)

df.fre.fl.wa <- group_by(airline.wa, date)
statis.date2 <- summarise(df.fre.fl.wa, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(statis.date2)
mean(statis.date2$count) # avg. 18.75556 flights per day

df.fre.fl.wa1 <- group_by(airline.wa, month)
statis.date20 <- summarise(df.fre.fl.wa1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(statis.date20)

#gender + class
df.cl <- group_by(airline, gender, class)
summarise(df.cl, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#airline$day
df.cl1 <- group_by(airline, gender, day)
summarise(df.cl1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#gender + airlinestatus
df.sta <- group_by(airline, gender, airlineStatus)
summarise(df.sta, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#airlinestatus_cs
df.sta.cs <- group_by(airline.cs, airlineStatus)
summarise(df.sta.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#airlinestatus_wa
df.sta.wa <- group_by(airline.wa, airlineStatus)
summarise(df.sta.wa, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#class_cs
df.sta.cs1 <- group_by(airline.cs, class)
summarise(df.sta.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#airlinestatus_wa
df.sta.wa1 <- group_by(airline.wa, class)
summarise(df.sta.wa1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))


#gender + airlinestatus
df.sta1 <- group_by(airline, gender, flightCancelled)
summarise(df.sta1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

df.sta2 <- group_by(airline, gender, flightCancelled, class)
summarise(df.sta2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

airline$destCity
#so we've decided to use atlanta, GA and Chicago, IL b/c they have the highest count.
df.sta3 <- group_by(airline, originCity)
statedf <- as.data.frame(summarise(df.sta3, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE)))
View(statedf)
df.sta4 <- group_by(airline, destCity)
statedf.d <- as.data.frame(summarise(df.sta4, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE)))
View(statedf.d)
airline$airlineCode

#find out the highest avg sat
statedf[which.max(statedf$avg_sat),]
#217 Petersburg, AK    13       4
statedf[which.min(statedf$avg_sat),]
#291 Wilmington, DE    10     2.6

#find out the highest count
statedf[which.max(statedf$count),]
#19 Atlanta, GA  8428 3.39701
#found a missing value
statedf <- statedf[-which.min(statedf$count),]
statedf[which.min(statedf$count),]
#Adak Island, AK     3 2.666667

str(statedf)
summary(statedf)

statedf1 <- filter(statedf, statedf$count > 1000)
statedf2 <- filter(statedf, statedf$count >900)
View(statedf1)
statedf2[order(-statedf2$count),]
statedf2[order(-statedf2$avg_sat),]

min(statedf2$avg_sat)
max(statedf2$avg_sat)

statedf <- statedf[-which.min(statedf$count),]

#try different airline to see
df.sta5 <- group_by(airline, airlineCode, airlineName)
statedf.a <- as.data.frame(summarise(df.sta5, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE)))
View(statedf.a)
statedf.a <- statedf.a[-which.min(statedf.a$count),]
View(statedf.a)

statedf.final <- filter(airline, airline$originCity == "Atlanta, GA"|airline$originCity == "Chicago, IL" )
df.sta6 <- group_by(statedf.final, airlineCode, airlineName)
statedf.a1 <- as.data.frame(summarise(df.sta6, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE)))
View(statedf.a1)

unique(statedf.final$originCity)
unique(statedf.final$destCity)

df.age <- group_by(airline, AgeGroup)
summarise(df.age, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

df.age.cs <- group_by(airline.cs, AgeGroup)
summarise(df.age.cs, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

df.age.wa <- group_by(airline.wa, AgeGroup)
summarise(df.age.wa, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))

#age + class
df.age1 <- group_by(airline, AgeGroup, class)
ac <- summarise(df.age1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(ac)

df.age.cs1 <- group_by(airline.cs, AgeGroup, class)
ac.cs <- summarise(df.age.cs1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(ac.cs)

df.age.wa1 <- group_by(airline.wa, AgeGroup, class)
ac.wa <- summarise(df.age.wa1, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(ac.wa)

#age + airlineStatus
df.age2 <- group_by(airline, AgeGroup, airlineStatus)
ac1 <- summarise(df.age2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(ac1)

df.age.cs2 <- group_by(airline.cs, AgeGroup, airlineStatus)
ac.cs1 <- summarise(df.age.cs2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(ac.cs1)

df.age.wa2 <- group_by(airline.wa, AgeGroup, airlineStatus)
ac.wa1 <- summarise(df.age.wa2, count = n(), avg_sat = mean(satisfaction,na.rm = TRUE))
View(ac.wa1)

#avg satis rate
summary(airline$satisfaction)
summary(airline.cs$satisfaction)
summary(airline.wa$satisfaction)

#filter and make them a new dataframe
str(airline$flightCancelled)
unique(airline$flightCancelled)
m.c1 <- filter(airline,flightCancelled =="No", gender =="Male")
View(m.c1)
mean(m.c1$satisfaction)
summary(m.c1)

str(airline.cs)
ps.cs <- filter(airline.cs, travelType == "Personal Travel")
table(airline.cs$travelType)
table(airline.cs$satisfaction)



ps.all.cs <- lm(formula = satisfaction ~ airlineStatus + age + gender + priceSensitivity + 
                 firstYearFlight + numberOfFlights + arriveDelay.5 +
                 scheduledDepDelay + flightDistance + percentOfOtherFlight +
                 AgeGroup +loyaltyCardsNum + month + shoppingAtAirport +
                 foodAtAirport +class, data = ps.cs)
summary(ps.all.cs)
#Multiple R-squared:  0.4388,	Adjusted R-squared:  0.4369 

plot(ps.cs$loyaltyCardsNum, ps.cs$satisfaction)

m.flight.cs <- lm(formula = satisfaction ~ firstYearFlight + numberOfFlights + 
                    flightCancelled + num.flightbyYr + num.yrs, 
                  data = airline.cs)
summary(m.flight.cs)
#Multiple R-squared:  0.05605,	Adjusted R-squared:  0.05591

yo <- c(5,23,31,44,35,6)
yo[5]

str(airline)
summary(airline)
#airline$satisfaction <- as.numeric(airline$satisfaction, na.rm=TRUE)
View(airline)
View(airline.raw)
str(airline.raw)
length(unique(airline.raw$Satisfaction))
unique(airline.raw$Satisfaction)

#find out which rows have weird value
which(airline.raw$Satisfaction=="4.00.2.00")
#[1] 38899 38900
which(airline.raw$Satisfaction=="4.00.5")
#[1] 38898
sum(is.na(airline.raw$Satisfaction))#making sure there is no NA values in satisfaction
#change to new value
airline.raw[which(airline.raw$Satisfaction=="4.00.2.00"),1] = 4
airline.raw[which(airline.raw$Satisfaction=="4.00.5"),1] = 4
#copy into a new dataframe
airline2 <- airline.raw
#change satisfaction from factor to numeric data
airline2$Satisfaction <- as.numeric(airline2$Satisfaction)

str(airline2)
max(airline2$Satisfaction)
unique(airline2$Satisfaction)

#length(airline$satisfaction)==length(airline$airlineStatus)
airline$year <- format(as.Date(airline$date, format="%d/%m/%y"), "%y")
airline$year <- NULL

