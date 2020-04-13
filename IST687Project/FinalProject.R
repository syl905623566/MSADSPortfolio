


survey<-read.csv("spring19survey.csv")
str(survey)
View(survey)

table<-table(survey$Airline.Status)
View(table)
table<-data.frame(table)
table[1,1]
table[2,1]
#make subset only blue
blue<-subset(survey,survey$Airline.Status==table[1,1])
View(blue)

table2<-data.frame(table(blue$Class))
View(table2)
#make subset with business class from blue
bluebusiness<-subset(blue,blue$Class==table2[1,1])
View(bluebusiness)
#make subset with eco class from blue
blueeco<-subset(blue,blue$Class==table2[2,1])
View(blueeco)
#make subset with eco plus from blue
blueecoplus<-subset(blue,blue$Class==table2[3,1])
View(blueecoplus)

install.packages("dplyr")
library(dplyr)

Age<-lm(survey$Satisfaction~survey$Age)
Age
summary(Age)
#significant

Price<-lm(survey$Satisfaction~survey$Price.Sensitivity)
Price
summary(Price)
#significant

FlightPerYear<-lm(survey$Satisfaction~survey$Flights.Per.Year)
summary(FlightPerYear)
#significant

Loyalty<-lm(survey$Satisfaction~survey$Loyalty)
summary(Loyalty)
#significant

S1<-subset(survey,survey$Satisfaction==1)
S2<-subset(survey,survey$Satisfaction==2)
S3<-subset(survey,survey$Satisfaction==3)
S4<-subset(survey,survey$Satisfaction==4)
S5<-subset(survey,survey$Satisfaction==5)
View(S1)
hist(S1$Age, breaks = 4)
hist(S2$Age, breaks = 4)
hist(S3$Age, breaks = 4)
hist(S4$Age, breaks = 4)
hist(S5$Age, breaks = 4)
min(survey$Age)
max(survey$Age)
Age1<-subset(survey, survey$Age<=30)
Age2<-subset(survey, survey$Age<=60 & survey$Age>30)
Age3<-subset(survey, survey$Age<=85 & survey$Age>60)
View(Age2)
hist(Age1$Satisfaction)
hist(Age2$Satisfaction)
hist(Age3$Satisfaction)
mean(Age1$Satisfaction)
#3.324

mean(Age2$Satisfaction, na.rm = TRUE)
#3.63
mean(survey$Satisfaction, na.rm = TRUE)
#3.38

table(Age2$Partner.Name)
max(table(Age2$Partner.Name))
ma<-lm(Age3$Satisfaction~Age3$Age, data=Age3)
summary(ma)
lm<-lm(survey$Loyalty~survey$Age, data=survey)
summary(lm)

A3034<-subset(Age2, Age2$Age<35)
A3539<-subset(Age2,Age2$Age>34&Age2$Age<40)
A4044<-subset(Age2, Age2$Age>39 & Age2$Age<45)
A4549<-subset(Age2, Age2$Age>44 & Age2$Age<50)
A5054<-subset(Age2, Age2$Age>49 & Age2$Age<55)
A5559<-subset(Age2, Age2$Age>54 & Age2$Age<60)
meanage<-c(mean(A3034$Age),mean(A3539$Age),mean(A4044$Age),mean(A4549$Age),mean(A5054$Age),mean(A5559$Age))
meanPriSen<-c(mean(A3034$Price.Sensitivity),mean(A3539$Price.Sensitivity),mean(A4044$Price.Sensitivity),mean(A4549$Price.Sensitivity),mean(A5054$Price.Sensitivity),mean(A5559$Price.Sensitivity))
meanPriSen
meanYrFlight<-c(mean(A3034$Flights.Per.Year),mean(A3539$Flights.Per.Year),mean(A4044$Flights.Per.Year),mean(A4549$Flights.Per.Year),mean(A5054$Flights.Per.Year),mean(A5559$Flights.Per.Year))
meanYrFlight
meanLoyalty<-c(mean(A3034$Loyalty),mean(A3539$Age),mean(A4044$Age),mean(A4549$Age),mean(A5054$Age),mean(A5559$Age))
meanage
meansat<-c(mean(A3034$Satisfaction),mean(A3539$Satisfaction,na.rm = TRUE),mean(A4044$Satisfaction,na.rm = TRUE),mean(A4549$Satisfaction),mean(A5054$Satisfaction,na.rm = TRUE),mean(A5559$Satisfaction))
meansat
agesat<-data.frame(meanage,meansat)
m1<-lm(agesat$meansat~agesat$meanage,data=agesat)
summary(m1)
library(dplyr)
library(RJSONIO)
library(ggplot2)
library(kernlab)
survey<-read.csv("spring19survey.csv")
GroupByStatus<-group_by(survey, survey$Airline.Status)
#Clean NA and non-integer in satisfaction
GroupByStatus<-GroupByStatus[-which(GroupByStatus$Satisfaction==2.5),]
GroupByStatus<-GroupByStatus[-which(GroupByStatus$Satisfaction==3.5),]
GroupByStatus<-GroupByStatus[-which(GroupByStatus$Satisfaction==4.5),]
GroupByStatus<-na.omit(GroupByStatus)
Blue<-subset(GroupByStatus,GroupByStatus$Airline.Status=="Blue")
BlueEco<-subset(Blue,Blue$Class=="Eco")
PN<-table(BlueEco$Partner.Name)
PN<-data.frame(PN)
PN[1,1]
BlueEcoCheapseats<-subset(BlueEco,BlueEco$Partner.Name==PN[1,1])
Cheap<-BlueEcoCheapseats
Cheap$happyCust<-Cheap$Satisfaction>3
dim(Cheap)
dim(Cheap)[1]
RandomIndex<-sample(1:dim(Cheap)[1])
length(RandomIndex)
cutpoint<-floor(2*dim(Cheap)[1]/3)
cutpoint
train<-Cheap[RandomIndex[1:cutpoint],]
View(train)
test<-Cheap[RandomIndex[(cutpoint+1):dim(Cheap)[1]],]

SVM<-ksvm(happyCust ~  Gender + Price.Sensitivity + Flights.Per.Year+Loyalty, data=train,
          kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SVM
svmPred<-predict(SVM,test,type = "votes")
View(svmPred)                 
svmPred<-svmPred[,1]>0.5                 
comTable<-data.frame(test[,31],svmPred)
table(comTable)
percentError <- (table(comTable)[1,2]+table(comTable)[2,1])/nrow(test)
percentError
