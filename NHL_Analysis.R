#Brandon Vermeer
#Thesis Code
#Spring 2018
#############################################################

#clear items and install packages
rm(list=ls()) 
install.packages("car")
library("car")

#set working directory
getwd() 
setwd("/Users/BrandonVermeer/Documents/Thesis/NHL_Team_Stats")   

#Load 2015-2016 Season Team Data
Season12_Teams <- read.csv("16-17Teams.csv")
head(Season12_Teams)
str(Season12_Teams)
dim(Season12_Teams) 
attach(Season12_Teams)

#Wins vs Goals Allowed 2016-2017
WinsVsGA12 <- lm(W ~ GA)
summary(WinsVsGA12)
plot(WinsVsGA12)

#Wins vs Goals For 2016-2017
WinsVsGF12 <- lm(W ~ GF)
summary(WinsVsGF12)
plot(WinsVsGF12)

#Wins vs Goals For + Goals Against 2016-2017
WinsVsGF12andGA12 <- lm(W ~ GF + GA)
summary(WinsVsGF12andGA12)
plot(WinsVsGF12andGA12)
predict(WinsVsGF12andGA12)

#W=GF+GA Residuals
r <- residuals(WinsVsGF12andGA12)
residualPlots(WinsVsGF12andGA12)
qqnorm(r)
qqline(r)
shapiro.test(residuals(WinsVsGF12andGA12))

#Load Corsi Based Data
Season12_Corsi <- read.csv("16-17Corsi.csv")
attach(Season12_Corsi)

#Create Corsi based modles
WinsVsCorsi <- lm(Wins ~ Corsi)
summary(WinsVsCorsi)
WinsVsCorsiAVG <- lm(Wins ~ CorsiAVG)
summary(WinsVsCorsiAVG)

#Load data for MAX models
Season12_MAX <- read.csv("16-17WOTeams.csv")
attach(Season12_MAX)

#Create MAX Model
max.mod <- lm(W ~ ., Season12_MAX)
summary(max.mod)
step.mod <- step(max.mod)
summary(step.mod)

#Load data for All Season Regressions
AllSeasons <- read.csv("AllSeasons.csv")
attach(AllSeasons)

#Create all season regressions
AllSeasonsGFandGA <- lm(WA ~ GFA + GAA)
summary(AllSeasonsGFandGA)
plot(AllSeasonsGFandGA)

#Residuals for plot
r2 <- residuals(AllSeasonsGFandGA)
residualPlots(AllSeasonsGFandGA)
qqnorm(r2)
qqline(r2)