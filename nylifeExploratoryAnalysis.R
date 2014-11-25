# install all the required packages

install.packages("nnet")
install.packages("dplyr")
install.packages("lattice")
install.packages("data.table")

# Set the working directory

setwd("D:\\Infosys\\Capital Markets Practice\\400. Engagements\\2014-11. NY Life Data Strategy\\Survey_R")

# Include the Libaries

library(plyr)
library(dplyr)
library(data.table)
library(lattice)
library(ggplot2)
require(lattice)

# source files

source("..\\Rscripts\\nyLifeKeyFunctions.R")

# Load the CSV file into R

nlNeilsenData = read.csv("Survey.CSV")
ncol(nlNeilsenData)
nrow(nlNeilsenData)

# Run Summary Statistics
names(nlNeilsenData)
summary(nlNeilsenData)
summary(nlNeilsenData$auto_inst.1)
table(nlNeilsenData$state)

abc=as.data.frame(table(nlNeilsenData$state))
top10States = abc[with(abc,order(-Freq)),][1:10,1]

abc=as.data.frame(table(nlNeilsenData$auto_inst.1))
top10Insurers = abc[with(abc,order(-Freq)),][1:10,1]

# get the data related to top insurance companies
idxTopInsurers = nlNeilsenData$auto_inst.1 %in% top10Insurers
nlTopIns = nlNeilsenData[idxTopInsurers,]

# add a dummy variable to make the counting easier
nlTopIns$dummy = 1
sumInsState = ddply(nlTopIns, c("auto_inst.1","state"), summarize, tot = sum(dummy))
ggplot(sumInsState, aes(auto_inst.1,tot)) + geom_point() + 
  facet_wrap( ~ state ) + 
  theme(axis.text.x=element_text(angle=-90)) +
  labs(title = "# of Resp By Ins and State" ) + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  

#  Generate Plots
barchart(tot ~ auto_inst.1 | state,sumInsState)
sumInsIncome = ddply(nlTopIns, c("auto_inst.1","income"), summarize, tot = sum(dummy))
barchart(tot ~ auto_inst.1 | income,sumInsIncome )
ggplot(sumInsIncome, aes(auto_inst.1,tot)) + geom_point() + 
  facet_wrap( ~ income ) + 
  theme(axis.text.x=element_text(angle=-90)) +
  labs(title = "# of Resp By Ins and Income" ) + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# cluster the data and see if there are meaningful differences between the data sets

nlNeilsenDataClean <- na.omit(nlNeilsenData) # listwise deletion of missing
nlNeilsenDataClean <- scale(nlNeilsenDataClean)

# Exploratory Factor Analysis


