library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)

source("..\\Rscripts\\nyLifeKeyFunctions.R")

setwd("D:\\Infosys\\Capital Markets Practice\\400. Engagements\\2014-11. NY Life Data Strategy\\Survey_R")
dfNeilsen = read.csv("Nielsen Translated FINAL_Analysis_ver3_statanalysis.csv")

## Draw a stacked bar chart that add up to 100%
## Change the axis to show the percentages
## Use couple of key dimensions and group by other variables

pdf('test.pdf', width=21, height=27)

g = ggplot(dfNeilsen, aes(x=census_division,fill=as.factor(own_any),position="fill")) + 
     geom_bar(position="fill") +
#     geom_bar() +
#    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90))

g1 =g + facet_wrap( ~ asian_nationality, ncol=2,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

g + facet_wrap( ~ level_of_education, ncol=2,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

g + facet_wrap( ~ marital_status, ncol=2,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

g + facet_wrap( ~ race, ncol=2,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

g + facet_wrap( ~ as.factor(household_size), ncol=2,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

g + facet_wrap( ~ age, ncol=3,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

g + facet_wrap( ~ household_income, ncol=3,scales="free_y") + 
  theme(strip.text.x=element_text(size=8))

## Show the proportions - Draw charts between two categorical variables
## The first stat_sum prints the point which is the default geom for stat_sum
## The second state sum prints the text next to the geom
## Both the stat_sums inherit the group from the original plot. This can be overridden

gg = ggplot(dfNeilsen, aes(x = census_division, group=census_division,y = as.factor(own_any),color=as.factor(own_any))) +
  stat_sum(show_guide=FALSE) + 
  scale_size(range = c(3,10)) +
  stat_sum(aes(label = paste(round(..prop.. * 100,0), "%", sep = "")), 
           geom = "text", size = 3, hjust = -0.6, show_guide = FALSE) +
#  scale_size(range = c(2,8)) +
  theme(axis.text.x=element_text(angle=90))

gg

gg + facet_wrap( ~ level_of_education, ncol=2) + 
  theme(strip.text.x=element_text(size=8))

gg + facet_wrap( ~ marital_status, ncol=2) + 
  theme(strip.text.x=element_text(size=8))

gg + facet_wrap( ~ as.factor(household_size), ncol=2) + 
  theme(strip.text.x=element_text(size=8))

gg + facet_wrap( ~ marital_status, ncol=2) + 
  theme(strip.text.x=element_text(size=8))

gg + facet_wrap( ~ as.factor(q6_employer_life_have_life_ins_purchased_through_employer), ncol=2) + 
  theme(strip.text.x=element_text(size=8))

# Test out the graphs

plotStackedPercChartDiscrete(dfNeilsen,"census_division","gender",c("level_of_education"))

means <- data.frame(means=c(13.8,14.8),condition=1:2)
testplot(means)

p <- ggplot(means, aes(fill=condition, y=means, x = condition))
p + geom_bar(position="dodge", stat="identity")


plotExamples()
printingMultPlots()
printMultPlots2()


plotStackedPercChartDiscreteNew(df,c("cat2","cat1","cat2"))



ggplot(data=subset(dfNeilsen, premiums_from_policies_purchased_in_past_3_years > 0 & own_any == 1),aes(x=as.factor(census_division),y=premiums_from_policies_purchased_in_past_3_years)) +
  geom_boxplot()

table(dfNeilsen$age)

