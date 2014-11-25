# source required packages

install.packages("nnet")
install.packages("dplyr")
install.packages("lattice")
install.packages("XLConnect")
install.packages("ClustOfVar")
install.packages("gridExtra")
install.packages("car")
install.packages("data.table")
install.packages("stringr")
install.packages("leaps")

# Include the Libaries

library(plyr)
library(data.table)
library(lattice)
library(ggplot2)
library(XLConnect)
library(gdata)
library(ClustOfVar)
library(scales)
library(gridExtra)
library(car)
library(stringr)
library(leaps)

# Turn Warning Off

options(warn=-1)
#options(warn = 0)

# Set working Directory

setwd("..\\Survey_R")

# Source all the required Files that carry key functions

source("..\\Rscripts\\nyLifeKeyFunctions.R")

# Load the file into a data frame

dfNeilsen = read.csv("Nielsen Translated FINAL_Analysis_ver3_statanalysis.csv")

#### All the transformations requiredm 

# Add a column to do transformations

dfNeilsen$cnt = 1

## Add Attributes that are New York Life

dfNeilsen$term_life_nyl=(dfNeilsen$trmlf_inst_1_institution_name %in% "New York Life") * 1
dfNeilsen$univ_life_nyl=(dfNeilsen$unilf_inst_1_institution_name %in% "New York Life") * 1
dfNeilsen$var_life_nyl=(dfNeilsen$varlf_inst_1_institution_name %in% "New York Life") * 1
dfNeilsen$Whole_life_nyl=(dfNeilsen$whlf_inst_1_institution_name %in% "New York Life") * 1
dfNeilsen$res_life_nyl=(dfNeilsen$res_inst_1_institution_name %in% "New York Life") * 1
dfNeilsen$nyl_ins = ( dfNeilsen$term_life_nyl + dfNeilsen$univ_life_nyl + dfNeilsen$var_life_nyl + dfNeilsen$Whole_life_nyl + dfNeilsen$res_life_nyl > 0 ) * 1
dfNeilsen$revLavish = ( dfNeilsen$q6c_own_golf_cart + dfNeilsen$q6c_own_recreational_vehicle_rv + dfNeilsen$q6c_own_personal_watercraft > 0 ) * 1

## Recompute the flags based on the available data

dfNeilsen$own_term=(str_trim(dfNeilsen$trmlf_inst_1_institution_name)!="")* 1
dfNeilsen$own_wl=(str_trim(dfNeilsen$whlf_inst_1_institution_name)!="")* 1
dfNeilsen$own_ul=(str_trim(dfNeilsen$unilf_inst_1_institution_name)!="")* 1
dfNeilsen$own_vl=(str_trim(dfNeilsen$varlf_inst_1_institution_name)!="")* 1
dfNeilsen$own_auto=(str_trim(dfNeilsen$auto_inst_1_institution_name)!="")* 1
dfNeilsen$own_res=(str_trim(dfNeilsen$res_inst_1_institution_name)!="")* 1
dfNeilsen$own_any=(dfNeilsen$own_term + dfNeilsen$own_wl + dfNeilsen$own_ul + dfNeilsen$own_vl + dfNeilsen$own_auto + dfNeilsen$own_res > 0 )* 1
dfNeilsen$own_lifeins=(dfNeilsen$own_term + dfNeilsen$own_wl + dfNeilsen$own_ul + dfNeilsen$own_vl  > 0 )* 1

# recode some existing Variable

dfNeilsen$revAge=recode(dfNeilsen$age,"
    'Under 20' = '1 - Under 20' ;
    c('20-24','25-29','30-34','35-39')='2 - 20 and 40' ; 
    c('40-44','45-49','50-54','55-59')='3 - 40 and 60' ; 
    c('60-64','65-69','70-74','75-79')='4 - 60 and 80' ; 
    c('25-29','30-34','35-39')='5 - 20 and 40' ;
    '80 or Older' = '6 - 80 and Above'
                      
")

dfNeilsen$revIncome=recode(dfNeilsen$household_income, "
  	c('Under $10000') = '1 - Under 10k' ;
		c('$10000-$14999','$15000-$19999','$20000-$24999','$25000-$29999') = '2 - 10 to 30k' ;
		c('$30000-$34999','$35000-$39999','$40000-$49999','$50000-$59999') = '3 - 30 to 60k' ;
		c('$60000-$74999','$75000-$99999') = '4 - 60 to 100k' ;
		c('$100000-$124999','$125000-$149999') = '5.100 to 150k' ;
		c('$150000-$199999','$200000-$249999','$250000-$299999') = '6 - 150 to 300k' ;
		c('$300000-$399999','$400000-$499999') = '7 - 300 to 400k' ;
		c('$500000 or more') = '8 - 500k and above' ;
		c('Don\\'t know/Prefer not to say') = '8 - not answered' 
		"
)

# Recode all the variables that are starting with a "No"

recVect=c('q47_agent_works_for_single_company','q45_recommendation_from_agent','q45_part_of_investment_portfolio','q45_income_protection','q45_alternative_to_mortgage_insurance','q45_pay_for_funeral_expenses_final_expenses','q45_other','q45_recommendation_from_friend','q45_married_divorced','q45_had_a_child','q45_employment_status_changed','q45_to_change_a_feature_on_an_existing_policy','q45_obtained_better_rates_from_new_insurer','q45_ease_inheritance_burdens','q45_part_of_retirement_planning','q61_recommendation_from_agent','q61_part_of_investment_portfolio','q61_income_protection','q61_alternative_to_mortgage_insurance','q61_pay_for_funeral_expenses_final_expenses','q61_other','q61_recommendation_from_friend','q61_married_divorced','q61_had_a_child','q61_employment_status_changed','q61_to_change_a_feature_on_an_existing_policy','q61_obtained_better_rates_from_new_insurer','q61_ease_inheritance_burdens','q61_part_of_retirement_planning','q69_recommendation_from_agent','q69_part_of_investment_portfolio','q69_income_protection','q69_alternative_to_mortgage_insurance','q69_pay_for_funeral_expenses_final_expenses','q69_other','q69_recommendation_from_friend','q69_married_divorced','q69_had_a_child','q69_employment_status_changed','q69_to_change_a_feature_on_an_existing_policy','q69_obtained_better_rates_from_new_insurer','q69_ease_inheritance_burdens','q69_part_of_retirement_planning','q55_agent_works_for_single_company','q53_recommendation_from_agent','q53_part_of_investment_portfolio','q53_income_protection','q53_alternative_to_mortgage_insurance','q53_pay_for_funeral_expenses_final_expenses','q53_other','q53_recommendation_from_friend','q53_married_divorced','q53_had_a_child','q53_employment_status_changed','q53_to_change_a_feature_on_an_existing_policy','q53_obtained_better_rates_from_new_insurer','q53_ease_inheritance_burdens','q53_part_of_retirement_planning')
dfNeilsenExtend = recodeValuesByGrep ( dfNeilsen, recVect, "no")

dfNeilsen = cbind(dfNeilsen,dfNeilsenExtend )

test = t(dfNeilsen[, as.character(c("auto_inst_1_institution_name", 
                       "res_inst_1_institution_name", 
                       "unilf_inst_1_institution_name",
                       "varlf_inst_1_institution_name",
                       "whlf_inst_1_institution_name",
                       "trmlf_inst_1_institution_name")
)])

### Run sapply and convert it back to data frame and add it to the original data frame
test = as.data.frame(test)
k=data.frame(uniqElem=sapply(test,fnUniqueElements))
dfNeilsen$uniqInst = k[,1]

### Find out the number of companies with whom they have life insurance
### Leveraging the same code as above

test = t(dfNeilsen[, as.character(c( "unilf_inst_1_institution_name",
                                    "varlf_inst_1_institution_name",
                                    "whlf_inst_1_institution_name",
                                    "trmlf_inst_1_institution_name")
)])

### Run lapply and convert it back to data frame and add it to the original data frame
test = as.data.frame(test)
k=data.frame(uniqElem=sapply(test,fnUniqueElements))
dfNeilsen$uniqLifeIns = k[,1]

dfCategories = read.csv("NeilsenDataCategories.csv")

# Understand the Quality of the data

nlDQIssues=ddply(dfNeilsen, "state_of_residence", colwise(fnNumMissing))


## Perform Exploratory Data Analysis and Create Copius Graphs

graphDemographicsParams = c('uniqInst','uniqLifeIns','revAge','revIncome','revLavish','asian_nationality','census_division','level_of_education','number_employed_full_time','gender','spanish_hispanic_or_latino_descent','household_size','marital_status','occupation','business_owner','race','state_of_residence','when_plan_to_retire')

createCombinationsAndPrintGraphs_twoVar (dfNeilsen,graphDemographicsParams,"own_any",1,"demoOneVarVsOwnAny.pdf")
createCombinationsAndPrintGraphs_threeVar (dfNeilsen,graphDemographicsParams,"own_any",2,"demoTwoVarVsOwnAny.pdf")

createCombinationsAndPrintGraphs_twoVar (dfNeilsen,graphDemographicsParams,"own_lifeins",1,"demoOneVarVsOwnLifeIns.pdf")
createCombinationsAndPrintGraphs_threeVar (dfNeilsen,graphDemographicsParams,"own_lifeins",2,"demoTwoVarVsOwnLifeIns.pdf")

## Compare All Insurance Graphs together

createCombinationsAndPrintGraphs_twoVar_CompIns (dfNeilsen,graphDemographicsParams,"own_lifeins",1,"demoOneVarCompareIns.pdf")

## Generate Other Graphs

createCombinationsAndPrintGraphs_twoVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"nyl_ins",1,"demoOneVarVsNylIns.pdf")
createCombinationsAndPrintGraphs_threeVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"nyl_ins",2,"demoTwoVarVsNylIns.pdf")

createCombinationsAndPrintGraphs_twoVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_term",1,"demoOneVarVsOwnTerm.pdf")
createCombinationsAndPrintGraphs_threeVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_term",2,"demoTwoVarVsOwnTerm.pdf")

createCombinationsAndPrintGraphs_twoVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_wl",1,"demoOneVarVsOwnWl.pdf")
createCombinationsAndPrintGraphs_threeVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_wl",2,"demoTwoVarVsOwnWl.pdf")

createCombinationsAndPrintGraphs_twoVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_vl",1,"demoOneVarVsOwnVl.pdf")
createCombinationsAndPrintGraphs_threeVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_vl",2,"demoTwoVarVsOwnVl.pdf")

createCombinationsAndPrintGraphs_twoVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_ul",1,"demoOneVarVsOwnUl.pdf")
createCombinationsAndPrintGraphs_threeVar (subset(dfNeilsen,own_lifeins == 1),graphDemographicsParams,"own_ul",2,"demoTwoVarVsOwnUl.pdf")

## Test Functions Out

plotStackedPercChartDiscreteNoFaceting (dfNeilsen,c('revAge','revIncome'))
plotStackedPercChartDiscreteCompIns(dfNeilsen,c('revAge','own_any'))

## Test Create Bivariate Charts

printCharts2Var(dfNeilsen,"auto_inst_1_institution_name","whlf_inst_1_institution_name",-90,1 )
printCharts2Var(dfNeilsen,"q6c_own_golf_cart","own_any",-90, 0 )
printCharts2Var(dfNeilsen,"own_any","state_of_residence",90, 1 )
printCharts2Var(dfNeilsen,"own_any","auto_premp_1_q9_auto_annual_policy_premium",90, 1 )
printCharts2Var(dfNeilsen,"premiums_from_policies_purchased_in_past_3_years","auto_lngthp_1_q10_auto_how_long_with_continuous_coverage",90, 1 )
printCharts2Var(dfNeilsen,"connexions_lifestage_group","own_any",90, 1 )

ggplot(data=dfNeilsen, aes(uniqInst)) + geom_histogram(binwidth=1) +
  labs(list(title = "Neilsen Data Chart of two variables",x="Num Ins Comp",y="Freq" )) +
  facet_grid ( q4_auto_have_auto_insurance  ~ own_term )


### Generate Linear Models #######################

library(leaps)
regsubsets.out <-
  regsubsets(own_lifeins ~ revAge + revIncome + revLavish + asian_nationality + census_division + level_of_education + number_employed_full_time + gender + spanish_hispanic_or_latino_descent + household_size + marital_status + occupation + business_owner + race + state_of_residence + when_plan_to_retire,
             data = dfNeilsen,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "forward",
             really.big=FALSE)
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

regsubsets.out = regsubsets(own_lifeins ~ revAge + revIncome + revLavish + level_of_education + gender +  marital_status + race,
           data = dfNeilsen,
           nbest = 1,       # 1 best model for each number of predictors
           nvmax = NULL,    # NULL for no limit on number of variables
           force.in = NULL, force.out = NULL,
           method = "exhaustive",
           really.big=FALSE)
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
which.max(summary.out$adjr2)

summary.out$which[24,]




   
