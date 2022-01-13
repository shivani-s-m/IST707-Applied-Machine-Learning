#Homework 3

library(dplyr)
library(arules)
library(arulesViz)

bankdata <- read.csv("C:/Users/shiva/Downloads/bankdata_csv_all.csv")
str(bankdata)

#dropping ID columns 
bd <- bankdata[-1]
head(bd)

#Converting the sex, region, married, children, car, save_acct, current_act, mortgage and pep column into factors
bd$sex <- as.factor(bd$sex)
bd$region <- as.factor(bd$region)
bd$married <- as.factor(bd$married)
bd$children <- as.factor(bd$children)
bd$car <- as.factor(bd$car)
bd$save_act <- as.factor(bd$save_act)
bd$current_act <- as.factor(bd$current_act)
bd$mortgage <- as.factor(bd$mortgage)
bd$pep <- as.factor(bd$pep)

#Discretizing the age columns in appropriate age groups

bd$age <- cut(bd$age, breaks = c(0,12,18,25,44,60,Inf),labels=c("child","teens","young-adult", "middle-aged-adult" , "old-aged-adult", "senior"))
bd$age
#Discretizing the income column into 4 categories: "lower class", "lower middle class", "upper middle class", "upper class"

bd$income <- cut(bd$income, 4, labels = c("lower class", "lower middle class", "upper middle class", "upper class"))
bd$income
bd <- as(bd,"transactions")
inspect(bd)
rules <- apriori(bd, parameter = list(supp = 0.04, conf = 0.9, maxlen=3))
inspect(rules[1:50])
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(rules_conf)
rules1<-apriori(data=bd, parameter=list(supp=0.04,conf = 0.9,minlen=3), 
               appearance = list(default="lhs",rhs=('pep=YES')),
               control = list(verbose=F))
inspect(rules1)
