## Group Assignment Advanced Statistics Group 10
## Problem 1 
## Dataset : Cereal   
## Purpose:A subset of the data collected by Roberts and Lattin, reflecting the evaluations of the 12 most frequently cited
## cereal brands in the sample (in the original study, a total of 40 different brands were evaluated by 121 respondents, 
## but the majority of brands were rated by only a small number of consumers). 
## The 25 attributes and 12 brands are listed below
## In total 116 respondents provided 235 observations of the 12 selected brands. 
## How do you characterize the consideration behaviour of the 12 selected 
## brands? Analyze and interpret your results using factor analysis. 
## Likert Scale :
##6:Extremely satisfied
##5:Very satisfied
##4 Somewhat satisfied
##3 Somewhat dissatisfied
##2 Very dissatisfied
##1 Extremely dissatisfied/#

## EDA 
getwd()

cereal = read.csv(file="D:/Study/Great Lakes/advanced analytics/Group Assignment/cereal.csv",header=T)
str(cereal)
summary(cereal)
##sum(complete.cases(cereal[2:26]))  ## no of missing cases
##sum(complete.obs(cereal[2:26]))  ## no of missing cases
class(cereal)
boxplot(cereal[2:26],notch = TRUE,  col = "Green")
hist(cereal$Filling)
plot(density(cereal$Filling))


## FA : cereal data 
## Itereation 1
##cereal1 = cereal[c(1:16,18:34,36:44)]
cortest.bartlett(cereal[2:26])  ## p value =0
KMO(cereal[2:26])  ## overall msa =.84 per44 .69 min
scree(cereal[2:26])  ## 4 factors
fa1.out = fa(cereal[2:25], nfactors=4, fm="pa", rotate = "varimax")
fa.diagram(fa1.out)
fa1.out$communality  ## communality check
v1 = fa1.out$communality
sort(v1)
print(fa1.out$Structure, digits = 3)
fa1.out$e.values[1:4]
100*fa1.out$e.values[1:4]/length(fa1.out$e.values)
sum(100*fa1.out$e.values[1:4]/length(fa1.out$e.values))  ## 57.507

## Iteration 2 :  removing Easy(Col 6) and Process(22)
cereal2 = cereal[c(2:5,7:21,23:26)]
cortest.bartlett(cereal2)  ## p value =0
KMO(cereal2)  ## overall msa =.86 
scree(cereal2)  ## 4 factors
fa2.out = fa(cereal2, nfactors=4, fm="pa", rotate = "varimax")
fa.diagram(fa2.out)
fa2.out$communality  ## communality check
v1 = fa2.out$communality
sort(v1)
print(fa2.out$Structure, digits = 3)
fa2.out$e.values[1:4]
100*fa2.out$e.values[1:4]/length(fa2.out$e.values)
sum(100*fa2.out$e.values[1:4]/length(fa2.out$e.values))    ## 61.21

## Iteration 3 : removing soggy and economical
cereal3 = cereal[c(2:5,7:11,14:21,23:26)]
cortest.bartlett(cereal3)  ## p value =0
KMO(cereal3)  ## overall msa =.86 
scree(cereal3)  ## 3 factors
fa3.out = fa(cereal3, nfactors=3, fm="pa", rotate = "varimax")
fa.diagram(fa3.out)
fa3.out$communality  ## communality check
v1 = fa3.out$communality
sort(v1)
print(fa3.out$Structure, digits = 3)
fa3.out$e.values[1:3]
100*fa3.out$e.values[1:3]/length(fa3.out$e.values)
sum(100*fa3.out$e.values[1:3]/length(fa3.out$e.values))    ## 57.44


## Iteration 4 : removing Plain and Boring
cereal4 = cereal[c(2:5,7:11,14:16,18:21,23:24,26)]
cortest.bartlett(cereal4)  ## p value =0
KMO(cereal4)  ## overall msa =.86 
scree(cereal4)  ## 3 factors
fa4.out = fa(cereal4, nfactors=3, fm="pa", rotate = "varimax")
fa.diagram(fa4.out)
fa4.out$communality  ## communality check
v1 = fa4.out$communality
sort(v1)
print(fa4.out$Structure, digits = 3)
fa4.out$e.values[1:3]
100*fa4.out$e.values[1:3]/length(fa4.out$e.values)
sum(100*fa4.out$e.values[1:3]/length(fa4.out$e.values))    ## 61.16


## Iteration 5 : removing crisp and fruit
cereal5 = cereal[c(2:5,7:11,14:16,19:20,23:24,26)]
cortest.bartlett(cereal5)  ## p value =0
KMO(cereal5)  ## overall msa =.87
scree(cereal5)  ## 3 factors
fa5.out = fa(cereal5, nfactors=3, fm="pa", rotate = "varimax")
fa.diagram(fa5.out)
fa5.out$communality  ## communality check
v1 = fa5.out$communality
sort(v1)
print(fa5.out$Structure, digits = 3)
fa5.out$e.values[1:3]
100*fa5.out$e.values[1:3]/length(fa5.out$e.values)
sum(100*fa5.out$e.values[1:3]/length(fa5.out$e.values))    ## 64.46


## Iteration 6 : removing variables fun and salt
cereal6 = cereal[c(2:5,8:9,11,14:16,19:20,23:24,26)]
cortest.bartlett(cereal6)  ## p value =0
KMO(cereal6)  ## overall msa =.87
scree(cereal6)  ## 3 factors
fa6.out = fa(cereal6, nfactors=3, fm="pa", rotate = "varimax")
fa.diagram(fa6.out)
fa6.out$communality  ## communality check
v1 = fa6.out$communality
sort(v1)
print(fa6.out$Structure, digits = 3)
fa6.out$e.values[1:3]
100*fa6.out$e.values[1:3]/length(fa6.out$e.values)
sum(100*fa6.out$e.values[1:3]/length(fa6.out$e.values))    ## 67.34


## Iteration 7 : removing variables treat and regular
cereal7 = cereal[c(2:5,8:9,11,14:16,20,23,26)]
cortest.bartlett(cereal7)  ## p value = 1.536968e-306 no enough correlation between the variables
##KMO(cereal7)  ## overall msa =.86
##scree(cereal7)  ## 2 factors
##fa7.out = fa(cereal7, nfactors=2, fm="pa", rotate = "varimax")
##fa.diagram(fa7.out)
##fa7.out$communality  ## communality check
##v1 = fa7.out$communality
##sort(v1)
##print(fa7.out$Structure, digits = 3)
##fa7.out$e.values[1:2]
##100*fa7.out$e.values[1:2]/length(fa7.out$e.values)
##sum(100*fa7.out$e.values[1:2]/length(fa7.out$e.values))    ## 67.34



## Problem 3
## All Greens Franchise 
##Explain the importance of X2, X3, X4, X5, X6 on Annual Net Sales, X1. The data (X1, X2, X3, X4, X5, X6) are for each franchise store.
##X1 = annual net sales/$1000 X2 = number sq. ft./1000 X3 = inventory/$1000
##X4 = amount spent on advertising/$1000 X5 = size of sales district/1000 families
##X6 = number of competing stores in district

##The final deliverable should include:
##  .	Detailed Exploratory Data Analysis 
##  .	Using multiple ways to identify the relationship among all the variables along with explanations of their 
##    importance 


## 







