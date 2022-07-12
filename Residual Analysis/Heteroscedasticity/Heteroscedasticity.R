#Introduction:

#Heteroscedasticity means unequal scatter. In regression analysis, we talk about heteroscedasticity in the context of the residuals or error term. Specifically, heteroscedasticity is a systematic change in the spread of the residuals over the range of measured values. Heteroscedasticity is a problem because ordinary least squares (OLS) regression assumes that all residuals
#are drawn from a population that has a constant variance (homoscedasticity).

#Aim: To plot the residuals and comment on the assumption of homescedasticity. Also test for heterosedasticity using Breusch Pagan test.

#Data Description: 

#The following table shows the annual consumption and disposable income for 30 households in India. The independant variable is income denoted by varibale 
# X and the dependant variable is expenditure denoted by X. Since there is only one independant variable, we build a simple regression model.

library(readxl)

data=read_excel("C:/Users/Srikar/Desktop/SS/R/Sem 5/Linear Regression/Practical 11/data.xlsx")
head(data)

#Procedure:

#) Building the regression model

mod=lm(data$Expenditure~data$Income,data=data)
summary(mod)

#We observe that Intercept is significant as its p-value is 0.05, the significance level. This means that if income was 0, the avergae expenditure
# will be intercept value. The regressor, income is also significant which means that income explains the variation of expenditur.

#2)Performing  Breusch-Pagan Test.

library(lmtest) 

bptest(mod)

#Since the p-value is less than 0.05, we accept the nullhypothesis.We have sufficient evidence to say that heteroscedasticity is present
#in the regression model. 

#COnclusion:

#The assumption of homoscedascity is fulfilled