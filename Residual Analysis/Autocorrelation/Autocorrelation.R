#Introduction:

#Autocorrelation refers to the degree of correlation between the values of the same variables across different observations in the data
# For example, one might expect the air temperature on the 1st day of the month to be more similar to the temperature on the 2nd day compared to the 31st day.  If the temperature values that occurred closer together in time are, in fact, more similar than the temperature values that occurred farther apart in time, 
#the data would be autocorrelated.Autocorrelation might mostly refers to either autocorrelation in errors, or also more generally to time series models where variables are related to their past realizations.
#When it comes to autocorrelation in error term then it means that different errors are correlated with each other (usually across time but spatial autocorrelation can sometiems exists as well).

#The differene between multicolinearity and autocorrelation are that Autocorrelation refers to a correlation between the values of an independent variable, 
#while multicollinearity refers to a correlation between two or more independent variables.

#Aim: i) To fit a regression model of data
#     ii) To check the assumption of no autocorrelation

#Data Description:

#The dataset contains the information about the purity of oxygen produced by a fractionation process is thought to be related to the percentages of
#hydrocarbons in the main condenser of the processing unit. The dataset contains 20 observations sampled from a population.
#Here the two varibales are Purity (given in %) and Hydrocarbon (given in %). The independent varibale (X) is hydrocarbon which is thought to 
#be effecting the production of pure oxygen.

library(readxl)
data <- read_excel("C:/Users/Srikar/Desktop/SS/R/Sem 5/Linear Regression/Practical 10/1940834_Practical 10.xlsx")
head(data)

#Hypothesis Statement

#Null Hypothesis- Errors are not autocorrelated
#Alternate Hypothesis- Errors are autocorrelated


#Procedure:

#1)Constructing the regression Model

mod=lm(data$Purity~data$Hydrocarbon)
summary(mod)


#We obtain the model :

# Y = 77.863+ 11.801*X where

# Y is the purity of oxygen obtained and X is the amount of hydrocarbons present.


#We observe that the intecept p-value is below the significance value (0.05) and hence we can say that the intecept is significant in the prediction.
#This means that if the values of the regressors were all zero, the intercept would predict the average purity of oxygen if theere were
#no hydrocarbon present at all

#The p-values of the regressor Hydrocarbons shows significance as its below 0.05. That means these variables describe the linear relationship with the 
#independent variable. .Also the overall p-value is also lesser than significane level (0.05) and hence we can say that the model is significant.

#The R-squared value is 0.3891 which means that only 38.91% of the variation of the Y variable (purity of oxygen) is explained by 
# X varibale (Hydrocarbon)

#2)Checking the assumptions of multicollinearity

require(stats)

#Using the Durbin-Watson test to check for autocorrelation for errors

library(lmtest)

dwtest(data$Purity~data$Hydrocarbon)


#Since the p-value is greater than 0.05 (significance level), we accept the null hypothesis and say that there is no autocorrelation 
#between the errors or the residuals.

#Conclsuion:

#1) We obtain the regression model : # Y = 77.863+ 11.801*X where

# Y is the purity of oxygen obtained and X is the amount of hydrocarbons present. The model is not ideal fit as the independant variable hardly
#explains the depedant variable.

#2) The assumption of no autocorrealtion of errors has been fulfilled. This means that we can proceed further with the experiment



     