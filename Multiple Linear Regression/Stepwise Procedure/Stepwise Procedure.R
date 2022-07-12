#Introduction: 

#Multiple regression is a statistical technique that can be used to analyze the relationship between a single dependent variable and several independent variables. The objective of multiple regression analysis is to use the independent variables whose values are known to predict the value of the single dependent value. Each predictor value is weighed, 
#the weights denoting their relative contribution to the overall prediction.

#R-squared (R2) is a statistical measure that represents the proportion of the variance for a dependent variable that's explained by an independent variable
#The P value is defined as the probability under the assumption of no effect or no difference (null hypothesis), of obtaining a result equal to or more extreme than what was actually observed. The P stands for probability and measures how likely it is that 
#any observed difference between groups is due to chance.

#Stepwise regression is the step-by-step iterative construction of a regression model that involves the selection of independent variables to be used in a final model. It involves adding or removing potential explanatory variables in succession and 
#testing for statistical significance after each iteration

#Objective: 

#1) To test the regressors using stepwise procedure
#2) To predict the weight of the fish using the given regressors

#Data Description:

#This dataset is a record of a certain  type of fish known as Perch Fish. It contains 5 independent variables and 1 dependent variable i.e. the weight of the 
#fish (in grams). The dependent variables are type of species, vertical length (in cms),diagonal length (in cms), cross length (cms), height and width (in cms),
#The dataset contains a sample of 50 fishes in a market.

library(readxl)
dat<- read_excel("C:/Users/Srikar/Desktop/SS/R/Sem 5/Linear Regression/Practical 5/Data.xlsx")
head(dat,10)

names(dat)

#Here the column names represent the following:

#Length1 - Vertical Length
#Length2 - Diagonal Length
#Length3 - cross Length
#height - Height 
#Width - Width

#Procedure:

#1) Visualizng the correlation matrix

r=cor(dat)

library(ggplot2)
library(ggcorrplot)
ggcorrplot(r, hc.order = TRUE, type = "lower",lab = TRUE)

#We can observe that all the variables have high correlationship with each other and especially with the dependent variable.
#The coorealtion value is above 0.90 which shows high correlation with the dependent variable, Weight.

#2) Constructing the regression model

mod=lm(dat$Weight~.,data=dat)

#The model obtained is:

#) Y=-515.24+10.53X1+ 102.73X2+-98.25X3+49.49X4+ 79.76X5

#where X1,X2,X3,X4 and X5 are Length1,Length2,Length3,height and Width respectively.


summary(mod)

#We see that that range of residuals is large which means that values (in grams) will differ from the observed value.

#We observe that the intecept p-value is below the significance value (0.05) and hence we can say that the intecept is significant in the prediction.
#This means that if the values of the regressors were all zero, the intercept would tell us the mean estimate of the dependent variable (Weight).Since
#height, width and length of fish can't be 0, the intecept value has no real meaning.

#The p-values of all the regressors except length1 show significance. That means these variables describe the linear relationship with the 
#independent variable. Since most of the variables show significance, we can say that model is a good fit.Also the overall 
#p-value is also lesser than significane level (0.05) and hence we can say that the model is a good fit.

#The R-squared value 0.9574 which means that 95.74% of the variation is explained by the regressors. The adjusted R-Squared 
#shows the variation explained by the regressors that truly contribute to the known variation.

#To find out which varibales really contribute to the the model, we can test it out by forward selection method.

#3)Using stepwise procedure to find the best regressors

step(mod, direction="both")


#We observe that only Width, lenght2 and height and length3 truly give the estimate values. Using only these variables and excluding
#length1, we will construct a new model.

nmod=lm(dat$Weight~dat$Length2+dat$Length3+dat$Height+dat$Width)

#The new model obtained is:

#Y=-521.02+112.24X1+-96.94X2+47.98X3+ 76.55X4

#where X1,X2,X3 and X4 are Length2,Length3,height and Width respectively.

summary(nmod)

#We see that all variables are now significant . The r-squared has also increased since we removed an insignificant factor.


#4)Constructing the test data for prediction 

set.seed(123)
x1<-rnorm(50,29.41)
x2=rnorm(50,33.846)
x3=rnorm(50,12.47)
x4<-rnorm(50,4.80)
df<-data.frame(x1,x2,x3,x4)

A=predict(nmod,df)
df1=data.frame(dat$Weight,A,dat$Weight-A) 
head(df1,10)

#From this table, we can compare the observed and expected values are close to each other.As the R-sqaured value explains only
#95.36 of the variation. The rest of the variation is explained by chance or unknow causes.

#Conclusion:

#The newly obtained model which is the best fit for predicting the weight of the fish is :

#Y=-521.02+112.24X1+-96.94X2+47.98X3+ 76.55X4 with an R-squared value of 95.36 of the known variation.
