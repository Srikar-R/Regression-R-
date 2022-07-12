#Introduction:Multicollinearity is the phenomenon when a number of the explanatory variables are strongly correlated.
#f two independent variables are correlated, they explain the same information. The model will not be able to know which of the two variables is actually responsible for a 
#change in the dependent variable.test for multicollinearity problems using the Variance Inflation Factor, or VIF in short. The VIF indicates for an independent variable how much it is correlated 
#to the other independent variables

#VIF starts from 1 and has no upper limit. A VIF of 1 is the best you can have as this indicates that there is no multicollinearity for this variable. A VIF of higher than 5 or 10 indicates that 
#there is a problem with the independent variables in your model.

#Objective: To find if there is multicolinearity in the dataset between the independent varibales

#Data Descrption: The National Family Health Surveys (NFHS) programme, initiated in the early 1990s, has emerged as a nationally important source of data on population, health, and nutrition for India and its states
#NFHS-3 was designed to provide estimates of important indicators on family welfare, maternal and child health, and nutrition.
#The dataset provides 10 dependent variables that help in finding out the total fertility rate(TFR)

#The data file contains 29 observations on 11 variables sampled from NFHS 2005-06.
#Y=TFR
#X1=HDI
#X2=Infant mortality rate
#X3=contraceptive use(any method)
#X4=Female Age at marriage
#X5=Median number of months since preceding the birth
#X6=female literacy in percentage
#X7=maternal care
#X8=Male age at marriage
#X9=percent of population with improved water supply
#X10=male literacy in percentage

library(readxl)
dat=read_excel("C:/Users/Srikar/Desktop/SS/R/Sem 5/Linear Regression/Practical 9/data.xlsx")
head(dat)


#Procedure:

#1) Building the regression model

mod=lm(y~.,data=dat)
summary(mod)

#We see that that range of residuals is slighlty arge which means that the values will differe a bit more from the observed (y)
#value

#We observe that the intecept p-value is below the significance value (0.05) and hence we can say that the intecept is significant in the prediction.
#This means that if the values of the regressors were all zero, the intercept would tell us the mean estimate of the dependent variable (Total fertility).
#rate).SInce age can not be 0, intercept has no real meaning.

#Only x3,x4 and x7 shows significance i.e. their p-value is lesser than 0.05(significance level).A better fit of
#the model would be the one where only the significant variables exist.Also the overall p-value is also lesser than significane level (0.05) and hence we can say that the model is significant.

#The R-squared value 0.8434 which means that 89.94% of the variation is explained by the regressors. The adjusted R-Squared 
#shows the variation explained by the regressors that truly contribute to the known variation.

#2) Checking the multicolinearity assumption

library(car)

vif(mod)

#From this we observe that x1 and x6 are the  cayses of multicolinearity as their VIF is above 10.We remove these variables to 
#fulfill the multicolinearity assumption

new_dat=dat[,c(-2,-7)]
head(new_dat)

mod1=lm(y~.,data=new_dat)

vif(mod1)

#We find that there is no multicolinearity between the varibales as the varibales that caused it have been removed.


#Conclusion:

#The mutilcolonearity assumption has been fulfilled 
