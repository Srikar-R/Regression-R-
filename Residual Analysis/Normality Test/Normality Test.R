#Introduction: 

#Multiple regression is a statistical technique that can be used to analyze the relationship between a single dependent variable and several independent variables. The objective of multiple regression analysis is to use the independent variables whose values are known to predict the value of the single dependent value. Each predictor value is weighed, 
#the weights denoting their relative contribution to the overall prediction.

#The difference between an observed value of the response variable and 
#the value of the response variable predicted from the regression line.

#Objective: 

#1) To predict the marks based on the number of hours studied
#2) To plot the resiudals and test the assumptions of normality of the residuals

#Data Description:

#The given dataset describes the number of hours a student studied and the mask scored by them. The dataset contains two
#varibales i.e. Hours and Scores. It contains the data of 25 students.

library(readxl)
dat <- read_excel("C:/Users/Srikar/Desktop/SS/R/Sem 5/Linear Regression/Practical 8/dataset.xlsx")
head(dat)



#Procedure

#1) Constructing the regression model

mod=lm(dat$Scores~.,data=dat)
summary(mod)

#We obtain the model :

# Y = 2.4837+ 9.7758*X where

# Y is the number of marks obtained and X is the number of hours studied by student.


#We observe that the intecept p-value is above the significance value (0.05) and hence we can say that the intecept is not significant in the prediction.
#This means that if the values of the regressors were all zero, the intercept would not predict the average score of student if they 
#did not study at all.

#The p-values of the regressor Hours shows significance as its below 0.05. That means these variables describe the linear relationship with the 
#independent variable. .Also the overall p-value is also lesser than significane level (0.05) and hence we can say that the model is significant.

#The R-squared value 0.9529 which means that 95.29% of the variation is explained by the regressors. 


#2) Plotting the regression line and its residuals

library(broom)

model_diagnostics=augment(mod)
model_diagnostics

#We observe that the fitted values and the observed values differ on an average of 1.83 marks.We observe that the standard
#residuals are very small which indicate that our model is very useful in prediction.

library(tidyverse)

ggplot(model_diagnostics, aes(dat$Hours, dat$Scores)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = dat$Hours, yend = .fitted), color = "red", size =
                 0.3)+xlab("Hours")+ylab("Scores")+ggtitle("Regression plot")+theme(plot.title = element_text(hjust = 0.5))

## The plot shows the residuals or the difference between observed and expected values.The black dots are the observed values of the scores obtained by students and the blue line is the 
#fitted regression line and the red verticle lines are showing the residuals or the deviation between the observed
# and expected values of response variable.

#3) Testing the regression assumption of normality

#In simple regression,we assume that the error or the residual is nromally distributed. 
#We test the normality by plotting a Normal Q-Q (Qauntile-Quantile ) plot graphical technique for determining if two data sets come from populations with a 
#normal distribution

plot(mod)
#We observe that the plot is not along the dotted line or the standardized plots dont form a staright line. This means
#that the graph is not normally distributed rather is slightly negatively skewed as most data points are in the upper half.Observations
#11,25 and 4 are outliers as their standard residuals are unusally large.

#As the errors are not normally distributed, the response variable should be transformed such a way that the 
#errors should be apporoximately normally distributed.


#Conclusion:

#1. We obtain the model Y = 2.4837+ 9.7758*X where  Y is the number of marks obtained and X is the number of hours studied by student.

#2. We observe that resiudals are not normally distributed and hence the response variable should be transformed such a way that the 
#errors should be apporoximately normally distributed.When errors are not normally distributed, estimations are not normally distributed and we can no longer use p-values to decide if the coefficient is different from zero. In short, if the normality assumption of the errors is not met, we cannot draw a valid conclusion based on statistical 
#inference in linear regression analysis. Hence, after transformation, a new model is requried to be constructed.

