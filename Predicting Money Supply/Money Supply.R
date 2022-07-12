
#Aim: To build a regression model to predict the amount of currency in the economy

#Data Description:

#The data obsreves the various monetary variables in India over a period of 58 months. The variables in the data set are M1(the amount of currency in
#circulation), IIP(the index of inflation), INT(the bamk interest rates), UPI(the value of UPI  transactions in that month), 
#CC(the value of credit card transactions in that month), DC(thev alue of debit card transactions in that month).
 
library(readxl)
dat <- read_excel("C:/Users/Srikar/Downloads/Book2.xlsx")
head(dat)

#Data Summary:

summary(dat)

#We observe that the minimum amount of currency circulation in the economy is 9.65 and the maximum is 10.327. The index of inflation ranges 
#between 3.989 to 4.971. The bank interest rate is between 5.5 and 7.5/ The range of value of UPI transactions are between -4.711 to 5.610.
#The value of credit card transactions lie between 18.16 to 19.14. The value of debit transactions lie between 20 to 21.

library(readxl)
dat <- read_excel("C:/Users/Srikar/Downloads/Book2.xlsx")
head(dat)

#Procedure

#1.Visualizing COrr plot

r=cor(dat)

library(ggplot2)
library(ggcorrplot)
ggcorrplot(r, hc.order = TRUE, type = "lower",lab = TRUE)

# From this table, we observe that there is high correlation between UPI value and value of credit card transactions.There
#is also high correlation between UPI and the money circulated. There is medium corrlation between the credit and debit card valations.

#We can observe low correlation with respect to credit card and bank interest rates. There is no correlation between 
#credit card and index of inflation.


#2. Building the regression model

mod=lm(dat$M1~.,data = dat)
summary(mod)


#We observe that intercept is significant as its p-value is lesser than the significance value (0.05). This means that if all the values
#were 0, then the average circulation of money would be 12.13 thousand crores.

#We also observe the variables UPI, CC and DC to be significant as their p-values are lesser than the significane value (0.05).Variables such as
#IIP and INT have no significant relationship with the money circulated.

#The overall model is significant as the p-value is way below the significance level (0.05). The R-squared value shows
# 0.6853 which means that the independant variables only contribute to 68.53% of variation of the dependant variable M1, the money circulated in the 
#economy. The adjusted R-Square which is obviouylsy lesser shows the amount of variation that is described by variables that 
#are actualy significant.

#We can confirm the variables that matter by using the ANOVA model as well.

library(stats)
anova(mod)

#Using the anova model, we can see that even INT shows significance on the model which varies from our intital summary of our regression model.
#We also observe that from the ANOVA model, we can see that CC does not show significance as opposed to our previuos model.

#The number of reasons could be enumourous but to begin with we can check the assumption sof multiple regression and check
#whether the model satisfies these are not.

#3) Checking the important assumptions

#(i) The multicolinearity assumption

library(car)
vif(mod)

#Since we see that the vif value of the regressors are not above 10, we say that assumption of no multicollinearity is satisfied.
#This means that no two variables interact with each other to have an impact on the money circulation in the economy.

#(ii) Autocorrelation assumption

library(lmtest)

dwtest(dat$IIP~dat$UPI)
dwtest(dat$IIP~dat$INT)
dwtest(dat$IIP~dat$CC)
dwtest(dat$IIP~dat$DC)
dwtest(dat$INT~dat$UPI)
dwtest(dat$INT~dat$IIP)
dwtest(dat$INT~dat$CC)
dwtest(dat$INT~dat$DC)
dwtest(dat$UPI~dat$CC)
dwtest(dat$UPI~dat$DC)
dwtest(dat$CC~dat$DC)

#We can see that the assumption of autocorrelation has been fulfilled as all of them have a p-value less than significance level.

#(iii) Heteroscedascity assumption)

library(lmtest)
bptest(mod)

#SInce the p-value is lesser than 0.05, the significance level, we say that it is significant and that the it is hetereoscedastic.

#Since it fulfilles the assumptions given above, we can test out by only taking factors that have significant impact on the dependant variable.

#4) Perfroming stepwise procedure

step(mod, direction="both")

#We observe that after adding and subtracting the variables, we get a model that is built only on significant varibales. This model is more
#reliable and will possess less error. We have removed the variable INT from the model

#New model
nmod=lm(dat$M1 ~ IIP + UPI + CC + DC, data = dat)

summary(nmod)

#We can see that most variables show significane except IIP which was not removed during the stepwise procedure as it might explain the variation to 
#a minimum extent.

confint(nmod)
#Using a 95 confidence interval, we can say that if we were to keep sampling the data, we will get these ranges of values for our
#independant varables between the given range.


#We still see that the R-squared is more or less the same. Alhtough the model is significant, it might not be perfect due to several factors such
#as missing variables, missing data or having non-normal distribution of errors.

#We can test the assumption of normality of errors to verify the model

#5) HCecking the normality of error assumption

plot(nmod)

#We observe that residual line deviates a lot fro, the dotted line showing that there is a huge deviation from the observed balies.
#We see that as the fitted values become larger, the error becomes smaller.

#From the Normal Q-Q plot, we observe that the data is not exacltyu normal and is skewed towards the left. We also see some outliers in
#the dataset. We can test the normlaity of errors using the Shapiro-Wilk test to clarify.

shapiro.test(nmod$residuals)

#As seen from the diagram and observation from the test, we observe that the data is not normal although most of the data plots
#are very close the mean. There are a few outliers as well which makes the data not exaclty normal.


#6) Testing the model

#Although the model might not be perfect and not fulfil assumptions of normlaity of error due to outliers and some skewed datapoints,
#we can test the models strength as for now by substituing the dependanct varibales from the dataset into the model.

# Comparison of Observed and Expected value

library(broom)
prediction=augment(nmod)
prediction

#We see that the model predicts close to the observed values but does not exactly give us values very near to the observed values itself.
#We see that our model with an R-Sqauyred of 65% is able to gives values closse to the observed values itself.

#We can see an indepth comparion with the Repsonse variable (M1) and Regressors from the new model.

library(tidyverse)

ggplot(prediction, aes(dat$IIP, dat$M1)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = dat$IIP, yend = .fitted), color = "red", size =
                 0.3)+xlab("IIP")+ylab("M!")+ggtitle("Regression plot")+theme(plot.title = element_text(hjust = 0.5))

#We obsevre that with respect to IIP, the residual lines are faw away from the regression line itself.

ggplot(prediction, aes(dat$UPI, dat$M1)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = dat$IIP, yend = .fitted), color = "red", size =
                 0.3)+xlab("UPI")+ylab("M1")+ggtitle("Regression plot")+theme(plot.title = element_text(hjust = 0.5))

#We obsevre that with respect to UPI, the residual lines are faw away from the regression line itself.

ggplot(prediction, aes(dat$CC, dat$M1)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = dat$IIP, yend = .fitted), color = "red", size =
                 0.3)+xlab("CC")+ylab("M!")+ggtitle("Regression plot")+theme(plot.title = element_text(hjust = 0.5))

ggplot(prediction, aes(dat$IIP, dat$M1)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = dat$IIP, yend = .fitted), color = "red", size =
                 0.3)+xlab("DC")+ylab("M!")+ggtitle("Regression plot")+theme(plot.title = element_text(hjust = 0.5))

#Similarlry for CC and DC the residuals are closer to the line as the values increase since the values are clustered amongst the
#higher values of the observation.


#Conclusion:

# We observe that we got a model that only explain 65% of the variaion. Although this isnt an idela R-squuared value, it still fulfills the assumptions
#of the multiple regression. A higher R-square need not mean that our model isnt fit but usually it is better to have more indepndant variables 
#that truly explain the model. Our model shows that it is not the perfect fit but can be used to predict to some extent.

#The drawbacks of the model also include the fact that there are some outliers in the dataset and the dataset isnt exaclty normal. This  can 
#be tackled by tranforming the data into a perfectly normal fit. We might also require some more independant variables to explain the variation in
#the data.

# Nevertheless, we see that the model's expected values are closed to the bserved values. Since, the values are smaller and are in decimals, the 
#preciion really matters in such cases.

#The model proves that its useful in the domain of economics as it helps to predict how much currency will be flowing during a given period of time.
#This is extremely important as the government must have enough for its citizens so that people can afford and recieve income acordingly. The model
#will help the govenment to know when to pump in more money and take away when it is needed.









































































































































































































