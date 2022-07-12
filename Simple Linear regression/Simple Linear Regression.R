#OBJECTIVE: To predict the percentage of an student based on the no. of study hours. 

#DATA DESCRIPTION: The given dataset describes the number of hours a student studied and the mask scored by them. The dataset contains two
#varibales i.e. Hours and Scores. It contains the data of 25 students.

#Here the dependent variable is "Marks" ( marks scored) and the independent variable is "Hours" (Number of hours studied)

library(readxl)

data1=read_excel("C:\\Users\\Srikar\\Desktop\\Study stuff\\Internship\\Grips Foundation\\Task 1\\dataset.xlsx")  #Importing dataset
head(data1)   # Viewing first 6 observations


#SUMMARY STATISTICS:

summary(data1)

#The minimum hours any student student studied in the dataset is 1 hour and 10 minutes and the maximum hoursa student studied is for 9.2 hours.
#The average hours the entire population studied is apporximately 5 hours.

#The least marks scored by a student is 17 and the maximum marks scored by a student is 95.The average score of the class is 51.48.

#DATA VISUALIZATION:

barplot(data1$Hours,col = 'red',xlab = 'Student',ylab='No of hours Studied')
barplot(data1$Scores,col= 'blue',xlab='Student',ylab='Nof marks scored')

#The bargraphs show the variablity in data. The number of studied and the marks scored by students differ from each student.

#RELEVANT TERMS: 

#I) Linear Regression- A linear regression is a statistical model that analyzes the relationship between a response variable (often called y) and one or more variables and their 
#interactions (often called x or explanatory variables). 

#II)p-value-A p-value indicates whether or not you can reject or accept a hypothesis

#II)R-Sqaured- It is the coefficient of determination or R². This measure is defined by the proportion of the 
#total variability explained by the regression model.

#R^2= (Explained Variation of the model)/ (Total variation of the model)


#ANALYSIS: 

#1) Plotting the distribution of scores through scatter plot

plot(data1,col='purple')

#2) Constructing the regression model

Reg=lm(data1$Scores~data1$Hours,data=data1)
Reg
summary(Reg)

# The intercept of the model is 2.4837 and the coefficent for the variable 'score' is 9.7758. It is observed that the 
# p-value of the varibale 'scores' is below 0.05 which means that it's an excellent addition to the model.

#The multiple R-sqaured is 0.9529 and adjusted R-sqaured is 0.9509 which are very close to 1. This means that it is explains 95% of
#the variability.

#The eqaution of the model is 

# Marks = 2.4837+ 9.7758 * X 

#Here X is the value of the variable, "Hours"

#3)Plotting the regression line 

abline(Reg)

#We see that the data plot are very close to the regression line. This tells us that the residuals of the plot are very close to the line
# which will be easy to predict the value as error is low.

#4)Testing the model with past data

hours=data.frame(c(2.5,5.1,3.2,8.5,3.5,1.5,9.2,5.5,8.3,2.7,7.7,5.9,4.5,3.3,1.1,8.9,2.5,1.9,6.1,7.4,2.7,4.8,3.8,6.9,7.8))
Test.data=predict(Reg,newdata = hours)

comparison=data.frame(Test.data,data1$Scores)
comparison

#We observe that values predicted by the model and the actual data are very close. Using the same model we can predict the amount of
#marks a student will score if they studied for 9 hours and 25 minutes.

#5) Predicting score if student studied for 9.25 hours

P=2.4837+ 9.7758 * 9.25
P

#CONCLUSION: 

#I) The regression model of this data is 2.4837+ 9.7758 * X where X is the number of hours studied.

#II) If a student studies for 9.25 hours then the predicted score would be 92.90 according to the regression model.

