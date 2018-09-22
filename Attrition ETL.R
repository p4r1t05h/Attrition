rm(list = ls())
getwd()
dir()
setwd("D:/Data Science/ETLHive/Projects/Attrition")
getwd()

#Loading appropriate libraries

library(xlsx)
library(dplyr)
library(tree)
library(e1071)
library(randomForest)
library(class)


#Loading Data

attrition<-read.xlsx("Attrition Case Study.xlsx", sheetIndex = 1, header = T)
att<-data.frame(attrition)

str(att)

head(att, 10)

#Changing in the proper data set

att$Attrition<-as.factor(att$Attrition)
att$Education<-as.factor(att$Education)
att$EnvironmentSatisfaction<-as.factor(att$EnvironmentSatisfaction)
att$JobInvolvement<-as.factor(att$JobInvolvement)
att$JobLevel<-as.factor(att$JobLevel)
att$JobSatisfaction<-as.factor(att$JobSatisfaction)
att$PerformanceRating<-as.factor(att$PerformanceRating)
att$RelationshipSatisfaction<-as.factor(att$RelationshipSatisfaction)
att$StockOptionLevel<-as.factor(att$StockOptionLevel)
att$WorkLifeBalance<-as.factor(att$WorkLifeBalance)

str(att)

#Checking for missing values

table(is.na(att))

#Creating new Predictor Age Group

Age_Group<-cut(att$Age, breaks = 2, labels = c("Young","Old"))
att<-cbind(att, Age_Group)
str(att)

#Exploratory Data Analysis
#Univariate analysis for All quantitative variables

#numerical analysis of Age

age.5p<-summary(att$Age)
age.5p

#Visualization of Age

hist(att$Age, main = "Histogram of Age", xlab = "Age")
boxplot(att$Age, ylab="Age", names = "Boxplot of Age")
plot(density(att$Age))

#Shapiro Test for normality (p>0.05)
shapiro.test(att$Age)
#normal

#Numerical analysis for Daily Rate

dailyrate.5p<-summary(att$DailyRate)
dailyrate.5p

#Visualization of Daily Rate

hist(att$DailyRate, main = "Histogram of Daily Rate", xlab = "Daily Rate")
boxplot(att$DailyRate, ylab="Daily Rate", names = "Boxplot of Daily Rate")
plot(density(att$DailyRate))

#Shapiro Test for normality (p>0.05)
shapiro.test(att$DailyRate)
#normal

#Numerical analysis for Distance from Home

distancefromhome.5p<-summary(att$DistanceFromHome)
distancefromhome.5p

#Visualization of Daily Rate

hist(att$DistanceFromHome, main = "Histogram of Distance From Home", xlab = "Distance From Home")
boxplot(att$DistanceFromHome, ylab="Distance From Home", names = "Boxplot of Distance From Home")
plot(density(att$DistanceFromHome))

#Shapiro Test for normality (p>0.05)
shapiro.test(att$DistanceFromHome)
#normal

#Numerical analysis for Employee Count

employeenumber.5p<-summary(att$EmployeeNumber)
employeenumber.5p

#Visualization of Daily Rate

hist(att$EmployeeNumber, main = "Histogram of Employee Number", xlab = "Employee Number")
boxplot(att$DailyRate, ylab="Employee Number", names = "Boxplot of EMployee Number")
plot(density(att$EmployeeNumber), main = "Employee Number")

#Shapiro Test for normality (p>0.05)
shapiro.test(att$Age)
#normal

#Numerical analysis for Hourly Rate

hourlyrate.5p<-summary(att$HourlyRate)
hourlyrate.5p

#Visualization of Hourly Rate

hist(att$HourlyRate, main = "Histogram of Hourly Rate", xlab = "Hourly Rate")
boxplot(att$HourlyRate, ylab="Hourly Rate", names = "Boxplot of Hourly Rate")
plot(density(att$DailyRate))

#Numerical analysis for Monthly Income

monthlyincome.5p<-summary(att$MonthlyIncome)
monthlyincome.5p

#Visualization of Daily Rate

hist(att$MonthlyIncome, main = "Histogram of Monthly Income", xlab = "Monthly Income")
boxplot(att$MonthlyIncome, ylab="Monthly Income", names = "Boxplot of Monthly Income")
plot(density(att$MonthlyIncome))

#Numerical analysis for Monthly Rate

monthlyrate.5p<-summary(att$MonthlyRate)
monthlyrate.5p

#Visualization of Monthly Rate

hist(att$MonthlyRate, main = "Histogram of Monthly Rate", xlab = "Monthly Rate")
boxplot(att$MonthlyRate, ylab="Monthly Rate", names = "Boxplot of Monthly Rate")
plot(density(att$MonthlyRate))

#Numerical analysis for N. of Companies Worked

companiesworked.5p<-summary(att$NumCompaniesWorked)
companiesworked.5p

#Visualization of Daily Rate

hist(att$NumCompaniesWorked, main = "Histogram of Companies Worked", xlab = "Companies Worked")
boxplot(att$NumCompaniesWorked, ylab="Companies Worked", names = "Boxplot of Companies Worked")
plot(density(att$NumCompaniesWorked))

#Numerical analysis for Salary Hike

salaryhike.5p<-summary(att$PercentSalaryHike)
salaryhike.5p

#Visualization of Salary Hike

hist(att$PercentSalaryHike, main = "Histogram of Salary Hike", xlab = "Percent of Salary Hike")
boxplot(att$PercentSalaryHike, ylab="Salary Hike", names = "Boxplot of Salary Hike")
plot(density(att$PercentSalaryHike))

#Numerical analysis for Total Working Years

workingyears.5p<-summary(att$TotalWorkingYears)
workingyears.5p

#Visualization of Working Years

hist(att$TotalWorkingYears, main = "Histogram of Working Years", xlab = "Working Years")
boxplot(att$TotalWorkingYears, ylab="Working Years", names = "Boxplot of Working Years")
plot(density(att$TotalWorkingYears))

#Numerical analysis for Training

training.5p<-summary(att$TrainingTimesLastYear)
training.5p

#Visualization of training

hist(att$TrainingTimesLastYear, main = "Histogram of Training", xlab = "Training")
boxplot(att$TrainingTimesLastYear, ylab="Training", names = "Boxplot of Training")
plot(density(att$TrainingTimesLastYear))

#Numerical analysis for Years at Company

yearsatcompany.5p<-summary(att$YearsAtCompany)
yearsatcompany.5p

#Visualization of Years at Company

hist(att$YearsAtCompany, main = "Histogram of Years At Company", xlab = "Years at Company")
boxplot(att$YearsAtCompany, ylab="Years At Company", names = "Boxplot of Years at Company")
plot(density(att$YearsAtCompany))

#Numerical analysis for Years in Role

yearsinrole.5p<-summary(att$YearsInCurrentRole)
yearsinrole.5p

#Visualization of Years in Role

hist(att$YearsInCurrentRole, main = "Histogram of Years in Role", xlab = "Years in Role")
boxplot(att$YearsInCurrentRole, ylab="Years in Role", names = "Boxplot of Years in Role")
plot(density(att$YearsInCurrentRole))

#Numerical analysis for Year Since Last Promotion

lastpromotion.5p<-summary(att$YearsSinceLastPromotion)
lastpromotion.5p

#Visualization of Year Since LAst Promotion

hist(att$YearsSinceLastPromotion, main = "Histogram of Year Since Last Promotion", xlab = "Last Promotion")
boxplot(att$YearsSinceLastPromotion, ylab="Last Promotion", names = "Boxplot of Last Promotion")
plot(density(att$YearsSinceLastPromotion))

#Numerical analysis for Years With Current Manager

manager.5p<-summary(att$YearsWithCurrManager)
manager.5p

#Visualization of Years With Current Manager

hist(att$YearsWithCurrManager, main = "Histogram of Current Manager", xlab = "Years with Current Manager")
boxplot(att$YearsWithCurrManager, ylab="Years With Current Manager", names = "Boxplot of Years With Current Manager")
plot(density(att$YearsWithCurrManager))


#Univariate Analysis of Categorical Variables

#Numerical Analysis of Business Travel

prop.table(table(att$BusinessTravel))*100

businesstravel<-as.matrix(table(att$BusinessTravel))


#visualization of Business Travel
barplot(businesstravel, col = c(3,4,5))
pie(businesstravel)

str(att)
#Numerical Analysis of Department

prop.table(table(att$Department))*100

department<-as.matrix(table(att$Department))


#visualization of Department
barplot(department, col = c(3,4,5))
pie(department)

#Numerical Analysis of Environment Satisfaction

prop.table(table(att$EnvironmentSatisfaction))*100

envi<-as.matrix(table(att$EnvironmentSatisfaction))


#visualization of Environment Satisfaction
barplot(envi, col = c(3,4,5))
pie(envi)

#Numerical Analysis of Gender

prop.table(table(att$Gender))*100

gender<-as.matrix(table(att$Gender))


#visualization of Business Travel
barplot(gender)
pie(gender)

#Numerical Analysis of Job Involvement

prop.table(table(att$JobInvolvement))*100

jobinv<-as.matrix(table(att$JobInvolvement))


#visualization of Business Travel
barplot(jobinv)
pie(jobinv)

#Numerical Analysis of Job Level

prop.table(table(att$JobLevel))*100

level<-as.matrix(table(att$JobLevel))


#visualization of Job Level
barplot(level)
pie(level)

#Numerical Analysis of Job Role

prop.table(table(att$JobRole))*100

jobrole<-as.matrix(table(att$JobRole))


#visualization of Business Travel
barplot(jobrole)
pie(jobrole)

#Numerical Analysis of Job Satisfaction

prop.table(table(att$JobSatisfaction))*100

satis<-as.matrix(table(att$JobSatisfaction))


#visualization of Job Satisfaction
barplot(satis)
pie(satis)

#Numerical Analysis of Marital Status

prop.table(table(att$MaritalStatus))*100

marital<-as.matrix(table(att$MaritalStatus))


#visualization of Business Travel
barplot(marital)
pie(marital)

#Numerical Analysis of Overtime

prop.table(table(att$OverTime))*100

overtime<-as.matrix(table(att$OverTime))


#visualization of Business Travel
barplot(overtime)
pie(overtime)

#Numerical Analysis of Performance Rating

prop.table(table(att$PerformanceRating))*100

perform<-as.matrix(table(att$PerformanceRating))


#visualization of Business Travel
barplot(perform)
pie(perform)

#Numerical Analysis of Relationship Satisfaction

prop.table(table(att$RelationshipSatisfaction))*100

relation<-as.matrix(table(att$RelationshipSatisfaction))


#visualization of Relationship Statisfaction
barplot(relation)
pie(relation)

#Numerical Analysis of Stock Option

prop.table(table(att$StockOptionLevel))*100

stock<-as.matrix(table(att$StockOptionLevel))


#visualization of Business Travel
barplot(stock)
pie(stock)

#Numerical Analysis of Work Life Balance

prop.table(table(att$WorkLifeBalance))*100

worklife<-as.matrix(table(att$WorkLifeBalance))


#visualization of Work Life Balance
barplot(worklife)
pie(worklife)

#Numerical Analysis of Age Group

prop.table(table(att$Age_Group))*100

agegroup<-as.matrix(table(att$Age_Group))


#visualization of Business Travel
barplot(agegroup)
pie(agegroup)

#======================================================

#Bivariate Analysis
#C->C (C=Attrition)

#Numerical Analysis of Business Travel-> Attrition

cc1<-table(att$BusinessTravel, att$Attrition)
cc1.prop<-prop.table(cc1)*100
cc1.prop

#Dependency Test

cc1.test<-chisq.test(att$BusinessTravel, att$Attrition)
cc1.test


#Dependent

#Numerical Analysis of Department-> Attrition

cc2<-table(att$Department, att$Attrition)
cc2.prop<-prop.table(cc2)*100
cc2.prop

#Dependency Test

cc2.test<-chisq.test(att$Department, att$Attrition)
cc2.test


#Dependent

str(att)

#Numerical Analysis of Education-> Attrition

cc3<-table(att$Education, att$Attrition)
cc3.prop<-prop.table(cc3)*100
cc3.prop

#Dependency Test

cc3.test<-chisq.test(att$Education, att$Attrition)
cc3.test

#INDependent

str(att)

#Numerical Analysis of Education Field-> Attrition

cc4<-table(att$EducationField, att$Attrition)
cc4.prop<-prop.table(cc4)*100
cc4.prop

#Dependency Test

cc4.test<-chisq.test(att$EducationField, att$Attrition)
cc4.test

#Dependent

str(att)

#Numerical Analysis of job Involvement-> Attrition

cc5<-table(att$JobInvolvement, att$Attrition)
cc5.prop<-prop.table(cc5)*100
cc5.prop

#Dependency Test

cc5.test<-chisq.test(att$JobInvolvement, att$Attrition)
cc5.test

#Dependent

str(att)

#Numerical Analysis of Job Level-> Attrition

cc6<-table(att$JobLevel, att$Attrition)
cc6.prop<-prop.table(cc6)*100
cc6.prop

#Dependency Test

cc6.test<-chisq.test(att$JobLevel, att$Attrition)
cc6.test

#Dependent

str(att)

#Numerical Analysis of Job Level-> Attrition

cc7<-table(att$JobRole, att$Attrition)
cc7.prop<-prop.table(cc7)*100
cc7.prop

#Dependency Test

cc7.test<-chisq.test(att$JobRole, att$Attrition)
cc7.test

#Dependent

str(att)

#Numerical Analysis of Job Satisfaction-> Attrition

cc8<-table(att$JobSatisfaction, att$Attrition)
cc8.prop<-prop.table(cc8)*100
cc8.prop

#Dependency Test

cc8.test<-chisq.test(att$JobSatisfaction, att$Attrition)
cc8.test

#Dependent

str(att)

#Numerical Analysis of Job Level-> Attrition

cc9<-table(att$MaritalStatus, att$Attrition)
cc9.prop<-prop.table(cc9)*100
cc9.prop

#Dependency Test

cc9.test<-chisq.test(att$MaritalStatus, att$Attrition)
cc9.test

#Dependent

str(att)

#Numerical Analysis of overtime-> Attrition

cc10<-table(att$OverTime, att$Attrition)
cc10.prop<-prop.table(cc10)*100
cc10.prop

#Dependency Test

cc10.test<-chisq.test(att$OverTime, att$Attrition)
cc10.test

#Dependent

str(att)

#Numerical Analysis of Performance Rating-> Attrition

cc11<-table(att$PerformanceRating, att$Attrition)
cc11.prop<-prop.table(cc11)*100
cc11.prop

#Dependency Test

cc11.test<-chisq.test(att$PerformanceRating, att$Attrition)
cc11.test

#InDependent

str(att)

#Numerical Analysis of Relationship Satisfaction-> Attrition

cc12<-table(att$RelationshipSatisfaction, att$Attrition)
cc12.prop<-prop.table(cc12)*100
cc12.prop

#Dependency Test

cc12.test<-chisq.test(att$RelationshipSatisfaction, att$Attrition)
cc12.test

#InDependent

str(att)

#Numerical Analysis of Stock Option-> Attrition

cc13<-table(att$StockOptionLevel, att$Attrition)
cc13.prop<-prop.table(cc13)*100
cc13.prop

#Dependency Test

cc13.test<-chisq.test(att$StockOptionLevel, att$Attrition)
cc13.test

#Dependent

str(att)

#Numerical Analysis of Work Life Balance-> Attrition

cc14<-table(att$WorkLifeBalance, att$Attrition)
cc14.prop<-prop.table(cc14)*100
cc14.prop

#Dependency Test

cc14.test<-chisq.test(att$WorkLifeBalance, att$Attrition)
cc14.test

#Dependent

str(att)

#Numerical Analysis of age Group-> Attrition

cc15<-table(att$Age_Group, att$Attrition)
cc15.prop<-prop.table(cc15)*100
cc15.prop

#Dependency Test

cc15.test<-chisq.test(att$Age_Group, att$Attrition)
cc15.test

#Dependent

##=====================================================
#Q->C 

#Using Logistic regression for determining important quantitative Xs

lr<-glm(att$Attrition~Age+DailyRate+DistanceFromHome+EmployeeCount+EmployeeNumber+HourlyRate+MonthlyIncome+
          MonthlyRate+NumCompaniesWorked+PercentSalaryHike+StandardHours+TotalWorkingYears+TrainingTimesLastYear+
          YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = att, family = binomial)

summary(lr)$coef

#Removing unImportant Xs= work life balance, relationship satisfaction, performance rating, overtime, job satisfaction
#job involvement, gender, environment satisfaction, education field, business travel, training times last year
#percent salary hike, monthly rate, hourly rate, distance from home, daily rate

str(att)
att<-att[,c(-4,-7,-9,-10,-13,-20,-22,-24,-25,-26)]
str(att)

att<-att[,-17]

#Dividing into train and test

set.seed(123)

ind<-sample(1:nrow(att), 1000)
train<-att[ind,]
test<-att[-ind,]

#==============Logistic Regression Model

model1.lr<-glm(Attrition~., data = train, family = "binomial")

coef(model1.lr)
summary(model1.lr)

predict.model1<-predict(model1.lr, newdata = test[,-1], type ="response")

#Converting probablities into 0 & 1

predict.model1<-ifelse(predict.model1>0.5,1,0)


predict1.table<-table(Predicted=predict.model1, True=test$Attrition)

predict1.table

#Accuracy

sum(predicct1.table[c(1,4)])/ sum(predicct1.table[c(1:4)])
#=87.66

#Error Rate
1-sum(predicct1.table[c(1,4)])/ sum(predicct1.table[c(1:4)])
#=12.34

#====Decision tree Model

model2.tree = tree(train$Attrition ~., data=train)
plot(model2.tree)
text(model2.tree)

predict.model2<-predict(model2.tree, newdata = test[,-1], type = "class")

predict2.table<-table(Predicted=predict.model2, True=test$Attrition)

predict2.table

#Accuracy

sum(predict2.table[c(1,4)])/ sum(predict2.table[c(1:4)])
#=82.34

#Error Rate
1-sum(predict2.table[c(1,4)])/ sum(predict2.table[c(1:4)])
#=17.76

#=====Random Forest Model=============
model3.rf<-randomForest(train$Attrition~., data = train, importance=T, ntree=500)

summary(model3.rf)

predict.model3<-predict(model3.rf, test[,-1])

predict3.table<-table(Predicted=predict.model3, True=test$Attrition)

predict3.table

#Accuracy

sum(predict3.table[c(1,4)])/ sum(predict3.table[c(1:4)])
#=85.95

#Error Rate
1-sum(predict3.table[c(1,4)])/ sum(predict3.table[c(1:4)])
#=14.04

#=====bagging Model=============
model4.bagging<-randomForest(train$Attrition~., data = train, importance=T, ntree=500, mtry=19)

summary(model4.bagging)

predict.model4<-predict(model4.bagging, test[,-1])

predict4.table<-table(Predicted=predict.model4, True=test$Attrition)

predict4.table

#Accuracy

sum(predict4.table[c(1,4)])/ sum(predict4.table[c(1:4)])
#=86.17

#Error Rate
1-sum(predict4.table[c(1,4)])/ sum(predict4.table[c(1:4)])
#=13.82

#====Naive Bayes Model

model5.nb<-naiveBayes(train$Attrition~., data = train)

model5.nb$tables

#Predicting Model

predict.model5<-predict(model5.nb, newdata = test[,-1], type = "class")

predict5.table<-table(Predicted=predict.model5, True=test$Attrition)

predict5.table

#Accuracy

sum(predict5.table[c(1,4)])/ sum(predict5.table[c(1:4)])
#=78.51

#Error Rate
1-sum(predict5.table[c(1,4)])/ sum(predict5.table[c(1:4)])
#=21.5

#======================Support Vector Machine==============

model6.svm<-tune(svm, train$Attrition~., data=train, kernal="radial",
                 ranges = list(cost=c(0.1,1,10,100,1000)))

#summary(model6.svm)

#predict.model6<-predict(model6.svm$best, newdata= test[,-1], type="class")

#=====KNN Model

#model7.knn<-knn(train[,2:25], test[,2:25], train[,1], k=10)
