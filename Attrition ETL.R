rm(list = ls())
getwd()
dir()
dir.create("ETL Attrition")
setwd("D:/Data Science/ETLHive/Projects/Attrition")
getwd()

#Loading appropriate libraries

library(xlsx)
library(dplyr)

#Loading Data

attrition<-read.xlsx("Attrition Case Study.xlsx", sheetIndex = 1, header = T)
att<-attrition

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
?cut
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
#Q->Q (Q=Monthly Income)

#Numerical Analysis of Age-> Monthly Income

qq1<-cor(att$Age, att$MonthlyIncome)
qq1

#Visualization

plot(att$Age~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq1.test<-lm(Age~MonthlyIncome, data = att)
summary(qq1.test)
abline(qq1.test)

#Dependent
#Numerical Analysis of Daily Rate-> Monthly Income

qq2<-cor(att$DailyRate, att$MonthlyIncome)
qq2

#Visualization

plot(att$DailyRate~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq2.test<-lm(DailyRate~MonthlyIncome, data = att)
summary(qq2.test)
abline(qq2.test)

#InDependent

#Numerical Analysis of Distance From Home-> Monthly Income

qq3<-cor(att$DistanceFromHome, att$MonthlyIncome)
qq3

#Visualization

plot(att$DistanceFromHome~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq3.test<-lm(DistanceFromHome~MonthlyIncome, data = att)
summary(qq3.test)
abline(qq3.test)

#InDependent

#Numerical Analysis of Hourly Rate-> Monthly Income

qq4<-cor(att$HourlyRate, att$MonthlyIncome)
qq4

#Visualization

plot(att$HourlyRate~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq4.test<-lm(HourlyRate~MonthlyIncome, data = att)
summary(qq4.test)
abline(qq4.test)

#InDependent

#Numerical Analysis of Monthly Rate-> Monthly Income

qq5<-cor(att$MonthlyRate, att$MonthlyIncome)
qq5

#Visualization

plot(att$MonthlyRate~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq5.test<-lm(MonthlyRate~MonthlyIncome, data = att)
summary(qq5.test)
abline(qq5.test)

#InDependent

#Numerical Analysis of Number of Companies Worked-> Monthly Income

qq6<-cor(att$NumCompaniesWorked, att$MonthlyIncome)
qq6

#Visualization

plot(att$NumCompaniesWorked~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq6.test<-lm(NumCompaniesWorked~MonthlyIncome, data = att)
summary(qq6.test)
abline(qq6.test)

#Dependent

#Numerical Analysis of Percent Salary Hike-> Monthly Income

qq7<-cor(att$PercentSalaryHike, att$MonthlyIncome)
qq7

#Visualization

plot(att$PercentSalaryHike~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq7.test<-lm(PercentSalaryHike~MonthlyIncome, data = att)
summary(qq7.test)
abline(qq7.test)

#InDependent

#Numerical Analysis of Total Working Years-> Monthly Income

qq8<-cor(att$TotalWorkingYears, att$MonthlyIncome)
qq8

#Visualization

plot(att$TotalWorkingYears~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq8.test<-lm(TotalWorkingYears~MonthlyIncome, data = att)
summary(qq8.test)
abline(qq8.test)

#Dependent

#Numerical Analysis of Training Times Last Year-> Monthly Income

qq9<-cor(att$TrainingTimesLastYear, att$MonthlyIncome)
qq9

#Visualization

plot(att$TrainingTimesLastYear~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq9.test<-lm(TrainingTimesLastYear~MonthlyIncome, data = att)
summary(qq9.test)
abline(qq9.test)

#InDependent

#Numerical Analysis of Years At Company-> Monthly Income

qq10<-cor(att$YearsAtCompany, att$MonthlyIncome)
qq10

#Visualization

plot(att$YearsAtCompany~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq10.test<-lm(YearsAtCompany~MonthlyIncome, data = att)
summary(qq10.test)
abline(qq10.test)

#Dependent

#Numerical Analysis of Years In Current Role-> Monthly Income

qq11<-cor(att$YearsInCurrentRole, att$MonthlyIncome)
qq11

#Visualization

plot(att$YearsInCurrentRole~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq11.test<-lm(YearsInCurrentRole~MonthlyIncome, data = att)
summary(qq11.test)
abline(qq11.test)

#Dependent

#Numerical Analysis of Years Since Last Promotion-> Monthly Income

qq12<-cor(att$YearsSinceLastPromotion, att$MonthlyIncome)
qq12

#Visualization

plot(att$YearsSinceLastPromotion~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq12.test<-lm(YearsSinceLastPromotion~MonthlyIncome, data = att)
summary(qq12.test)
abline(qq12.test)

#Dependent

#Numerical Analysis of Years With Current Manager-> Monthly Income

qq13<-cor(att$YearsWithCurrManager, att$MonthlyIncome)
qq13

#Visualization

plot(att$YearsWithCurrManager~att$MonthlyIncome)

#Dependency test
#t Test for Slope

qq13.test<-lm(MonthlyIncome~YearsWithCurrManager, data = att)
summary(qq13.test)
abline(qq13.test)

#Dependent

#============================================================
#C->Q (Q=Monthly Income)

str(att)

#Numerical Analysis of Business Travel-> Monthly Income

cq1<-boxplot(att$MonthlyIncome~att$BusinessTravel)
cq1$stats
tapply(att$MonthlyIncome, att$BusinessTravel, summary)

#Dependency test


cq1.aov<-aov(att$MonthlyIncome~att$BusinessTravel)
summary(cq1.aov)

#InDependent

#Numerical Analysis of Department-> Monthly Income

cq2<-boxplot(att$MonthlyIncome~att$Department)
cq2$stats
tapply(att$MonthlyIncome, att$Department, summary)

#Dependency Test

cq2.aov<-aov(att$MonthlyIncome~att$Department)
summary(cq2.aov)

#Dependent

#Numerical Analysis of Education-> Monthly Income

cq3<-boxplot(att$MonthlyIncome~att$Education)
cq3$stats
tapply(att$MonthlyIncome, att$Education, summary)

#Dependency Test

cq3.aov<-aov(att$MonthlyIncome~att$Education)
summary(cq3.aov)

#Dependent

#Numerical Analysis of Education Field-> Monthly Income

cq4<-boxplot(att$MonthlyIncome~att$EducationField)
cq4$stats
tapply(att$MonthlyIncome, att$EducationField, summary)

#Dependency Test

cq4.aov<-aov(att$MonthlyIncome~att$EducationField)
summary(cq4.aov)

par(mfrow=c(1,1))

#InDependent

#Numerical Analysis of Education Field-> Monthly Income

str(att)

cq5<-boxplot(att$MonthlyIncome~att$EnvironmentSatisfaction)
cq5$stats
tapply(att$MonthlyIncome, att$EnvironmentSatisfaction, summary)

#Dependency Test

cq5.aov<-aov(att$MonthlyIncome~att$EnvironmentSatisfaction)
summary(cq5.aov)

#InDependent

#Numerical Analysis of Gender-> Monthly Income

str(att)

cq6<-boxplot(att$MonthlyIncome~att$Gender)
cq6$stats
tapply(att$MonthlyIncome, att$Gender, summary)

#Dependency Test

cq6.test<-t.test(att$MonthlyIncome~att$Gender, alternative = "t")
cq6.test

#InDependent

#Numerical Analysis of Job Involvement-> Monthly Income

str(att)

cq7<-boxplot(att$MonthlyIncome~att$JobInvolvement)
cq7$stats
tapply(att$MonthlyIncome, att$JobInvolvement, summary)

#Dependency Test

cq7.aov<-aov(att$MonthlyIncome~att$JobInvolvement)
summary(cq7.aov)

#InDependent

#Numerical Analysis of Job Level-> Monthly Income

str(att)

cq8<-boxplot(att$MonthlyIncome~att$JobLevel)
cq8$stats
tapply(att$MonthlyIncome, att$JobLevel, summary)

#Dependency Test

cq8.aov<-aov(att$MonthlyIncome~att$JobLevel)
summary(cq8.aov)

#Dependent

#Numerical Analysis of Job Role-> Monthly Income

str(att)

cq9<-boxplot(att$MonthlyIncome~att$JobRole)
cq9$stats
tapply(att$MonthlyIncome, att$JobRole, summary)

#Dependency Test

cq9.aov<-aov(att$MonthlyIncome~att$JobRole)
summary(cq9.aov)

#Dependent

#Numerical Analysis of Job Satisfaction-> Monthly Income

str(att)

cq10<-boxplot(att$MonthlyIncome~att$JobSatisfaction)
cq10$stats
tapply(att$MonthlyIncome, att$JobSatisfaction, summary)

#Dependency Test

cq10.aov<-aov(att$MonthlyIncome~att$JobSatisfaction)
summary(cq10.aov)

#InDependent

#Numerical Analysis of Marital Status-> Monthly Income

str(att)

cq11<-boxplot(att$MonthlyIncome~att$MaritalStatus)
cq11$stats
tapply(att$MonthlyIncome, att$MaritalStatus, summary)

#Dependency Test

cq11.aov<-aov(att$MonthlyIncome~att$MaritalStatus)
summary(cq11.aov)

#Dependent

#Numerical Analysis of Overtime -> Monthly Income

str(att)

cq12<-boxplot(att$MonthlyIncome~att$OverTime)
cq12$stats
tapply(att$MonthlyIncome, att$OverTime, summary)

#Dependency Test

cq12.test<-t.test(att$MonthlyIncome~att$OverTime, alternative = "t")
cq12.test

#InDependent

#Numerical Analysis of Performance Rating-> Monthly Income

str(att)

cq13<-boxplot(att$MonthlyIncome~att$PerformanceRating)
cq13$stats
tapply(att$MonthlyIncome, att$PerformanceRating, summary)

#Dependency Test

cq13.test<-t.test(att$MonthlyIncome~att$PerformanceRating, alternative="t")
cq13.test

#InDependent

#Numerical Analysis of Relationship Satisfaction-> Monthly Income

str(att)

cq14<-boxplot(att$MonthlyIncome~att$RelationshipSatisfaction)
cq14$stats
tapply(att$MonthlyIncome, att$RelationshipSatisfaction, summary)

#Dependency Test

cq14.aov<-aov(att$MonthlyIncome~att$RelationshipSatisfaction)
summary(cq14.aov)

#InDependent

#Numerical Analysis of Stock Option Level-> Monthly Income

str(att)

cq15<-boxplot(att$MonthlyIncome~att$StockOptionLevel)
cq15$stats
tapply(att$MonthlyIncome, att$StockOptionLevel, summary)

#Dependency Test

cq15.aov<-aov(att$MonthlyIncome~att$StockOptionLevel)
summary(cq15.aov)

#Dependent

#Numerical Analysis of Work Life Balance-> Monthly Income

str(att)

cq16<-boxplot(att$MonthlyIncome~att$WorkLifeBalance)
cq16$stats
tapply(att$MonthlyIncome, att$WorkLifeBalance, summary)

#Dependency Test

cq16.aov<-aov(att$MonthlyIncome~att$WorkLifeBalance)
summary(cq16.aov)

#InDependent

#Numerical Analysis of Age Group-> Monthly Income

str(att)

cq17<-boxplot(att$MonthlyIncome~att$Age_Group)
cq17$stats
tapply(att$MonthlyIncome, att$Age_Group, summary)

#Dependency Test

cq17.test<-t.test(att$MonthlyIncome~att$Age_Group, alternative ="t")
cq17.test

#Dependent

#Removing unImportant Xs= work life balance, relationship satisfaction, performance rating, overtime, job satisfaction
#job involvement, gender, environment satisfaction, education field, business travel, training times last year
#percent salary hike, monthly rate, hourly rate, distance from home, daily rate

att<-rm(att$WorkLifeBalance, att$RelationshipSatisfaction, att$PerformanceRating, att$OverTime,
        att$JobSatisfaction, att$JobInvolvement, att$Gender, att$EnvironmentSatisfaction, att$EducationField,
        att$BusinessTravel, att$TrainingTimesLastYear, att$PercentSalaryHike, att$MonthlyRate, att$HourlyRate,
        att$DistanceFromHome, att$DailyRate)