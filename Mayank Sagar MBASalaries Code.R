# Analysis of MBA Starting Salaries
# NAME: Mayank Sagar
# EMAIL: mayanksagar26@gmail.com
# COLLEGE : R.V.COllege Of Engineering, Bengaluru.

##setting the directory and assigning a variabel to the data frame
setwd("D:/Data Science and Analytics using R/Data files")

#Reading the dataset and creating a data frame
mba.df<-read.csv(paste("MBASSData.csv",sep = ""))

#Viewing the data frame mba.df
View(mba.df)

#Analyzing the summary of the data and describing the variables
library(psych)
describe(mba.df)

summary(mba.df)

#Creating a subset of those who have given their salary details and taken part in the survey
mbasalary.df<-mba.df[which(mba.df$salary>999),]
View(mbasalary.df)

#Visualizing  through Box Plots / Bar Plots the distribution of each variable independently
##BOX PLOTS##
#GMAT Score
boxplot(mba.df$gmat_tot,horizontal = TRUE, xlab="GMAT Score",main="BoxPlot Presentation Of GMAT Score")

#GMAT Percentages
par(mfrow=c(1,3))
with(mba.df, boxplot(mba.df$gmat_qpc,main="GMAT percentage quantitive",ylab="Percentage %"))
with(mba.df, boxplot(mba.df$gmat_vpc,main="GMAT percentage verbal",ylab="Percentage %"))
with(mba.df, boxplot(mba.df$gmat_tpc,main="GMAT percentage total",ylab="Percentage %"))
par(mfrow=c(1,1))

#Spring avg and Fall avg
par(mfrow=c(1,2))
with(mba.df, boxplot(mba.df$s_avg,main="Spring MBA Average",ylab="Average"))
with(mba.df, boxplot(mba.df$f_avg,main="Fall MBA Average",ylab="Average"))
par(mfrow=c(1,1))

#Working years
boxplot(mba.df$work_yrs,horizontal = TRUE, xlab="Working Yeats",main=" Working experience in years")

#Starting Salaries
table(mba.df$salary>999)
salarygiven<-mba.df$salary[mba.df$salary>999] 
  ##This is done to avoid the mean of those who didn't give their salary details or not placed.
boxplot(salarygiven,horizontal = TRUE, xlab="Salary",main="Boxplot presentation of Starting salary ")

##BAR PLOTS##

#Age
count<-table(mba.df$age)
barplot(count, main = "Barplot for age", xlab = "Age in Years")

#Sex
count1<-table(mba.df$sex)
barplot(count1, main = "Barplot for sex of the people", xlab = "Gender, Male(1) Female(2)")
axis(side=1, at=c(1,2), labels = c("Male", "Female"))


#Quartile Ranking
count2<-table(mba.df$quarter)
barplot(count2, main = "Barplot for quartile ranking")

#First Language
count3<-table(mba.df$frstlang)
barplot(count3, main = "Barplot for First language selected",xlab = "English(1), Others(2)")
axis(side=1, at=c(1,2), labels = c("English", "Others"))

#Degree of satisfaction
count4<-table(mba.df$satis[mba.df$satis<998])
barplot(count4, main = "Barplot for Degree of satisfaction", xlab = "Rating")

#Scatter Plots to understand how are the variables correlated pair-wise


library(car)
scatterplot(mbasalary.df$salary,mbasalary.df$age,main="Salary of MBAs  with Age",ylab = "Age in years", xlab="Salary",cex=1.1,pch=19)

scatterplot(mbasalary.df$salary,mbasalary.df$work_yrs,main="Salary of MBAs  with Work experience",ylab = "Work experience in years", xlab="Salary",cex=1.1,pch=19)

scatterplot(mbasalary.df$salary,mbasalary.df$gmat_tpc,main="Salary of MBAs  with GMAT Percentile",ylab = "GMAT Percentile %", xlab="Salary",cex=1.1,pch=19)


#Plots for binary categorical data with salaries of MBAs

plot(jitter(mbasalary.df$sex),jitter(mbasalary.df$salary),main="Salary of MBAs  with Sex",xlab = "Gender Male(1) Female(2)", ylab="Salary",cex=1.1)

plot(jitter(mbasalary.df$frstlang),jitter(mbasalary.df$salary),main="Salary of MBAs  with First Language",xlab = "FIrst Language English(1) Other(2)", ylab="Salary",cex=1.1)

plot(jitter(mbasalary.df$satis),jitter(mbasalary.df$salary),main="Salary of MBAs  with degree of satisfaction",xlab = "Degree of Satisfaction out of 7", ylab="Salary",cex=1.1)

#Scatterplot matrix
scatterplotMatrix(
  mbasalary.df[
    ,c("salary","work_yrs","gmat_tpc")], 
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix", diagonal = "histogram")

##Correlation tests to find relationship between different parameters 
# Correlation matrix,covariance matrix, Corrgram

corr.test(mbasalary.df, use = "complete")

x<-mbasalary.df[,c("age","sex","gmat_tot","gmat_qpc","gmat_vpc","gmat_tpc","s_avg","f_avg","quarter", "work_yrs", "frstlang", "salary","satis")]
y<-mbasalary.df[,c("salary","gmat_tpc","work_yrs","satis","age")]
cor(x,y)
cov(x,y)
var(x,y)

#Visualizing relation through corrplots

library(corrplot)
corrplot(corr=cor(mbasalary.df[,c(1:13)],use = "complete.obs"), method = "ellipse")
library(gplots)
corrplot.mixed(corr=cor(mbasalary.df[,c(1:13)],use = "complete.obs"), upper = "ellipse", tl.pos = "lt", col = colorpanel(50, "red", "gray60", "blue4"))

#VIsualizing by corrgram

library(corrgram)

corrgram(mbasalary.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of MBA Salaries")


#Generating Contingency table and performing chi-Square Test

mytable<-xtabs(~sex+work_yrs, data = mbasalary.df)
addmargins(mytable)
chisq.test(mytable)
##Because p value is more than 0.05 we can't reject the null hypothesis and 
## the parameters sex and work_yrs are independent.

mytable1<-xtabs(~work_yrs+satis, data = mbasalary.df)
addmargins(mytable1)
chisq.test(mytable1)
##Because p value is less than 0.05 we can reject the null hypothesis and 
## the parameters  are not independent.

mytable2<-xtabs(~sex+frstlang, data = mbasalary.df)
addmargins(mytable2)
chisq.test(mytable2)
##Because p value is more than 0.05 we can't reject the null hypothesis and 
## the parameters  are independent.

mytable3<-xtabs(~work_yrs+frstlang, data = mbasalary.df)
addmargins(mytable3)
chisq.test(mytable3)
##Because p value is less than 0.05 we can reject the null hypothesis and 
## the parameters  are not independent.

#T-Test
 #1.Average Salary of Males is greater than the average salaries of Females
t.test(salary~sex,alternative="greater",data=mbasalary.df)
##Because p value is more than 0.05 we can't reject the null hypothesis

  #2.Average Salary of people those who have English as their first language is greater than 
  #average salaries of those of speak other language
t.test(salary~frstlang, alternative="greater", data = mbasalary.df)
##Because p value is more than 0.05 we can't reject the null hypothesis 

  #3.Average GMAT Percentile of Males is greater than the  average GMAT percentile of Females
t.test(gmat_tpc~sex, alternative="greater", data = mbasalary.df)
##Because p value is more than 0.05 we can't reject the null hypothesis

#Generating a multiple linear regression model for MBAs Salaries
#1.
model1<-lm(salary~work_yrs+gmat_tot-1, data = mbasalary.df)
summary(model1)

  #Coefficents of the model
model1$coefficients
  #Residuals of the model
residuals(model1)
  #Fitting the model
fitted(model1)

###.  Model1:    salary = b0 + b1*Work_yrs + b2*gmat_tot
#   b0 = -1(assumption),  b1 = 3264.2887, b2=146.7158
#  Model:    salary = -1 + 3264.2887*work_yrs + 146.7158*gmat_tot


model2<-lm(salary~work_yrs+age*frstlang+gmat_tot+sex-1, data = mbasalary.df)
summary(model2)

#Coefficents of the model
model2$coefficients
#Residuals of the model
model2$fitted.values
#Fitting the model
model2$residuals

###.  Model2:    salary = b0 + b1*Work_yrs + b2*age +b3*frstlang +b4*gmat_tot + b5*sex + b6*age*frstlangg
#   b0 = -1(assumption),  b1 = -958.86681 , b2=2905.80137 ,b3=-15290.15225, b4=40.35853
# b5=-2260.92128, b6=794.41173
#  Model:    salary = -1 -958.86681*work_yrs + 2905.80137*age - 15290.15225*frstlang 
#+ 40.35853*gmat_tot - 2260.92128*sex +794.41173*age*frstlangg


#3.
model3<-lm(salary~work_yrs+age, data = mbasalary.df)

summary(model3)

#Coefficents of the model
model3$coefficients

#Residuals of the model
model3$fitted.values

#Fitting the model
model3$residuals

###.  Model3:    salary = b0 + b1*Work_yrs + b2*age
#   b0 = 36967.5,  b1 = 388.8, b2= 2413.8
#  Model:    salary = 36967.5 + 388.8*work_yrs + 2413.8*age

#4.
model4<-lm(salary~work_yrs+sex, data = mbasalary.df)

summary(model4)

#Coefficents of the model
model4$coefficients

#Residuals of the model
model4$fitted.values

#Fitting the model
model4$residuals

###.  Model4:    salary = b0 + b1*Work_yrs + b2*sex
#   b0 = 99676.944 ,  b1 = 2629.973, b2= -4860.589
#  Model:    salary = 99676.944  + 2629.973*work_yrs + -4860.589*sex

#Creating a subset of those who were not placed
mbasalary0.df<-mba.df[which(mba.df$salary<998),]
View(mbasalary0.df)

#Generating Contingency table and performing chi-Square Test

mytable<-xtabs(~sex+work_yrs, data = mbasalary0.df)
addmargins(mytable)
chisq.test(mytable)
##Because p value is more than 0.05 we can't reject the null hypothesis and 
## the parameters sex and work_yrs are independent.

mytable2<-xtabs(~sex+frstlang, data = mbasalary0.df)
addmargins(mytable2)
chisq.test(mytable2)
##Because p value is more than 0.05 we can't reject the null hypothesis and 
## the parameters sex and frstlang are independent.


#CHALENGE ACCEPTED#

#Generating model for those who got placed
mbasalary.df$sex <- factor(mbasalary.df$sex)
is.factor(mbasalary.df$sex)
fit1 <- glm(sex~., family = binomial(link = 'logit'), data = mbasalary.df)
summary(fit1)
##Now we can analyze the fitting and interpret what the model is telling us.

##Now we can run the anova() function on the model to analyze the table of deviance
anova(fit1, test = "Chisq")

fitted.results <- predict(fit1,data=mbasalary.df,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != mbasalary.df$sex)
print(paste('Accuracy',1-misClasificError))



#Generating model for those who got placed
mbasalary0.df$sex <- factor(mbasalary0.df$sex)
is.factor(mbasalary0.df$sex)
fit2 <- glm(sex~., family = binomial(link = 'logit'), data = mbasalary0.df)
summary(fit2)
##Now we can analyze the fitting and interpret what the model is telling us.

##Now we can run the anova() function on the model to analyze the table of deviance
anova(fit2, test = "Chisq")


#TO test the accuracy
fitted.results <- predict(fit2,data=mbasalary0.df,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1)

misClasificError <- mean(fitted.results != mbasalary0.df$sex)
print(paste('Accuracy',1-misClasificError))

