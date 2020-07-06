
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)
library(lattice)
library(latticeExtra)

data=read.csv(file.choose())

dim(data)
str(data)
summary(data)

data$Number.of.Open.Complaints<-as.factor(data$Number.of.Open.Complaints)
data$Number.of.Policies<-as.factor(data$Number.of.Policies)


colnames(data)[which(names(data)=="Customer.Lifetime.Value")]="clv"

options(scipen = 999)

quantile(data$clv,seq(0,1,0.05))
boxplot(data$clv)

data1<-data[data$clv<12000,]
boxplot(data1$clv)


boxplot(data1$Income)
boxplot(data1$Monthly.Premium.Auto)
quantile(data1$Monthly.Premium.Auto,seq(0,1,0.05))
data1<-data1[data1$Monthly.Premium.Auto<135,]
boxplot(data1$Monthly.Premium.Auto)

boxplot(data1$Months.Since.Last.Claim)
boxplot(data$Months.Since.Policy.Inception)
boxplot(data1$Total.Claim.Amount)

quantile(data1$Total.Claim.Amount,seq(0,1,0.05))
data1<-data1[data1$Total.Claim.Amount<784,]

nrow(data)
nrow(data1)

sapply(data1,function(x)sum(is.na(x)))

table(data1$Response)

as.data.frame(colnames(data1))
Insurance<-select(data1,-c(Customer,Effective.To.Date))

#visualization
ggplot(Insurance)+geom_histogram(aes(clv),binwidth = 70,color="Blue")+theme_minimal()+labs(title = " Graphical Representation of Customer Life time Value ",x="CLV",y="Count")


set.seed(123)
spl=sample.split(Insurance$clv,0.7)

train.data <-subset(Insurance,spl==TRUE)
test.data <-subset(Insurance,spl==FALSE)

nrow(train.data)
nrow(test.data)


model1<-lm(clv~.,data=train.data)
summary(model1)

attach(Insurance)

model2<-lm(clv~Response+Coverage+Education+EmploymentStatus+
             Gender
           +Income
           +Marital.Status
           +Monthly.Premium.Auto
           +Months.Since.Last.Claim
           +Months.Since.Policy.Inception
           +Number.of.Open.Complaints
           +Number.of.Policies
           +Policy.Type+Policy
           +Renew.Offer.Type
           +Sales.Channel
           +Total.Claim.Amount
           +Vehicle.Class
           +Vehicle.Size
           ,data = train.data)
summary(model2)

model3<-lm(clv~Education+EmploymentStatus+
             Gender
           +Income
           +Marital.Status
           +Monthly.Premium.Auto
           +Months.Since.Last.Claim
           +Number.of.Open.Complaints
           +Number.of.Policies+Policy.Type
           +I(Policy=="Corporate L2")+I(Policy=="Corporate L3")
           +Total.Claim.Amount
           +Vehicle.Class
           +Vehicle.Size
           ,data = train.data)
summary(model3)

model4<-lm(clv~Education+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Unemployed")
            +Income+Gender
           +Marital.Status
           +Monthly.Premium.Auto
           +Months.Since.Last.Claim
           +Number.of.Open.Complaints
           +Number.of.Policies
           +Policy.Type
           +I(Policy=="Corporate L2")+I(Policy=="Corporate L3")
           +Total.Claim.Amount
           +I(Vehicle.Class=="SUV")
           ,data = train.data)
summary(model4)

vif(model4)

model5<-lm(clv~Education+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Unemployed")
           +Income+Gender
           +Marital.Status
           +Monthly.Premium.Auto
           +Months.Since.Last.Claim
           +Number.of.Open.Complaints
           +Number.of.Policies
           ,data = train.data)
summary(model5)
vif(model5)

model6<-lm(clv~I(Education=="Master")
           +I(EmploymentStatus=="Employed")
           +I(EmploymentStatus=="Unemployed")
           +Income+Gender
           +I(Marital.Status=="Single")
           +Monthly.Premium.Auto
           +Number.of.Open.Complaints
           +Number.of.Policies
           ,data = train.data)
summary(model6)

vif(model6)

fitted(model6)



xyplot(clv~Gender,data = Insurance,xlab = "Gender",ylab = "Clv")
xyplot(clv~Education,data = Insurance,xlab = "Education",ylab = "Clv")
xyplot(clv~EmploymentStatus,data = Insurance,xlab = "Employment Status",ylab = "Clv")
xyplot(clv~Income,data = Insurance,xlab = "Income",ylab = "Clv")
xyplot(clv~Marital.Status,data = Insurance,xlab = "Marital Status",ylab = "Clv")
xyplot(clv~Monthly.Premium.Auto,data = Insurance,xlab = "Monthly Premium Auto",ylab = "Clv")
xyplot(clv~Number.of.Open.Complaints,data = Insurance,xlab = "Number of Open Complaints",ylab = "Clv")
xyplot(clv~Number.of.Policies,data = Insurance,xlab = "Number of Policies",ylab = "Clv")


par(mfrow=c(2,2))
plot(model6)


options(scipen = 999)
train.data$pred <-fitted(model6)
attach(train.data)
mape1 <-print(((sum((abs(train.data$clv-train.data$pred))/train.data$clv))/nrow(train.data)))
mape1
rmse1 <-print(sqrt(mean(train.data$pred-train.data$clv)^2))
rmse1


ggplot(train.data,aes(x=clv,y=pred))+geom_point(col="steelblue",size=1)+geom_smooth(method = "lm",col="Red")+coord_cartesian(ylim = c(1000,15000))+labs(title = "Actual Vs Predicted",x="Customer Lifetime Value Actual",y="Customer Lifetime Value Predicted")

durbinWatsonTest(model6)

bptest(model6)

resids <- model6$residuals
par(mfrow=c(1,1))
plot(density(resids))
ggplot()+geom_density(aes(resids))


ad.test(resids)
qqnorm(resids)

#testing on test data

model<-lm(clv~I(Education=="Master")
           +I(EmploymentStatus=="Employed")
           +I(EmploymentStatus=="Unemployed")
           +Income+Gender
           +I(Marital.Status=="Single")
           +Monthly.Premium.Auto
           +Number.of.Open.Complaints
           +Number.of.Policies
           ,data = test.data)
summary(model)
vif(model)

fitted(model)

par(mfrow=c(2,2))
plot(model)


options(scipen = 999)
test.data$pred <-fitted(model)
attach(test.data)
mape1 <-print(((sum((abs(test.data$clv-test.data$pred))/test.data$clv))/nrow(test.data)))
mape1
rmse1 <-print(sqrt(mean(test.data$pred-test.data$clv)^2))
rmse1

durbinWatsonTest(model)

bptest(model)

resids <- model$residuals
par(mfrow=c(1,1))
plot(density(resids))
ggplot()+geom_density(aes(resids))


ad.test(resids)
qqnorm(resids)

