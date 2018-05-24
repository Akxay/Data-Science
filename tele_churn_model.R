
setwd( "E:\\Akxay\\GLIM\\Predictive Analytics")
tele_data<-read.csv(file.choose(), header=T)
View(tele_data)
str(tele_data)

library(caret)
library(car)
library(psych)
library(e1071)
library(plyr)

colnames(tele_data)

## Target Rate
sum(tele_data$Churn)/nrow(tele_data) # 14% churn rate

tele_data$Churn = as.factor(tele_data$Churn)
tele_data$ContractRenewal = as.factor(tele_data$ContractRenewal)
tele_data$DataPlan = as.factor(tele_data$DataPlan)

table(tele_data$Churn,tele_data$DataPlan)
# 403 out of 483 churn goes not have any data plan and 80 of them has
table(tele_data$Churn,tele_data$ContractRenewal)
# 137 out of 323 who do not renew contract churns

# impact of DataPlan and MonthlyCharge on customer churn
boxplot(tele_data$MonthlyCharge ~ tele_data$Churn)
title(xlab="Churn", ylab="MonthlyCharge",main="Distribution of Montly Charge", line=2,cex.lab=1)

boxplot(tele_data$DayMins ~ tele_data$Churn)
title(xlab="Churn", ylab="DayMins",main="Distribution of Day Mins", line=2,cex.lab=1)

boxplot(tele_data$CustServCalls ~ tele_data$Churn)
title(xlab="Churn", ylab="CustServCalls",main="Distribution of Customer Service calls", line=2,cex.lab=1)

qplot(DayMins, MonthlyCharge , colour = DataPlan, data=tele_data)

qplot(DayMins,  DataPlan, colour = Churn,data=tele_data)
qplot(DayMins,  DataUsage, colour = Churn,data=tele_data)
# but among those who dont use data plan but still charged high are  churning

# create 3D point color

library(scatterplot3d)

#Binary Classification Plots: tele_data vs No tele_data

tele_data$pcolor[tele_data$Churn==1] <- "red"
tele_data$pcolor[tele_data$Churn==0] <- "blue"

with(tele_data, {
  
  s3d <- scatterplot3d(DayMins, CustServCalls, DataUsage,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates tele_data or not
                       main="3-D Scatterplot: Using Churn Data ",
                       xlab="DayMins",
                       ylab="Customer Service Calls",
                       zlab=" DataUsage in GB")
  s3d.coords <- s3d$xyz.convert(DayMins, CustServCalls, DataUsage)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Churn or Not",
         c("1","0"), fill=c("red", "blue"))
})

# So customers paying high bills and not using much of data and acnt not active for long are churning



#Scatter Plots and Correlation

#pairs.panels(tele_data, gap=0, bg=c("red", "blue")[tele_data$Churn], pch=21)
levels(tele_data$Churn)

# Partition the data

summary(tele_data$Churn)

set.seed(1111)
pd<-sample(2,nrow(tele_data),replace=TRUE, prob=c(0.7,0.3))
train<-tele_data[pd==1,]
val<-tele_data[pd==2,]
summary(train$Churn)
summary(val$Churn)


library(SDMTools)
library(pROC)
library(Hmisc)


Logit<-Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins

#All Variables

Logit.1 <- glm(Logit   , train, family = binomial)
summary(Logit.1)
vif(Logit.1)

Logit.new<-Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+OverageFee+RoamMins
Logit.2 <- glm(Logit.new  , train, family = binomial)
summary(Logit.2)
vif(Logit.2)

Logit.3<-Churn~AccountWeeks+ContractRenewal+DataUsage+CustServCalls+DayMins+DayCalls+OverageFee+RoamMins
Logit.3 <- glm(Logit.3  , train, family = binomial)
summary(Logit.3)
vif(Logit.3)

Logit.4<-Churn ~ ContractRenewal+DataUsage+CustServCalls+DayMins+OverageFee+RoamMins
Logit.4 <- glm(Logit.4  , train, family = binomial)
summary(Logit.4)
vif(Logit.4)

pred.logit <- predict.glm(Logit.4, newdata=val, type="response")

# Rank ordering
## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
val$deciles <- decile(pred.logit)
View(val)

## Ranking code
##install.packages("data.table")
library(data.table)
val$Churn = as.numeric(as.character(val$Churn))
tmp_DT = data.table(val)
rank <- tmp_DT[, list(
  cnt = length(Churn), 
  cnt_resp = sum(Churn), 
  cnt_non_resp = sum(Churn == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);

View(rank)

# confusion Matrix and ROC curves

x = cbind(val,pred.logit)
churn = (subset(x, x$Churn==1))
summary(churn$pred.logit)
not_churn = (subset(x, x$Churn==0))
summary(not_churn$pred.logit)

df=x[,c(14,1)]
# Churn vs not churn predicted probablities
ggplot(df, aes(pred.logit, fill = as.factor(Churn))) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(df, aes(pred.logit, fill = as.factor(Churn))) + geom_density(alpha = 0.1)

## The distribution of the estimated probabilities
quantile(churn$pred.logit, c(0,1,5,10,25,30,35,50,80,86,90,95,100)/100)

tab.logit<-confusion.matrix(val$Churn,pred.logit,threshold = 0.22)
tab.logit
roc.logit<-roc(val$Churn,pred.logit )
roc.logit
plot(roc.logit)

#Recall- percent of the Churn cases did we catch? 
# 94/151 = 63%
#Precision = percent of Churn predictions were correct?
# 94/218 = 44%