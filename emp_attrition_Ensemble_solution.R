library(ineq)
library(car)
library(caret)
library(rpart)
library(randomForest)
library(RColorBrewer)
library(rpart.plot)
library(rattle)
library(caTools)
library(gplots)
library(ROCR) 

setwd ("E:/Akxay/GLIM/Data Mining/Assignment")
getwd()

## Data Import
data <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
dim(data)

str(data)
summary(data)

## Converting Data to variables for further EDA
data$Attrition <- as.factor(data$Attrition)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Department <- as.factor(data$Department)
data$Education <- as.factor(data$Education)
data$EducationField <- as.factor(data$EducationField)
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
data$Gender <- as.factor(data$Gender)
data$JobInvolvement <- as.factor(data$JobInvolvement)
data$JobLevel <- as.factor(data$JobLevel)
data$JobRole <- as.factor(data$JobRole)
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$NumCompaniesWorked <- as.factor(data$NumCompaniesWorked)
data$OverTime <- as.factor(data$OverTime)
data$PerformanceRating <- as.factor(data$PerformanceRating)
data$RelationshipSatisfaction <- as.factor(data$RelationshipSatisfaction)
data$StockOptionLevel <- as.factor(data$StockOptionLevel)
data$TrainingTimesLastYear <- as.factor(data$TrainingTimesLastYear)
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)
data$Flag[data$Attrition == "Yes"] <- 1
data$Flag[data$Attrition == "No"] <- 0
data$Flag <- as.factor(data$Flag)

# Plotting univariate summaries in the data 
par(mfrow= c(1,2))

par(mfrow= c(1,2))
barplot(table(data$OverTime,data$Flag),main="Effect of Overtime", 
        xlab="Attrition", ylab="Attrition Frequency",
        legend = rownames(table(data$OverTime,data$Flag)),beside=TRUE)
barplot(table(data$StockOptionLevel,data$Flag),main="Effect of StockOptionLevel", 
        xlab="Attrition", ylab="Attrition Frequency",
        legend = rownames(table(data$StockOptionLevel,data$Flag)),beside=TRUE)

par(mfrow= c(1,2))
barplot(table(data$MaritalStatus,data$Flag),main="Effect of MaritalStatus", 
        xlab="Attrition", ylab="Attrition Frequency",
        legend = rownames(table(data$MaritalStatus,data$Flag)),beside=TRUE)
barplot(table(data$JobLevel,data$Flag),main="Effect of JobLevel", 
        xlab="Attrition", ylab="Attrition Frequency",
        legend = rownames(table(data$JobLevel,data$Flag)),beside=TRUE)

par(mfrow= c(1,2))
boxplot(data$Age~data$Flag,data=data, main="Effect of Age", 
        xlab="Attrition", ylab="Age")
boxplot(data$MonthlyIncome ~data$Flag,data=data, main="Effect of MonthlyIncome", 
        xlab="Attrition", ylab="MonthlyIncome")

#Gender vs attrition
barplot(table(data$Gender,data$Flag),
        legend = rownames(table(data$Gender,data$Flag)),beside=TRUE)
# Business Travel vs attrition
barplot(table(data$BusinessTravel,data$Flag),
        legend = rownames(table(data$BusinessTravel,data$Flag)),beside=TRUE)

# Department vs attrition
par(mfrow= c(1,2))
barplot(table(data$Department,data$Flag),
        legend = rownames(table(data$Department,data$Flag)),beside=TRUE)

# Education Field vs attrition
barplot(table(data$EducationField,data$Flag),
        legend = rownames(table(data$EducationField,data$Flag)),beside=TRUE)

par(mfrow= c(1,2))

# EnvironmentSatisfaction vs attrition
barplot(table(data$EnvironmentSatisfaction,data$Flag),
        legend = rownames(table(data$EnvironmentSatisfaction,data$Flag)),beside=TRUE)
# JobSatisfaction vs attrition
barplot(table(data$JobSatisfaction,data$Flag),
        legend = rownames(table(data$JobSatisfaction,data$Flag)),beside=TRUE)

par(mfrow= c(1,2))

#RelationshipSatisfaction vs attrition
barplot(table(data$RelationshipSatisfaction,data$Flag),
        legend = rownames(table(data$RelationshipSatisfaction,data$Flag)),beside=TRUE)

# WorkLifeBalance vs attrition
barplot(table(data$WorkLifeBalance,data$Flag),
        legend = rownames(table(data$WorkLifeBalance,data$Flag)),beside=TRUE)

# TrainingTimesLastYear vs attrition

par(mfrow= c(1,2))
barplot(table(data$TrainingTimesLastYear,data$Flag),
        legend = rownames(table(data$TrainingTimesLastYear,data$Flag)),beside=TRUE)

#StockOptionLevel vs attrition
barplot(table(data$StockOptionLevel,data$Flag),
        legend = rownames(table(data$StockOptionLevel,data$Flag)),beside=TRUE)

par(mfrow= c(1,2))
#PerformanceRating VS ATTRITION
barplot(table(data$PerformanceRating,data$Flag),
        legend = rownames(table(data$PerformanceRating,data$Flag)),beside=TRUE)
#NumCompaniesWorked vs attrition
barplot(table(data$NumCompaniesWorked,data$Flag),
        legend = rownames(table(data$NumCompaniesWorked,data$Flag)),beside=TRUE)

par(mfrow= c(1,2))
barplot(table(data$MaritalStatus,data$Flag),
        legend = rownames(table(data$MaritalStatus,data$Flag)),beside=TRUE)
barplot(table(data$JobRole,data$Flag),
        legend = rownames(table(data$JobRole,data$Flag)),beside=TRUE)
barplot(table(data$JobInvolvement,data$Flag),
        legend = rownames(table(data$JobInvolvement,data$Flag)),beside=TRUE)
par(mfrow= c(1,2))
barplot(table(data$JobLevel,data$Flag),
        legend = rownames(table(data$JobLevel,data$Flag)),beside=TRUE)



# split data into dev and holdout

data$Attrition = factor(data$Attrition, levels = c("No","Yes"), labels=c(0,1))
data$Attrition = as.numeric(as.character(data$Attrition))
set.seed(1001)

data$random = runif(nrow(data),0,1)
RFDF.dev = data[which(data$random <= 0.7),]
RFDF.holdout = data[which(data$random > 0.7),]
c(nrow(RFDF.dev), nrow(RFDF.holdout))

## Target Rate
sum(RFDF.dev$Attrition)/nrow(RFDF.dev)
sum(RFDF.holdout$Attrition)/nrow(RFDF.holdout)

# Building Random Forest

library(randomForest)
?randomForest
View(RFDF.dev)
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(RFDF.dev$Attrition) ~ ., data = RFDF.dev[,-c(2,9,10,22,27,36,37)], 
                   ntree=51, mtry = 9, nodesize = 60,
                   importance=TRUE, set.seed(1))


print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate

## List the importance of the variables.
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]


?tuneRF
## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(2,9,10,22,27,36,37)], 
              y=as.factor(RFDF.dev$Attrition),
              mtryStart = 2, 
              ntreeTry=51, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = FALSE,
              doBest = TRUE,
              nodesize = 60, 
              importance=TRUE,
              set.seed(1)
)

(tRF$importance)

(VI_F=importance(RF))
varImpPlot(RF,type=2)

View(RFDF.dev)
## Scoring syntax
RFDF.dev$predict.class.RF <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score.RF <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
class(RFDF.dev$predict.score.RF)

## deciling
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


RFDF.dev$deciles <- decile(RFDF.dev$predict.score.RF[,2])


library(data.table)
tmp_DT = data.table(RFDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) ,
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

sum(RFDF.dev$Attrition) / nrow(RFDF.dev)


library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(RFDF.dev$predict.score.RF[,2], type="Gini")
gini

## Classification Error
with(RFDF.dev, table(Attrition, predict.class.RF))


## Scoring syntax
RFDF.holdout$predict.class.RF <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score.RF <- predict(tRF, RFDF.holdout, type="prob")

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score.RF[,2])

tmp_DT = data.table(RFDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)

# AUC and gini for test sample
library(ROCR)
pred <- prediction(RFDF.holdout$predict.score.RF[,2], RFDF.holdout$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

##install.packages("ineq")
library(ineq)
gini = ineq(RFDF.holdout$predict.score.RF[,2], type="Gini")

with(RFDF.holdout, table(Attrition, predict.class.RF))
auc
KS
gini


with(RFDF.holdout, table(Attrition, predict.class.RF))



#-----------------------------------------------------#
# Building Neural Network
# split data into dev and holdout

#get categorical variables
factors.df = (data[, sapply(data, is.factor)])
#get levels of each category
lapply(factors.df, levels)

#creating dummy variables of categorical variables for NN model

#occ.matrix <- model.matrix(~ Occupation - 1, data = nn.dev)
#nn.dev <- data.frame(nn.dev, occ.matrix)

library(dummies)
data.new <- dummy.data.frame(data[,-c(9,10,22,27,36)], sep = ".")
colnames(data.new) = make.names(colnames(data.new), unique=TRUE)
View(data.new)

set.seed(1001)

data.new$random = runif(nrow(data.new),0,1)
nn.dev = data.new[which(data.new$random <= 0.7),]
nn.holdout = data.new[which(data.new$random > 0.7),]
c(nrow(nn.dev), nrow(nn.holdout))

## Target Rate
sum(nn.dev$Attrition)/nrow(nn.dev)
sum(nn.holdout$Attrition)/nrow(nn.holdout)



library(neuralnet)
library(nnet)
?"neuralnet"

#scaled data
nn.devscaled <- scale(nn.dev[,-c(2,95)])
nn.devscaled <- cbind(nn.dev[2], nn.devscaled)
View(nn.devscaled)

set.seed(1111)
names <- c(colnames(nn.devscaled)) #choose the names you want
a <- as.formula(paste('nn.devscaled$Attrition ~ ' ,paste(names[-1],collapse='+')))

nn2 <- neuralnet(formula = a,
                 data = nn.devscaled[,-1], 
                 hidden = c(10,3),
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 1,
                 threshold = 0.1,
                 stepmax = 2000
                 ##startweights = startweightsObj
)



plot(nn2)


## Assigning the Probabilities to Dev Sample
nn.dev$Prob.nn = nn2$net.result[[1]] 


## The distribution of the estimated probabilities
quantile(nn.dev$Prob.nn, c(0,1,5,10,25,50,75,85,90,95,99,100)/100)
hist(nn.dev$Prob.nn)


## deciling
nn.dev$deciles <- decile(nn.dev$Prob.nn)


## Ranking code

tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)
?read.table

## Assgining 0 / 1 class based on certain threshold
nn.dev$Class.nn = ifelse(nn.dev$Prob.nn>0.1,1,0)
with( nn.dev, table(Attrition, as.factor(Class.nn)  ))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
library(caret)
confusionMatrix(nn.dev$Attrition, nn.dev$Class.nn)


## Error Computation
sum((nn.dev$Attrition - nn.dev$Prob.nn)^2)/2

## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute

#scaled data
x.scaled <- scale(nn.holdout[,-c(2,95)])
View(x.scaled)

compute.output = compute(nn2, x.scaled)
nn.holdout$Predict.score.nn = compute.output$net.result
View(nn.holdout)

quantile(nn.holdout$Predict.score.nn, c(0,1,5,10,25,50,75,85,90,95,99,100)/100)
nn.holdout$deciles <- decile(nn.holdout$Predict.score.nn)

library(data.table)
tmp_DT = data.table(nn.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)

## Assgining 0 / 1 class based on certain threshold
nn.holdout$Class.nn = ifelse(nn.holdout$Predict.score.nn>0.1,1,0)
with( nn.holdout, table(Attrition, as.factor(Class.nn)  ))


#--------------------------------------------------------------------------

# Ensemble Modeling- Combining Random Forest and Neural Network
final.df = cbind(RFDF.dev,nn.dev[,c(96,98,97)])
final.df$prob.score.nn = 1-final.df$Prob.nn[,1]
final.df$avg.score = (final.df$prob.score.nn + final.df$predict.score.RF[,1])/2

View(final.df)

quantile(final.df$avg.score, c(0,1,5,10,25,50,75,85,90,95,99,100)/100)
final.df$deciles.final <- decile(final.df$avg.score)

## Assgining 0 / 1 class based on certain threshold
final.df$final.class = ifelse(final.df$avg.score<0.1,1,0)
with( final.df, table(Attrition, as.factor(final.class)  ))

#holdout sample
final.df.holdout = cbind(RFDF.holdout,nn.holdout[,c(96,98,97)])
View(final.df.holdout)
final.df.holdout$prob.score.nn = 1-final.df.holdout$Predict.score.nn[,1]
final.df.holdout$avg.score = (final.df.holdout$prob.score.nn + final.df.holdout$predict.score.RF[,1])/2

View(final.df.holdout)

quantile(final.df.holdout$avg.score, c(0,1,5,10,25,50,75,85,90,95,99,100)/100)
final.df.holdout$deciles.final <- decile(final.df.holdout$avg.score)

## Assgining 0 / 1 class based on certain threshold
final.df.holdout$final.class = ifelse(final.df.holdout$avg.score<0.1,1,0)
with( final.df.holdout, table(Attrition, as.factor(final.class)  ))


# ensemble output does not outperform the Individual Neaural Network Model. 

# function to get dummy variables

createDummyVariables = function(dataframe){
  BusinessTravel.matrix = model.matrix(~ BusinessTravel - 1, data = dataframe)
  dataframe$BusinessTravel=NULL
  Department.matrix = model.matrix(~ Department - 1, data = dataframe)
  dataframe$Department = NULL
  Education.matrix = model.matrix(~ Education - 1, data = dataframe)
  dataframe$Education = NULL
  EducationField.matrix = model.matrix(~ EducationField - 1, data = dataframe)
  dataframe$EducationField=NULL
  EnvironmentSatisfaction.matrix = model.matrix(~ EnvironmentSatisfaction - 1, data = dataframe)
  dataframe$EnvironmentSatisfaction=NULL
  Gender.matrix = model.matrix(~ Gender - 1, data = dataframe)
  dataframe$Gender=NULL
  JobInvolvement.matrix = model.matrix(~ JobInvolvement - 1, data = dataframe)
  dataframe$JobInvolvement=NULL
  JobLevel.matrix = model.matrix(~ JobLevel - 1, data = dataframe)
  dataframe$JobLevel=NULL
  JobRole.matrix = model.matrix(~ JobRole - 1, data = dataframe)
  dataframe$JobRole=NULL
  JobSatisfaction.matrix = model.matrix(~ JobSatisfaction - 1, data = dataframe)
  dataframe$JobSatisfaction=NULL
  MaritalStatus.matrix = model.matrix(~ MaritalStatus - 1, data = dataframe)
  dataframe$MaritalStatus=NULL
  NumCompaniesWorked.matrix = model.matrix(~ NumCompaniesWorked - 1, data = dataframe)
  dataframe$NumCompaniesWorked = NULL
  OverTime.matrix = model.matrix(~ OverTime - 1, data = dataframe)
  dataframe$OverTime=NULL
  PerformanceRating.matrix = model.matrix(~ PerformanceRating - 1, data = dataframe)
  dataframe$PerformanceRating=NULL
  RelationshipSatisfaction.matrix = model.matrix(~ RelationshipSatisfaction - 1, data = dataframe)
  dataframe$RelationshipSatisfaction=NULL
  StockOptionLevel.matrix = model.matrix(~ StockOptionLevel - 1, data = dataframe)
  dataframe$StockOptionLevel=NULL
  TrainingTimesLastYear.matrix = model.matrix(~ TrainingTimesLastYear - 1, data = dataframe)
  dataframe$TrainingTimesLastYear=NULL
  WorkLifeBalance.matrix = model.matrix(~ WorkLifeBalance - 1, data = dataframe)
  dataframe$WorkLifeBalance=NULL
  dataframe$Flag=NULL
  dataframe$Over18 = NULL
  
  return (data.frame(dataframe, BusinessTravel.matrix, Department.matrix,
                     Education.matrix, EducationField.matrix, EnvironmentSatisfaction.matrix,
                     Gender.matrix, JobInvolvement.matrix, JobLevel.matrix,
                     JobRole.matrix, JobSatisfaction.matrix, MaritalStatus.matrix,
                     NumCompaniesWorked.matrix, OverTime.matrix, PerformanceRating.matrix,
                     RelationshipSatisfaction.matrix, StockOptionLevel.matrix, 
                     TrainingTimesLastYear.matrix, WorkLifeBalance.matrix))
}
dim(createDummyVariables(data))