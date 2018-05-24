
## Let us first set the working directory path
setwd ("E:\\Akxay\\GLIM\\Data Mining\\CART\\datafile")
getwd()



## Data Import
#CTDF.dev <- read.table("DEV_SAMPLE.csv", sep = ",", header = T)
#CTDF.holdout <- read.table("HOLDOUT_SAMPLE.csv", sep = ",", header = T)

CTDF.data = read.table("HR_Employee_Attrition_Data.csv",sep= ",", header = T)
CTDF.data$Attrition = factor(CTDF.data$Attrition, levels = c("No","Yes"), labels=c(0,1))
CTDF.data$Attrition = as.numeric(as.character(CTDF.data$Attrition))
View(CTDF.data)
# split data into dev and holdout
set.seed(1001)
#n = nrow(CTDF.data)
#trainIndex = sample(1:n, size = round(0.7*nrow(CTDF.data)), replace=F)
#CTDF.dev = CTDF.data[trainIndex ,]
#CTDF.holdout = CTDF.data[-trainIndex ,]
CTDF.data$random = runif(nrow(CTDF.data),0,1)
CTDF.dev = CTDF.data[which(CTDF.data$random <= 0.7),]
CTDF.holdout = CTDF.data[which(CTDF.data$random > 0.7),]
c(nrow(CTDF.dev), nrow(CTDF.holdout))
str(CTDF.dev)

## loading the library
library(rpart)
library(rpart.plot)

## Target Rate
sum(CTDF.dev$Attrition)/nrow(CTDF.dev)
sum(CTDF.holdout$Attrition)/nrow(CTDF.holdout)

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=60, minbucket = 20, cp = 0, xval = 10)


## calling the rpart function to build the tree
##m1 <- rpart(formula = Target ~ ., data = CTDF.dev[which(CTDF.dev$Holding_Period>10),-1], method = "class", control = r.ctrl)
m1 <- rpart(formula = CTDF.dev$Attrition~ ., data = CTDF.dev[,-c(2,9,10,22,27)], method = "class", control = r.ctrl)
m1
prp(m1,tweak=2.2,compress=TRUE,faclen=4,varlen=8)


## install.packages("rattle")
## install.packages("RcolorBrewer")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)


## to find how the tree performs
printcp(m1)
plotcp(m1)
cp=m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"]

##rattle()
## Pruning Code
ptree<- prune(m1, cp= cp ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")
prp(ptree,tweak=2.2,compress=TRUE,faclen=2,varlen=8)


## Let's use rattle to see various model evaluation measures
##rattle()

View(CTDF.dev)
## Scoring syntax
CTDF.dev$predict.class <- predict(ptree, CTDF.dev, type="class")
CTDF.dev$predict.score <- predict(ptree, CTDF.dev)

View(CTDF.dev)
head(CTDF.dev)


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

class(CTDF.dev$predict.score)
## deciling
CTDF.dev$deciles <- decile(CTDF.dev$predict.score[,2])
View(CTDF.dev)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(CTDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);

View(rank)



##install.packages("ROCR")
library(ROCR)
pred <- prediction(CTDF.dev$predict.score[,2], CTDF.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

##install.packages("ineq")
library(ineq)
gini = ineq(CTDF.dev$predict.score[,2], type="Gini")

with(CTDF.dev, table(Attrition, predict.class))
auc
KS
gini

View(rank)
## Syntax to get the node path
#tree.path <- path.rpart(ptree, node = c(26, 27))

nrow(CTDF.holdout)

## Scoring Holdout sample
CTDF.holdout$predict.class <- predict(ptree, CTDF.holdout, type="class")
CTDF.holdout$predict.score <- predict(ptree, CTDF.holdout)


CTDF.holdout$deciles <- decile(CTDF.holdout$predict.score[,2])
View(CTDF.holdout)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(CTDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_perct_resp <- round(h_rank$cum_resp * 100 / sum(h_rank$cnt_resp),2);
h_rank$cum_perct_non_resp <- round(h_rank$cum_non_resp * 100 / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_perct_resp - h_rank$cum_perct_non_resp);

View(h_rank)

# AUC and gini for test sample
library(ROCR)
pred <- prediction(CTDF.holdout$predict.score[,2], CTDF.holdout$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

##install.packages("ineq")
library(ineq)
gini = ineq(CTDF.holdout$predict.score[,2], type="Gini")

with(CTDF.holdout, table(Attrition, predict.class))
auc
KS
gini


with(CTDF.holdout, table(Attrition, predict.class))

write.csv(CTDF.dev,"Emp_attrition_dev_sample_1.csv")
write.csv(CTDF.holdout, "Emp_attrition_holdout_sample_1.csv")
