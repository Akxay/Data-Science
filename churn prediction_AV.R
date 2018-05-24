path = "E:/Akxay/GLIM/Finance and risk analytics/personal loan_AV_hackathon/churn pred/train_zsTMYVA/train.csv"
#test_path = "/Users/sahilkumar/Downloads/train_zsTMYVA/train.csv"
data = read.csv(path)
train = data
test = read.csv(test_path)
dim(test)
dim(train)

count_NA <- function(dataFrame) {
  df=data.frame()
  for (n in 1:ncol(dataFrame)){
    df[n,1]=colnames(dataFrame)[n]
    df[n,2]=sum(is.na(dataFrame[n]))
    names(df) <- c("colName","NA_Count")
  }
  return (df[order(-df$NA_Count),])
}

View(count_NA(test))

NA_df=count_NA(train)
train_notnull = (subset(NA_df, NA_df$NA_Count == 0))
dim(train_notnull)

train_notnull = (train[,c(train_notnull$colName)])

# Create df of Numeric Columns
get_num_columns <- function(dataFrame) {
  num_cols=sapply(dataFrame, is.numeric)
  return (dataFrame[ , num_cols])
}

# Create df of Categorical Columns
get_cat_columns <- function(dataFrame) {
  cat_cols=sapply(dataFrame, is.factor)
  return (dataFrame[ , cat_cols])
}

# numeric columns dataframe
num_cols = get_num_columns(train_notnull)
dim(num_cols)

# Categorical columns dataframe
cat_cols = get_cat_columns(train_notnull)
dim(cat_cols)
colnames((cat_cols))

# 2. Unique categories count
unique_cat_count <- function(dataFrame) {
  df=data.frame(rapply(dataFrame[,unlist(lapply(dataFrame, is.factor))],function(x)length(unique(x))))
  df=data.frame(as.factor(rownames(df)),df)
  colnames(df)[1]="cat_columns"
  colnames(df)[2]="cat_count"
  row.names(df) <- NULL
  return (df[order(-df[,2]),])
}
#View(unique_cat_count(train_notnull))

#percentage of Responders vs non-responsders
sum(train_notnull$Responders)/dim(train_notnull)[1] #-17% Responders

### t-test on numeric variables
#train_data = train_notnull[,-c(1, 7, 236, 237)] #id,city, responders, random

chi_test <- function(target_column, cat_column) {
  chitest = 
    return (chitest)
}
#install.packages("gmodels")
#library(gmodels)
#chi square test
chi_test_fun <- function(dataFrame, Target_col) {
  df=data.frame()
  for (n in 1:ncol(dataFrame)){
    df[n,1]=colnames(dataFrame)[n]
    chitest = CrossTable(Target_col, dataFrame[[n]], digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    pval = chitest$chisq$p.value
    df[n,2] = pval
    df[n,3] = ifelse((pval < 0.05), 1, 0)
    names(df) <- c("colName","pval","significance")
  }
  return (df)
}

chitest_df = (chi_test_fun(cat_cols[,-4], as.factor(num_cols$Responders)))
#View(chitest_df)

dim(chitest_df)
chitest_df = subset(chitest_df, chitest_df$significance==1)

ttest <- function(dataFrame, Target_col) {
  df=data.frame()
  for (n in 1:ncol(dataFrame)){
    df[n,1]=colnames(dataFrame)[n]
    pval = t.test(dataFrame[[n]] ~ Target_col)$p.value
    df[n,2] = pval
    df[n,3] = ifelse((pval < 0.05), 1, 0)
    names(df) <- c("colName","pval","significance")
  }
  return (df)
}
ttest_df = (ttest(num_cols[,-c(1,198)], as.factor(num_cols$Responders)))
#View(ttest_df)
#p value is less than 0.05, we reject the null hypothesis: h1 - dependent

ttest_df = subset(ttest_df, ttest_df$significance==1)
train_notnull_ttest = train_notnull[,ttest_df$colName]
train_notnull_chitest = train_notnull[,chitest_df$colName]
train_selected = cbind(train_notnull_ttest,train_notnull_chitest,num_cols$Responders)
dim(train_selected)
colnames(train_selected)[168] <- c("Responders")

#test data selection
test_notnull_ttest = test[,ttest_df$colName]
dim(test_notnull_ttest)
#View(count_NA(test_notnull_ttest))
test_notnull_chitest = test[,chitest_df$colName]
dim(test_notnull_chitest)
#View(count_NA(test_notnull_chitest))
test_selected = cbind(test_notnull_ttest,test_notnull_chitest)
dim(test_selected)
dim(train_selected)

#split data
train_selected$random = runif(nrow(train_selected),0,1)
train_selected.dev = train_selected[which(train_selected$random <= 0.8),]
train_selected.holdout = train_selected[which(train_selected$random > 0.8),]
c(nrow(train_selected.dev), nrow(train_selected.holdout))


#check distribution in train and dev set
sum(train_selected.dev$Responders)/dim(train_selected.dev)[1] #17%
sum(train_selected.holdout$Responders)/dim(train_selected.holdout)[1] #17%

## basine line model with rf
library(randomForest)
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(Responders) ~ ., data = train_selected.dev[,-c(169)], 
                   ntree=1000, mtry = 15, nodesize = 5000,
                   importance=TRUE)

print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")
RF$err.rate

impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

(VI_F=importance(RF))
varImpPlot(RF,type=2)

#XgBoost


#scoring
train_selected.holdout$predict.class <- predict(RF, train_selected.holdout, type="class")
train_selected.holdout$predict.score <- predict(RF, train_selected.holdout, type="prob")
train_selected.holdout$predict.score[,2]

#deciling

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



train_selected.holdout$deciles <- decile(train_selected.holdout$predict.score[,2])
#install.packages("data.table")
library(data.table)
tmp_DT = data.table(train_selected.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Responders), 
  cnt_resp = sum(Responders), 
  cnt_non_resp = sum(Responders == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);

#install.packages("scales")
library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)

## Classification Error
with(train_selected.holdout, table(Responders, predict.class))

#test set scoring submission
test_selected$predict.class <- predict(RF, test_selected, type="class")
test_selected$predict.score <- predict(RF, test_selected, type="prob")
test_selected$predict.score[,2]

submission = cbind(test$UCIC_ID, test_selected$predict.score[,2])
View(submission)
write.csv(submission, "/Users/sahilkumar/Documents/akshay_rem/submission.csv")

