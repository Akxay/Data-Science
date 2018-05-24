library(psych)

path = "E:/Akxay/GLIM/Finance and risk analytics/kreditech/kreditechDSTest2017/Training.csv"
data = read.csv(path, sep=";")

#test data
val_path = "E:/Akxay/GLIM/Finance and risk analytics/kreditech/kreditechDSTest2017/Validation.csv"
val_data = read.csv(val_path, sep=";")

dim(data)
dim(val_data)
View(val_data)

# check type of each column
str(data)
# Data Cleansing and pre-processing
data$v12 = as.numeric(gsub(",", "", as.character(data$v12)))
data$v50 = as.numeric(gsub(",", "", as.character(data$v50)))
data$v20 = as.numeric(gsub(",", "", as.character(data$v20)))
data$v97 = as.numeric(gsub(",", "", as.character(data$v97)))
data$v97 = as.numeric(gsub(",", "", as.character(data$v97)))
str(data)
data$v9 = as.factor(data$v9)

#val data
val_data$v9 = as.factor(val_data$v9)
str(val_data)
val_data$v12 = as.numeric(gsub(",", "", as.character(val_data$v12)))
val_data$v50 = as.numeric(gsub(",", "", as.character(val_data$v50)))
val_data$v20 = as.numeric(gsub(",", "", as.character(val_data$v20)))
val_data$v97 = as.numeric(gsub(",", "", as.character(val_data$v97)))
val_data$v97 = as.numeric(gsub(",", "", as.character(val_data$v97)))
View(val_data)
# 1. calculate number of NA's from your data frame
count_NA <- function(dataFrame) {
  df=data.frame()
  for (n in 1:ncol(dataFrame)){
    df[n,1]=colnames(dataFrame)[n]
    df[n,2]=sum(is.na(dataFrame[n]))
    names(df) <- c("colName","NA_Count")
  }
  return (df[order(-df$NA_Count),])
}

View(count_NA(val_data))



# v95 has 2145 entries as NA out of 3700, so let's create a new column with it
data$v95_NA = as.factor(ifelse(is.na(data$v95), 0, 1))
data$v95 = NULL
val_data$v95 = NULL
str(data)

# impute rest of the NA columns with median(numeric), mode(nominal)
# v55
data$v55[is.na(data$v55)] = median(data$v55, na.rm = TRUE)
#v42
data$v42[is.na(data$v42)] = median(data$v42, na.rm = TRUE)
#v12
data$v12[is.na(data$v12)] = median(data$v12, na.rm = TRUE)

#val data

val_data$v55[is.na(val_data$v55)] = median(val_data$v55, na.rm = TRUE)
#v42
val_data$v42[is.na(val_data$v42)] = median(val_data$v42, na.rm = TRUE)
#v12
val_data$v12[is.na(val_data$v12)] = median(val_data$v12, na.rm = TRUE)


#V33- CATEGORICAL
#mode function
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

data$v33[is.na(data$v33)] = getmode(data$v33)

#v99
data$v99[is.na(data$v99)] = getmode(data$v99)

#v32
data$v32[is.na(data$v32)] = getmode(data$v32)

#v85
data$v85[is.na(data$v85)] = getmode(data$v85)

#v7
data$v7[is.na(data$v7)] = getmode(data$v7)

# Val_data
val_data$v33[is.na(val_data$v33)] = getmode(val_data$v33)

#v99
val_data$v99[is.na(val_data$v99)] = getmode(val_data$v99)

#v32
val_data$v32[is.na(val_data$v32)] = getmode(val_data$v32)

#v85
val_data$v85[is.na(val_data$v85)] = getmode(val_data$v85)

#v7
val_data$v7[is.na(val_data$v7)] = getmode(val_data$v7)

# Exploratory Data Analysis

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
num_cols = get_num_columns(data)
View(num_cols)

# Categorical columns dataframe
cat_cols = get_cat_columns(data)
View(cat_cols)

#----------- NUMERIC COLUMNS-------------

#create a correlation matrix-plot
cor_data=cor(num_cols)
View(cor_data)
cor.plot(cor_data)
# We see v55 and v2 have correlation of 1 because v42 is just 10000 times v55.
#So we can drop one of these during our analysis.

#Let's see if PCA is possible or not (by looking at correlation matrix, it does not seem so but check if statistically if possible )
#step 1- perform Bartlett's spherity test for finding if the
#data is reducible or not.
#h0- The data is not reducible or is spherical

n <- nrow(num_cols)
p <- ncol(num_cols)
chi2 <- -(n-1-(2*p+5)/6)*log(det(cor_data))
# Determinant is 0, correlation matrix is not invertible.
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))

# Hence, PCA is not possible
#but we found- v55 and v2 have correlation of 1 because v42 is just 10000 times v55.
#So we can drop one of these during our analysis.----------------------

#----------let's look at distribution of each numeric column
hist(num_cols$v12)
hist(num_cols$v50) # seems some outliers
hist(num_cols$v55)  # seems some outliers
hist(num_cols$v20) # seems some outliers
hist(num_cols$v24) # seems some outliers
hist(num_cols$v97) # somewhat normally distributed
hist(num_cols$v42) # seems some outliers
hist(num_cols$v53) # seems some outliers

# let's check quantiles for susceptible columns
#v50
quantile(num_cols$v50, c(0,1,5,10,25,50,75,90,95,98,99,99.7,100)/100)
boxplot(num_cols$v50)
data = subset(data,data$v50 <= 8665)
hist(data$v50, breaks=10)

#v55
quantile(num_cols$v55, c(0,1,5,10,25,50,75,90,95,98,99,99.7,100)/100)
boxplot(num_cols$v55)
# Not seem to be a critical outlier

#v20
quantile(num_cols$v20, c(0,1,5,10,25,50,75,90,95,98,99,99.7,100)/100)
boxplot(num_cols$v20)
data = subset(data,data$v20 <= 13585.0)
hist(data$v20, breaks=10)

#v24
quantile(num_cols$v24, c(0,1,5,10,25,50,75,90,95,98,99,99.7,100)/100)
boxplot(num_cols$v24)
# Not seem to be a critical outlier

#v53
quantile(num_cols$v53, c(0,1,5,10,25,50,75,90,95,98,99,99.7,100)/100)
boxplot(num_cols$v53)
data = subset(data,data$v53 <= 50000.00)
hist(data$v53, breaks=10)

# Variable selection- t test for numerical columns
# "v12" "v50" "v55" "v20" "v24" "v97" "v42" "v53"
t.test(data$v12 ~ data$classlabel) # not signi
t.test(data$v50 ~ data$classlabel) # signi
t.test(data$v55 ~ data$classlabel) # signi
t.test(data$v20 ~ data$classlabel) # signi somewhat
t.test(data$v24 ~ data$classlabel) # signi
t.test(data$v97 ~ data$classlabel) # not signi
t.test(data$v42 ~ data$classlabel) # signi
t.test(data$v53 ~ data$classlabel) # signi

# ----------------let's look at categorical columns now------------

#"v33""v76" "v68" "v7" "v70" "v32" "v28" "v99" "v85""v9" "v84""v44" "v95_NA" 
cat_cols = cat_cols[,-c(13)]
colnames(cat_cols)

# 2. Unique categories count
unique_cat_count <- function(dataFrame) {
  df=data.frame(rapply(dataFrame[,unlist(lapply(dataFrame, is.factor))],function(x)length(unique(x))))
  df=data.frame(as.factor(rownames(df)),df)
  colnames(df)[1]="cat_columns"
  colnames(df)[2]="cat_count"
  row.names(df) <- NULL
  return (df[order(-df[,2]),])
}

View(unique_cat_count(cat_cols))
View(unique_cat_count(get_cat_columns(val_data)))

# Let's perform chi square test to check for variable dependence
# cross tab with respect to credibility
library(gmodels)

#"v33""v76" "v68" "v7" "v70" "v32" "v28" "v99" "v85""v9" "v84""v44" "v95_NA"
CrossTable(data$classlabel, data$v33, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v76, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v68, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v7, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v70, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v32, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v28, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v99, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v85, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v9, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
# V9 not significant
CrossTable(data$classlabel, data$v84, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v44, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(data$classlabel, data$v95_NA, digits=2, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
# v95_Na not significant

# ----Some bivariate viz
attach(data)
library(ggplot2)
qplot(v50, v20, colour = classlabel,data=data, main = "v50 Vs v20") 
# not much info as we don't have information about what v50 or v20 means

#---------------------Building Logistic Regression Model---------------------------


# check distribution of yes and no in classlabel
dim(subset(data, data$classlabel=="yes."))
#yes = (3328/3599)*100 = 92.4%
#no = (271/3599) * 100 = 7.53%

# As the percentage of minority class is very less, we need to SMOTE the data
# But border smote would work better, but let's just use SMOTE

#----------- SMOTE----------------

table(classlabel)
library(DMwR)
train = as.data.frame(data)
train$v12 = NULL 
train$v97 = NULL
train$v9 = NULL
train$v95_NA = NULL
train$v42 = NULL
train$v99 = NULL

train$v33 = NULL
train$v76 = NULL
train$v85 = NULL
train$v32 = NULL

#val_data
val_data$v12 = NULL 
val_data$v97 = NULL
val_data$v9 = NULL
val_data$v95_NA = NULL
val_data$v42 = NULL
val_data$v95 = NULL
val_data$v99 = NULL

val_data$v33 = NULL
val_data$v76 = NULL
val_data$v85 = NULL
val_data$v32 = NULL

val_data$v55 = as.numeric(val_data$v55)
val_data$v24 = as.numeric(val_data$v24)
val_data$v53 = as.numeric(val_data$v53)

#identify factors clearly
# 3. Unique categories name
unique_cat_name <- function(dataFrame) {
  x=dataFrame[,unlist(lapply(dataFrame, is.factor))]
  uniq_name=data.frame()
  for (i in 1:ncol(x)){
    str=colnames(x)[i]
    str1=paste0(unique(x[i])[,1],collapse=",")
    uniq_name[i,1]=str
    uniq_name[i,2]=str1
  }
  return(uniq_name)
}

View(unique_cat_name(get_cat_columns(train)))
View(unique_cat_name(get_cat_columns(val_data)))

train$v33 = factor(train$v33, labels=c("bb","dd","ff","h", "j", "n","v","z", "o"))
val_data$v33 = factor(val_data$v33, labels=c("bb","dd","ff","h", "j", "n","v","z"))

train$v76 = factor(train$v76, labels=c("g","p","s"))
val_data$v76 = factor(val_data$v76, labels=c("g","p","s"))

train$v85 = factor(train$v85, labels=c("g","p","gg"))
val_data$v85 = factor(val_data$v85, labels=c("g","p"))

# c,k,ff,i,j,q,W,d,m,cc,aa,r,x,e


colnames(train)
str(train)
SMtrain<-SMOTE(classlabel~.,train, perc.over = 300, k = 3, perc.under = 400)
# With these values, we are almost capture all the zeroes
table(SMtrain$classlabel)

# New Distribution- no. = 25%

# Drop insignificant variable as taken by above tests results

#1. Logistic Regression

dim(SMtrain)
Logit<-classlabel~.

#selected Variables
Logit.1 <- glm(Logit, SMtrain, family = binomial)
summary(Logit.1)
vif(Logit.1)

# The algorithm could not converge, So, let's try Tree based approach or LDA/SVM

# --------------CART---------------
## loading the library
library(rpart)
library(rpart.plot)
## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=20, minbucket = 10, cp = 0, xval = 5)


## calling the rpart function to build the tree
##m1 <- rpart(formula = Target ~ ., data = CTDF.dev[which(CTDF.dev$Holding_Period>10),-1], method = "class", control = r.ctrl)
m1 <- rpart(formula = classlabel ~ ., data = SMtrain, method = "class", control = r.ctrl)
m1


## install.packages("rattle")
## install.packages("RcolorBrewer")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)


## to find how the tree performs
printcp(m1)
plotcp(m1)

##rattle()
## Pruning Code
ptree<- prune(m1, cp= 0.00 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

#-------------------------Random FOrest-----------------------
library(randomForest)
## Calling syntax to build the Random Forest
RF <- randomForest(classlabel ~ ., data = SMtrain, 
                   ntree=500, mtry = 3, nodesize = 10,
                   importance=TRUE)


print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")


RF$err.rate

## List the importance of the variables.
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

(VI_F=importance(RF))
varImpPlot(RF,type=2)

## Scoring on validation set


str(val_data)
dim(SMtrain)

val_data$predict.class <- predict(RF, val_data[,-c(17)], type="class")
val_data$predict.score <- predict(RF, val_data[,-c(17)], type="prob")

# Evaluation

#1. rank Ordering
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


val_data$deciles <- decile(val_data$predict.score[,2])
val_data$classlabel_val = ifelse((val_data$classlabel == "yes."), 0, 1)
# yes are 0 and no are 1

library(data.table)
tmp_DT = data.table(val_data)
rank <- tmp_DT[, list(
  cnt = length(classlabel_val), 
  cnt_resp = sum(classlabel_val), 
  cnt_non_resp = sum(classlabel_val == 0 )) , 
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

View(rank) # KS is 37

library(ROCR)
pred <- prediction(val_data$predict.score[,2], val_data$classlabel)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
nauc <- as.numeric(auc@y.values)
auc
# AUC= 70%

# confusion Matrix and ROC curves



df=val_data[,c(14,12)]
# Churn vs not churn predicted probablities
ggplot(df, aes(val_data$predict.score[,2], fill = as.factor(classlabel))) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
ggplot(df, aes(val_data$predict.score[,2], fill = as.factor(classlabel))) + geom_density(alpha = 0.1)

library(SDMTools)
library(pROC)
library(Hmisc)

tab.logit<-confusion.matrix(val_data$classlabel_val, val_data$predict.score[,2],threshold = 0.4)
tab.logit

# 1's are no and 0 are yes
#TPR- percent of the Defualt cases did we catch? 
# 51/107 = 48%
#FPR
# 46/93 = 50%
