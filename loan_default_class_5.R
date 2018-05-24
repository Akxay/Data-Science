default = read.csv("E:/Akxay/GLIM/Finance and risk analytics/Data_BABI/05.loan_default.csv")
str(default)

# split data
#ignoring stratification of X
library(caTools)
set.seed(100)

split = sample.split(default$bad_flag, SplitRatio = 0.75) # we can specify the stratification of X also

defaultTrain = subset(default, split == TRUE)
str(defaultTrain)

model = glm(bad_flag~., family=binomial, data =defaultTrain)
summary(model)

#test data
defaultTest = subset(default, split == FALSE)
predictTest = predict(model, type="response", newdata = defaultTest)

#build the confusion matrix
predictClass = predictTest > 0.5
confusion_matrix = table(defaultTest$bad_flag, predictClass)
confusion_matrix

####Oversampling
#install.packages("DMwR")
library(DMwR)


defaultTrain = subset(default, split ==TRUE)
defaultTrain$bad_flag = as.factor(defaultTrain$bad_flag)
str(defaultTrain)
#Smoting
defaultTrainSmote = SMOTE(bad_flag~., perc.over = 500, defaultTrain) 
#perc.over says-- for each minority generate 5 new points

table(defaultTrain$bad_flag)
table(defaultTrainSmote$bad_flag)

model = glm(bad_flag~., family=binomial, data =defaultTrainSmote)
summary(model)

#test data
defaultTest = subset(default, split == FALSE)
predictTest = predict(model, type="response", newdata = defaultTest)

#build the confusion matrix
predictClass = predictTest > 0.5
confusion_matrix = table(defaultTest$bad_flag, predictClass)
confusion_matrix

