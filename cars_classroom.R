setwd( "E:\\Akxay\\GLIM\\Predictive Analytics\\classroom")
Cars<-read.csv(file.choose(), header=T)
View(Cars)
na.omit(Cars)

summary(Cars)
str(Cars)

#install.packages("psych")
#install.packages("e1071")
library(caret)
library(car)
library(psych)
library(e1071)
library(plyr)


#Define some dummies
Cars$Male <-ifelse(Cars$Gender=="Male",1,0)
Cars$Car<-ifelse(Cars$Transport=="Car",1,0)
Cars$Public<-ifelse(Cars$Transport=="Public Transport",1,0)
Cars$Scooter<-ifelse(Cars$Transport=="2Wheeler",1,0)
Cars$Vehicle<-ifelse(Cars$Transport=="Car","Car","None")
Cars$Vehicle<-as.factor(Cars$Vehicle)
summary(Cars$Transport)

# qplot()
qplot(Work.Exp,  Salary, colour = Transport, shape=Transport,data=Cars)

qplot(Work.Exp,  Salary, colour = Vehicle,data=Cars)

qplot(Work.Exp,  Salary, colour = Gender, shape=Gender,data=Cars)




# create 3D point color

library(scatterplot3d)

#Binary Classification Plots: Cars vs No Cars

Cars$pcolor[Cars$Car==1] <- "red"
Cars$pcolor[Cars$Car==0] <- "blue"

with(Cars, {
  
  s3d <- scatterplot3d(Work.Exp, Salary, Distance,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates cars or not
                       main="3-D Scatterplot: Using Cars ",
                       xlab="Years",
                       ylab="In'00,000",
                       zlab="Km")
  s3d.coords <- s3d$xyz.convert(Work.Exp, Salary, Distance)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Cars or Not",
         c("1","0"), fill=c("red", "blue"))
})


#Multi Class Plots:Transportation Used


Cars$pcolor[Cars$Transport=="Car"] <- "red"
Cars$pcolor[Cars$Transport=="Public Transport"] <- "blue"
Cars$pcolor[Cars$Transport=="2Wheeler"] <- "darkgreen"
with(Cars, {
  
  s3d <- scatterplot3d(Work.Exp, Salary, Distance,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates no. of cylinders
                       main="3-D Scatterplot: Vehicles Used ",
                       xlab="Years",
                       ylab="In'00,000",
                       zlab="Km")
  s3d.coords <- s3d$xyz.convert(Work.Exp, Salary, Distance)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Transportation Used",
         c("Car", "Public Transport","2wheelers"), fill=c("red", "blue", "darkgreen"))
})


#Scatter Plots and Correlation

Car.Scatter<-subset(Cars[,c(1,3:10)])

pairs.panels(Car.Scatter[1:7], gap=0, bg=c("red", "blue","green")[Car.Scatter$Transport], pch=21)
levels(Car.Scatter$Transport)


#Scatter Plots and Correlation
qplot(Work.Exp,  Salary, colour = Vehicle, data=Cars)



# Partition the data

summary(Cars$Car)

set.seed(1111)
pd<-sample(2,nrow(Cars),replace=TRUE, prob=c(0.7,0.3))
train<-Cars[pd==1,]
val<-Cars[pd==2,]
summary(train$Car)
summary(val$Car)

#Linear Probability Model
LPM<-Car~Age+Engineer+MBA+Work.Exp+Male+Distance+Salary+license
LPM<-lm(LPM,train)
summary(LPM)

#VIF:1/(1-R^2)
vif(LPM)

# Age and Salary and Work Exp, all can not be there in the model.
#check one by one by adding and removing one of these and check VIF again

LPM.1<-Car~Engineer+MBA+Work.Exp+Male+Distance+Salary+license
LPM.1<-lm(LPM.1,train)
summary(LPM.1)
vif(LPM.1)

LPM.2<-Car~Engineer+MBA+Male+Distance+Salary+license
LPM.2<-lm(LPM.2,train)
summary(LPM.2)
vif(LPM.2)
#Retain only the significant ones

LPM.3<-Car~Distance+Salary+license
LPM.3<-lm(LPM.3,train)
summary(LPM.3)
vif(LPM.3)


#Revisit the plot again

train$pcolor[train$Car==1] <- "red"
train$pcolor[train$Car==0] <- "blue"

with(train, {
  
  s3d <- scatterplot3d(license, Salary, Distance,       # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates cars or not
                       main="3-D Scatterplot: Using Cars_train ",
                       xlab="Binary",
                       ylab="Unit.Free (INR)",
                       zlab="Unit.Free (Km)")
  s3d.coords <- s3d$xyz.convert(license, Salary, Distance)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Cars or Not",
         c("1","0"), fill=c("red", "blue"))
})

#Now some Predictions
Pred_LPM <- predict(LPM.3,newdata=val)
Pred_LPM
#Confusionmatrix
tabLPM<-table(val$Car, Pred_LPM > 0.5)
tabLPM
sum(diag(tabLPM))/sum(tabLPM)

#What went right and what went wrong?


val$LPM<-Pred_LPM
val$pred.LPM<-ifelse(val$LPM>0.5,1,0)
val$correct.LPM<-ifelse(val$Car-val$pred.LPM==0,1,0)
summary(val$correct.LPM)



#Logistic Regression

install.packages(c("SDMTools","pROC", "Hmisc"))
library(SDMTools)
library(pROC)
library(Hmisc)


Logit<-Car~Age+Engineer+MBA+Work.Exp+Male+Distance+Salary+license

#All Variables

Logit.1 <- glm(Logit   , train, family = binomial)
summary(Logit.1)
vif(Logit.1)

Logit.new<-Car~Engineer+MBA+Male+Distance+Salary+license
Logit.2 <- glm(Logit.new  , train, family = binomial)
summary(Logit.2)
vif(Logit.2)

Logit.3<-Car~Distance+Salary
Logit.3 <- glm(Logit.3  , train, family = binomial)
summary(Logit.3)
vif(Logit.3)

pred.logit <- predict.glm(Logit.3, newdata=val, type="response")


tab.logit<-confusion.matrix(val$Car,pred.logit,threshold = 0.5)
tab.logit
roc.logit<-roc(val$Car,pred.logit )
roc.logit
plot(roc.logit)


ROC<- data.frame( Car=val$Car, Fitted = pred.logit)
write.csv(ROC, file = "ROC.csv")

val$logit<-pred.logit
val$pred.logit<-ifelse(val$logit>0.5,1,0)
val$correct.logit<-ifelse(val$Car-val$pred.logit==0,1,0)
summary(val$correct.logit)



#### So what to do?


#knn Algorithm

# Feature Scaling



train$norm.Salary<-(train$Salary-min(train$Salary))/(max(train$Salary)-min(train$Salary))
train$norm.license<-(train$license-min(train$license))/(max(train$license)-min(train$license))
train$norm.Distance<-(train$Distance-min(train$Distance))/(max(train$Distance)-min(train$Distance))



val$norm.Salary<-(val$Salary-min(val$Salary))/(max(val$Salary)-min(val$Salary))
val$norm.license<-(val$license-min(val$license))/(max(val$license)-min(val$license))
val$norm.Distance<-(val$Distance-min(val$Distance))/(max(val$Distance)-min(val$Distance))

library(class)
#Knn frames

train.knn<-train[,16:18]
val.knn<-val[,16:18]
train_target<-train[,11]
train_target
val_target<-val[,11]

#knn 3
knn3<-knn(train=train.knn,test=val.knn,cl=train_target,
          k=3)
knn3
tab.knn.3<-table(val_target,knn3)
print(tab.knn.3)
 confusionMatrix(table(val_target,knn3))   



#knn 5
knn5<-knn(train=train.knn,test=val.knn,cl=train_target,
          k=5)
knn5
tab.knn.5<-table(val_target,knn3)
print(tab.knn.5)
confusionMatrix(table(val_target,knn5))   

#Nave Bayes Theorem
library(ElemStatLearn)
train.NB<-train[,c(18,17,14)] # correct these
View(train.NB)
View(val.NB)
val.NB<-val[,c(15,17,14)]

train.NB$Vehicle<-as.factor(train.NB$Vehicle)
val.NB$Vehicle<-as.factor(val.NB$Vehicle)

qplot(norm.Salary,  norm.Distance, colour = Vehicle, data=val.NB)


str(train.NB)
#
NB.Cars<-naiveBayes(x=train.NB[-3], y=train.NB$Vehicle)
#pedict
y_pred.NB<-predict(NB.Cars,newdata=val.NB[-3])
y_pred.NB
#Confusion matrix
cm.NB=table(val.NB[,3],y_pred.NB)
cm.NB


# Visualising the Test set results
library(ElemStatLearn)
set = val.NB
X1 = seq(min(set[, 1])-0.1 , max(set[, 1]) +0.1, by = 0.01)
X2 = seq(min(set[, 2])-0.1 , max(set[, 2])+0.1 , by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('norm.Salary', 'norm.Distance')
y_grid = predict(NB.Cars, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes: Test ',
     xlab = 'norm.Salary', ylab = 'norm.Distance',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "Car", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "Car", 'black', 'green'))


#Linear Discriminant Analysis
#LDA
##Perform LDA
install.packages("MASS")
install.packages("ISLR")
library(MASS)


LDA<-Car~norm.Distance+norm.Salary




lda.Bi<-lda(LDA,train)
lda.Bi
lda.Bi.pred.train<-predict(lda.Bi,train)
lda.Bi.pred.train

lda.Bi.pred.val<-predict(lda.Bi, newdata=val)
lda.Bi.pred.val

#LDA Histogram
ldahist(data=lda.Bi.pred.val$x[,1], val$Car)


ldapredclass.Bi<-lda.Bi.pred.val$class

tab.LDA.Bi<-table(ldapredclass.Bi,val$Car)


print(tab.LDA.Bi)
sum(diag(tab.LDA.Bi))/sum(tab.LDA.Bi)


library(klaR)
partimat(Vehicle~norm.Distance+norm.Salary,data=train,"lda")
partimat(Vehicle~norm.Distance+norm.Salary,data=val,"qda")



#Hoq about QDA?
qda<-Car~norm.Distance+norm.Salary

qda.Bi<-qda(qda,train)
qda.Bi

qda.Bi.pred.val<-predict(qda.Bi, newdata=val)

qdapredclass.Bi<-qda.Bi.pred.val$class

tab.qda.Bi<-table(qdapredclass.Bi,val$Car)


print(tab.qda.Bi)
sum(diag(tab.qda.Bi))/sum(tab.qda.Bi)





#Observations Analysis
val$pred.lda<-ldapredclass.Bi
#val$pred.lda<-ifelse(val$lda>0.5,1,0)
val$correct.lda<-ifelse(val$Car==val$pred.lda,1,0)
summary(val$correct.lda)


#Support Vector Machines
SVM<-Car~norm.Distance+norm.Salary

install.packages("e1071")
library(e1071)
svm.1<-svm(SVM, data=train, type='C-classification', kernel = 'linear')

svm.1

#Predict
svm_pred<-predict(svm.1, newdata=val)



tab.svm<-table(val$Car, svm_pred)
print(tab.svm)
sum(diag(tab.svm))/sum(tab.svm)



#MultiClass



#For LDA
LDA.M<-Transport~norm.Distance+norm.Salary+ norm.license


lda.Multi<-lda(LDA.M,train)
lda.Multi
lda.Multi.pred.train<-predict(lda.Multi,train)
lda.Multi.pred.train

lda.Multi.pred.val<-predict(lda.Multi, newdata=val)
lda.Multi.pred.val

#LDA Histogram
ldahist(data=lda.Multi.pred.val$x[,1], val$Transport)

#Bi Plot
install.packages("devtools")
install_github('fawda123/ggord')
#install.packages("ggplot2")

library(devtools)

library(ggord)
library(ggplot2)

ggord(lda.Multi,train$Transport, ylim=c(-3,3))

#PArtition
#install.packages("klaR")
library(klaR)
partimat(Transport~norm.Distance+norm.Salary+norm.license,data=train,"lda")
partimat(Transport~norm.Distance+norm.Salary+norm.license,data=val,"lda")


partimat(Transport~norm.Distance+norm.Salary+norm.license,data=train,"qda")
partimat(Transport~norm.Distance+norm.Salary+norm.license,data=val,"qda")

ldapredclass.Multi<-lda.Multi.pred.val$class
ldapredclass.Multi
tab.LDA.Multi<-table(ldapredclass.Multi,val$Transport)
print(tab.LDA.Multi)
sum(diag(tab.LDA.Multi))/sum(tab.LDA.Multi)


#Multinomial Logit
M.Logit<-Transport~Distance+Salary+license


#Multi Nomial Logit
#Develop model on entire data
#install.packages("nnet")
library(nnet)


#Reshape data to wide


#In and out

#Reshape data to wide


train$tpt<-as.factor(train$Transport)
train$out<-relevel(train$Transport,ref="2Wheeler")
#Develop the model


multmodel1<-multinom(M.Logit, data=train)
summary(multmodel1)
# Significance of coefficients
# z values
z<-summary(multmodel1)$coefficients/summary(multmodel1)$standard.errors
print(z)
prob1<-(1-pnorm(abs(z),0,1))*2
prob1
multpred1<-predict(multmodel1,train)
predict(multmodel1,train, type="prob")

#Confusion

confmatrx<-table(multpred1,train$Transport)
print (confmatrx)
sum(diag(confmatrx))/sum(confmatrx)

#val
multpredval<-predict(multmodel1,val)
predict(multmodel1,val, type="prob")
#Confmatrix val

confmatrxval<-table(multpredval,val$Transport)
print (confmatrxval)
sum(diag(confmatrx))/sum(confmatrx)


