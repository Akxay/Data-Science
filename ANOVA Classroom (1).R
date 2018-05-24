## Read the data Medical1

library(psych)
library(car)

setwd("C:/Users/stelidevara/Downloads")
## Summary Statistics


####Coupon and Instore Promotion Data
datasales<-read.csv("couponsales.csv",header=TRUE)

#Step 2: Clearly identify the factors in the data
datasales$promotion<-factor(datasales$promotion, labels=c("1","2","3"))
datasales$coupon<-factor(datasales$coupon, labels=c("1","2"))


describe(datasales$sales)
describeBy(datasales$sales, group=datasales$promotion)

## Test for normality based on promotion
shapiro.test(datasales$sales)
cat("Normality p-values by Factor Place: ")
cat("Normality p-values by Factor Place: ")
for (i in unique(factor(datasales$promotion))){
  cat(shapiro.test(datasales[datasales$promotion==i, ]$sales)$p.value," ")
}

## Test for normality based on coupon

cat("Normality p-values by Factor Place: ")
cat("Normality p-values by Factor Place: ")
for (i in unique(factor(datasales$coupon))){
  cat(shapiro.test(datasales[datasales$coupon==i, ]$sales)$p.value," ")
}

qqnorm(datasales$sales, pch=19, cex=0.6)
qqline(datasales$sales, col = 'red')

######## Levene's test for variance

## Test for homogeneity of variance
boxplot(datasales$sales~datasales$promotion)
leveneTest(datasales@sales~datasales$promotion)


##################################################################################
####ANOVA based on promotion

aov1 <- aov(datasales$sales~datasales$promotion)

summary(aov1)

TukeyHSD(aov1)
plot(TukeyHSD(aov1))
##################################################################################
##################################################################################
####ANOVA based on coupon

aov1 <- aov(datasales$sales~datasales$coupon)

summary(aov1)

TukeyHSD(aov1)
plot(TukeyHSD(aov1))
##################################################################################
##################################################################################
##################################################################################
####ANOVA based on promotion and coupon

aov3 <- aov(datasales$sales~datasales$promotion+datasales$coupon+datasales$promotion*datasales$coupon)

summary(aov3)

TukeyHSD(aov3)
plot(TukeyHSD(aov3,ordered = T))
##################################################################################
##### Depression Scores Data
##################################################################################

## Summary Statistics
Medical1<-read.csv("Medical1.csv", header=TRUE)

#Step 2: Clearly identify the factors in the data

df$levels<-factor(Medical1$Place, labels=c("New York","North Carolina","Florida"))



describe(Medical1$DepressionScore)
describeBy(Medical1$DepressionScore, group=Medical1$Place)

## Test for normality
shapiro.test(Medical1$DepressionScore)
qqnorm(Medical1$DepressionScore, pch=19, cex=0.6)
qqline(Medical1$DepressionScore, col = 'red')


qqnorm(Medical1$DepressionScore, pch=19, cex=0.6)
qqline(Medical1$DepressionScore, col = 'red')

## Test for homogeneity of variance
boxplot(Medical1$DepressionScore~Medical1$Place)
leveneTest(Medical1$DepressionScore~Medical1$Place)

## ANOVA
aov1 <- aov(Medical1$DepressionScore~Medical1$Place)

summary(aov1)

TukeyHSD(aov1)
plot(TukeyHSD(aov1))

## Read the data Medical2

## Summary Statistics
describe(Medical2$`DepressionScore`)
describeBy(Medical2$DepressionScore, group=Medical2$Place)

## ANOVA
aov2 <- aov(Medical2$`Depression Score`~Medical2$'Place')
summary(aov2)

TukeyHSD(aov2)
plot(TukeyHSD(aov2))

## Read the data SalesSalary

## Summary Statistics
describe(SalesSalary$`Salary`)
describeBy(SalesSalary$`Salary`, group=SalesSalary$'Position')
describeBy(SalesSalary$`Salary`, group=SalesSalary$'Experience')

## Test for normality
shapiro.test(SalesSalary$`Salary`)
qqnorm(SalesSalary$`Salary`, pch=19, cex=0.6)
qqline(SalesSalary$`Salary`, col = 'red')

## Test for homogeneity of variance
boxplot(SalesSalary$`Salary`~SalesSalary$'Position'*SalesSalary$'Experience')
leveneTest(SalesSalary$`Salary`~SalesSalary$'Position'*SalesSalary$'Experience')

## ANOVA
aov3 <- aov(SalesSalary$`Salary`~SalesSalary$'Position')
summary(aov3)
# TukeyHSD(aov3)
# plot(TukeyHSD(aov3))

aov4 <- aov(SalesSalary$`Salary`~SalesSalary$'Experience')
summary(aov4)
TukeyHSD(aov4)
plot(TukeyHSD(aov4, ordered = T))

aov5 <- aov(SalesSalary$`Salary`~SalesSalary$'Experience'*SalesSalary$'Position')
summary(aov5)
TukeyHSD(aov5)
plot(TukeyHSD(aov5, ordered = T))

interaction.plot(SalesSalary$'Position',SalesSalary$'Experience',SalesSalary$`Salary`, 
                 fixed=T, col=c('red', 'blue', 'green'))

#####################
