
#importing the dataset
ad_cliclking <- read.csv(choose.files())

dim(ad_cliclking)
str(ad_cliclking)

#removing unnecessary variables

ad_cliclking <- ad_cliclking[,-c(4,8,11)]
str(ad_cliclking)

#missing values

colSums(is.na(ad_cliclking))

#no missinging values

#outliers

boxplot(ad_cliclking$VistID) #no outliers

boxplot(ad_cliclking$Time_Spent) #no outliers

boxplot(ad_cliclking$Age)

boxplot(ad_cliclking$Internet_Usage)

boxplot(ad_cliclking$Year)

#encoding

ad_cliclking$Ad_Topic <- factor(ad_cliclking$Ad_Topic)
ad_cliclking$Ad_Topic <- as.numeric(ad_cliclking$Ad_Topic)

ad_cliclking$Country_Name <- factor(ad_cliclking$Country_Name)
ad_cliclking$Country_Name <- as.numeric(ad_cliclking$Country_Name)

ad_cliclking$Male <- factor(ad_cliclking$Male)
ad_cliclking$Male <- as.numeric(ad_cliclking$Male)

ad_cliclking$Time_Period <- factor(ad_cliclking$Time_Period)
ad_cliclking$Time_Period <- as.numeric(ad_cliclking$Time_Period)

ad_cliclking$Month <- factor(ad_cliclking$Month)
ad_cliclking$Month <- as.numeric(ad_cliclking$Month)



str(ad_cliclking)


#Univariate analysis

attach(ad_cliclking)
hist(Time_Spent)
hist(Internet_Usage)
hist(Age)



#bivariate analysis



ggplot(ad_cliclking,
       aes(x = Clicked, 
           y = Time_Spent)) +
  geom_bar(stat = "identity")

ggplot(ad_cliclking, 
       aes(x = Clicked, 
           y = Time_Spent)) +
  geom_point(color= "lightblue") +
  geom_smooth(method = "lm")
  
  
ggplot(ad_cliclking, 
       aes(x = Clicked, 
           y = Internet_usage)) +
  geom_point(color= "lightblue") +
  geom_smooth(method = "lm")                             
      

       








#Anova_bivariate_univariate

cor(ad_cliclking$Clicked,ad_cliclking$Age)

attach(ad_cliclking)
ad_cliclking_aov <- aov(Clicked~Country_Name) # aov(num~char)
ad_cliclking_aov
summary(ad_cliclking_aov)

pf(0.231, 1,6655, lower.tail = FALSE)
p=0.63 #not significant

ad_cliclking_aov1 <- aov(Clicked~Age) #significant : p<2e-16 ***
ad_cliclking_aov1
summary(ad_cliclking_aov1)


ad_cliclking_aov2 <- aov(Clicked~Internet_Usage) #significant : p<2e-16 ***
ad_cliclking_aov2
summary(ad_cliclking_aov2)


ad_cliclking_aov3 <- aov(Clicked~Time_Spent) #significant : p<2e-16 ***
ad_cliclking_aov3
summary(ad_cliclking_aov3)


ad_cliclking_aov4 <- aov(Clicked~Ad_Topic) #not significant : p = 0.106
ad_cliclking_aov4
summary(ad_cliclking_aov4)

ad_cliclking_aov5 <- aov(Clicked~Male) #significant : p = 0.0253
ad_cliclking_aov5
summary(ad_cliclking_aov5)


ad_cliclking_aov6 <- aov(Clicked~Month) #not significant : p = 0.73
ad_cliclking_aov6
summary(ad_cliclking_aov6)

ad_cliclking_aov8 <- aov(Clicked~Time_Period) #significant : p <2e-16 ***
ad_cliclking_aov8
summary(ad_cliclking_aov8)

#Identifying the potential predictors to be 
#used in the logistic regression algorithm

ad_cliclking <- ad_cliclking[,-c(7)]
str(ad_cliclking)






## Split the data into training and test

library(caTools)
set.seed(100)
split <- sample.split(ad_cliclking$Clicked, SplitRatio = 0.70)
split
table(split)
training <- subset(ad_cliclking, split==TRUE)
test <- subset(ad_cliclking, split==FALSE)
nrow(training)
nrow(test)


# Building a generalised linear model with training dataset

names(ad_cliclking)

logit_reg <- glm(Clicked~., data=training, family = 'binomial')
logit_reg
summary(logit_reg)
step(logit_reg)


logit_reg1 <- glm(Clicked~.-VistID, data=training, family = 'binomial')
logit_reg1

#multicollinearity

vif(logit_reg1)

vif = 1/1-0.9309
vif = 0.0691

# Predict the test data with logit regression model

logit_pred <- predict(logit_reg1, newdata = test, type='response')
logit_pred

logit_cbind <- cbind(test$Clicked,logit_pred)
head(logit_cbind,10)

# Threshod value = 0.5 (50%)
logit_pred_50 <- ifelse(logit_pred>=0.5,1,0)
logit_pred_50

logit_cbind <- cbind(test$Clicked,logit_pred_50)
head(logit_cbind,10)


# Building confusion matrix to check accuracy
cm <- table(test$Clicked,logit_pred_50)
cm

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(cm)

#Checking ROC and AUC curve to find better cut-off line 
#and avoid changing threshold value

ROCR_PRED <- prediction(logit_pred_50, test$Clicked)
ROCR_PRED

nrow(test)

ROCR_PRED_PERFORMANCE <- performance(ROCR_PRED, 'tpr','fpr')
ROCR_PRED_PERFORMANCE


plot(ROCR_PRED_PERFORMANCE)

plot(ROCR_PRED_PERFORMANCE, print.cutoffs.at=c(0,1,by=0.1))
abline(a=0,b=1)


plot(ROCR_PRED_PERFORMANCE, print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(-1,1), cex=0.7)
abline(a=0,b=1)




