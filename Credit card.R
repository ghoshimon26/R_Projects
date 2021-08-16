
#credit card fraud detection

credit_card_data <- read.csv(choose.files())

dim(credit_card_data)

str(credit_card_data)

install.packages("ranger")
library(ranger)

library(caret)


head(credit_card_data)

colSums(is.na(credit_card_data)) # no missing values

#converting class to a factor variable

credit_card_data$Class <- factor(credit_card_data$Class, levels = c(0,1))

summary(credit_card_data)

table(credit_card_data$Class) # checking the number of fraud and non-fraudulent transactions

prop.table(table(credit_card_data$Class)) #getting the % distribution

#99.8% - Non -fraudulent , 0.17% - fraud

#Pie chart for showing the distribution

labels <- c('non-fraudulent','fraud')
labels <- paste(labels, round(100*prop.table(table(credit_card_data$Class)), 2))
labels <- paste0(labels, "%")

pie(table(credit_card_data$Class),labels, col = c('light blue', 'red'), main = "Credit Card Transaction Distribution")

#Creating the model

library(caTools)

set.seed(123)

split <- sample.split(credit_card_data$Class,SplitRatio = 0.80)
split
table(split)

training <- subset(credit_card_data, split==T)
test <- subset(credit_card_data, split==F)

nrow(training)
nrow(test)

#Balancing the dataset

#1.Random Over Sampling (ROS)

table(training$Class)

non_fraudulent_transaction <- 227452    

new_non_fraudulent_transaction <- 0.50

new_total <- non_fraudulent_transaction/new_non_fraudulent_transaction

install.packages("ROSE")

library(ROSE)

oversampling_result <- ovun.sample(Class~.,data=training,method = "over", 
                                   N=new_total, seed = 2019)

oversampling_credit <- oversampling_result$data

table(oversampling_credit$Class)

ggplot(data = oversampling_credit,aes(x=V1,y=V2,col=Class)) + 
  geom_point(position = position_jitter(width = 0.1)) + theme_bw() + scale_color_manual(values = c('dodgerblue2','red'))
       
#Random Under Sampling

table(training$Class)

fraudulent_transaction <- 394     

new_fraudulent_transaction <- 0.50

new_total <- fraudulent_transaction/new_fraudulent_transaction

undersampling_result <- ovun.sample(Class~.,data=training,method = "under", 
                                   N=new_total, seed = 2019)
undersampling_credit <- undersampling_result$data

table(undersampling_credit$Class)

ggplot(data = undersampling_credit,aes(x=V1,y=V2,col=Class)) + 
  geom_point() + theme_bw() + scale_color_manual(values = c('dodgerblue2','red'))

#Both ROS and RUS

n_new <- nrow(training) #227846

fraction_fraud_new <- 0.50

sampling_result <- ovun.sample(Class~.,data=training,method = "both", 
                               N=n_new, p=fraction_fraud_new, seed = 2019)

sampling_credit <- sampling_result$data

table(sampling_credit$Class)
prop.table(table(sampling_credit$Class))


ggplot(data = sampling_credit,aes(x=V1,y=V2,col=Class)) + 
  geom_point(position = position_jitter(width = 0.2)) + theme_bw() + scale_color_manual(values = c('dodgerblue2','red'))

# SMOTE for balancing the dataset

install.packages("smotefamily")
library(smotefamily)


table(training$Class)


#setting the no. of non-fraudulent and fraud cases and the % of non-fraudulent

n0 <- 227452    
n1 <- 394
r0 <- 0.6

ntimes <- ((1-r0)/r0) * (n0/n1) - 1

smote_output = SMOTE(X=training[ ,-c(1,31)],
                      target = training$Class,
                      K = 5,
                      dup_size = ntimes)


credit_smote <- smote_output$data

colnames(credit_smote)[30] <- "Class"

prop.table(table(credit_smote$Class))

#Class distribution for original distribution

ggplot(training,aes(x=V1,y=V2,color=Class)) + 
  geom_point() + scale_color_manual(values = c('dodgerblue2','red'))

#Class distribution for original distribution for over sampled dataset using SMOTE

ggplot(credit_smote,aes(x=V1,y=V2,color=Class)) + 
  geom_point() + scale_color_manual(values = c('dodgerblue2','red'))

#building decision tree

install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

CART_model <- rpart(Class~.,credit_smote)

rpart.plot(CART_model,extra = 0, type = 5, tweak = 1.2)

#predicting the fraud classes

predicted_value <- predict(CART_model, test, type = 'class')

confusionMatrix(predicted_value,test$Class)

#SMOTE balances the dataset first and then predicts

#correctly classified fraud transactions - 88
#Incorrect - 10
#Accuracy - 97.48%
#F1 score - 93.1%

