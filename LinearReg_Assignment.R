
life_expectancy <- read.csv(choose.files())

dim(life_expectancy)

str(life_expectancy)

#Graphical representation of variables

attach(life_expectancy)
hist(Income_Composition_of_Resources)
hist(Per_Capita_GDP)

ggplot(life_expectancy, aes(x = Adult_Mortality)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white")


ggplot(life_expectancy, aes(x = Alcohol)) +
  geom_histogram(fill = "orange", 
                 color = "white")

hist(Thinness_5.9_Years)
hist(Schooling)







#Relationship between target and predictor variables

ggplot(life_expectancy,
       aes(x = Life_Expectancy, 
           y = Adult_Mortality)) +
  geom_bar(stat = "identity")

ggplot(life_expectancy,
       aes(x = Life_Expectancy, 
           y = GDP )) +
  geom_bar(stat = "identity")

ggplot(life_expectancy, 
       aes(x = Life_Expectancy, 
           y = Under.five_Deaths)) +
  geom_point(color= "lightblue") +
  geom_smooth(method = "lm")

ggplot(life_expectancy, 
       aes(x = Life_Expectancy, 
           y = Income_Composition_of_Resources)) +
  geom_point(color= "lightblue") +
  geom_smooth(method = "lm")

ggplot(life_expectancy, 
       aes(x = Life_Expectancy, 
           y = Total_Expenditure)) +
  geom_point(color= "lightblue") +
  geom_smooth(method = "lm")



# Treating Missing Values


colSums(is.na(life_expectancy))


# Adult Mortality

unique(life_expectancy$Adult_Mortality)

boxplot(life_expectancy$Adult_Mortality)

median(life_expectancy$Adult_Mortality, na.rm = T)

life_expectancy$Adult_Mortality[which(is.na(life_expectancy$Adult_Mortality))] <- median(life_expectancy$Adult_Mortality, na.rm = T)
colSums(is.na(life_expectancy))


#Alcohol(no outlier)

unique(life_expectancy$Alcohol)

length(which(is.na(life_expectancy$Alcohol)))/ nrow(life_expectancy)

boxplot(life_expectancy$Alcohol)

mean(life_expectancy$Alcohol, na.rm = T)

life_expectancy$Alcohol[which(is.na(life_expectancy$Alcohol))] <- mean(life_expectancy$Alcohol, na.rm = T)
colSums(is.na(life_expectancy))

#Hepatitis B

table(unique(life_expectancy$Hepatitis_B))

unique(life_expectancy$Hepatitis_B)

length(which(is.na(life_expectancy$Hepatitis_B)))/ nrow(life_expectancy)

boxplot(life_expectancy$Hepatitis_B)

median(life_expectancy$Hepatitis_B, na.rm = T)

life_expectancy$Hepatitis_B[which(is.na(life_expectancy$Hepatitis_B))] <- median(life_expectancy$Hepatitis_B, na.rm = T)
colSums(is.na(life_expectancy))


#BMI(no outlier)

unique(life_expectancy$BMI)

length(which(is.na(life_expectancy$BMI)))/ nrow(life_expectancy)

boxplot(life_expectancy$BMI)

mean(life_expectancy$BMI, na.rm = T)

life_expectancy$BMI[which(is.na(life_expectancy$BMI))] <- mean(life_expectancy$BMI, na.rm = T)
colSums(is.na(life_expectancy))

#Polio

unique(life_expectancy$Polio)

length(which(is.na(life_expectancy$Polio)))/ nrow(life_expectancy)

boxplot(life_expectancy$Polio)

median(life_expectancy$Polio, na.rm = T)

life_expectancy$Polio[which(is.na(life_expectancy$Polio))] <- median(life_expectancy$Polio, na.rm = T)
colSums(is.na(life_expectancy))

#Total_Expenditure

unique(life_expectancy$Total_Expenditure)

length(which(is.na(life_expectancy$Total_Expenditure)))/ nrow(life_expectancy)

boxplot(life_expectancy$Total_Expenditure)

median(life_expectancy$Total_Expenditure, na.rm = T)

life_expectancy$Total_Expenditure[which(is.na(life_expectancy$Total_Expenditure))] <- median(life_expectancy$Total_Expenditure, na.rm = T)
colSums(is.na(life_expectancy))

#Dptheria

unique(life_expectancy$Diphtheria)

length(which(is.na(life_expectancy$Diphtheria)))/ nrow(life_expectancy)

boxplot(life_expectancy$Diphtheria)

median(life_expectancy$Diphtheria, na.rm = T)

life_expectancy$Diphtheria[which(is.na(life_expectancy$Diphtheria))] <- median(life_expectancy$Diphtheria, na.rm = T)
colSums(is.na(life_expectancy))

#GDP

unique(life_expectancy$GDP)

length(which(is.na(life_expectancy$GDP)))/ nrow(life_expectancy)

boxplot(life_expectancy$GDP)

median(life_expectancy$GDP, na.rm = T)

life_expectancy$GDP[which(is.na(life_expectancy$GDP))] <- median(life_expectancy$GDP, na.rm = T)
colSums(is.na(life_expectancy))

#Per_Capita_GDP 

unique(life_expectancy$Per_Capita_GDP)

length(which(is.na(life_expectancy$Per_Capita_GDP)))/ nrow(life_expectancy)

boxplot(life_expectancy$Per_Capita_GDP)

median(life_expectancy$Per_Capita_GDP, na.rm = T)

life_expectancy$Per_Capita_GDP[which(is.na(life_expectancy$Per_Capita_GDP))] <- median(life_expectancy$Per_Capita_GDP, na.rm = T)
colSums(is.na(life_expectancy))

# Population

unique(life_expectancy$Population)

length(which(is.na(life_expectancy$Population)))/ nrow(life_expectancy)

boxplot(life_expectancy$Population)

median(life_expectancy$Population, na.rm = T)

life_expectancy$Population[which(is.na(life_expectancy$Population))] <- median(life_expectancy$Population, na.rm = T)
colSums(is.na(life_expectancy))


# Thinness_1.19_Years

unique(life_expectancy$Thinness_1.19_Years)

length(which(is.na(life_expectancy$Thinness_1.19_Years)))/ nrow(life_expectancy)

boxplot(life_expectancy$Thinness_1.19_Years)

median(life_expectancy$Thinness_1.19_Years, na.rm = T)

life_expectancy$Thinness_1.19_Years[which(is.na(life_expectancy$Thinness_1.19_Years))] <- median(life_expectancy$Thinness_1.19_Years, na.rm = T)
colSums(is.na(life_expectancy))

# Thinness_5.9_Years 

unique(life_expectancy$Thinness_5.9_Years)

length(which(is.na(life_expectancy$Thinness_5.9_Years)))/ nrow(life_expectancy)

boxplot(life_expectancy$Thinness_5.9_Years)

median(life_expectancy$Thinness_5.9_Years, na.rm = T)

life_expectancy$Thinness_5.9_Years[which(is.na(life_expectancy$Thinness_5.9_Years))] <- median(life_expectancy$Thinness_5.9_Years, na.rm = T)
colSums(is.na(life_expectancy))


# Income_Composition_of_Resources

unique(life_expectancy$Income_Composition_of_Resources)

length(which(is.na(life_expectancy$Income_Composition_of_Resources)))/ nrow(life_expectancy)

boxplot(life_expectancy$Income_Composition_of_Resources)

median(life_expectancy$Income_Composition_of_Resources, na.rm = T)

life_expectancy$Income_Composition_of_Resources[which(is.na(life_expectancy$Income_Composition_of_Resources ))] <- median(life_expectancy$Income_Composition_of_Resources, na.rm = T)
colSums(is.na(life_expectancy))

#Schooling

unique(life_expectancy$Schooling)

length(which(is.na(life_expectancy$Schooling)))/ nrow(life_expectancy)

boxplot(life_expectancy$Schooling)

median(life_expectancy$Schooling, na.rm = T)

life_expectancy$Schooling[which(is.na(life_expectancy$Schooling))] <- median(life_expectancy$Schooling, na.rm = T)
colSums(is.na(life_expectancy))

# Life expectancy

unique(life_expectancy$Life_Expectancy)

length(which(is.na(life_expectancy$Life_Expectancy)))/ nrow(life_expectancy)

boxplot(life_expectancy$Life_Expectancy)

median(life_expectancy$Life_Expectancy, na.rm = T)

life_expectancy$Life_Expectancy[which(is.na(life_expectancy$Life_Expectancy))] <- median(life_expectancy$Life_Expectancy, na.rm = T)
colSums(is.na(life_expectancy))

#now there are no missing data




# Handling Outliers

boxplot(life_expectancy$Adult_Mortality)
summary(life_expectancy$Adult_Mortality)
Q1 = 74 
Q3 = 227.0 
IQR = Q3 - Q1
IQR = 153
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  456.5

neg_oulier = Q1 - 1.5*IQR
neg_oulier =  -155.5

life_expectancy$Adult_Mortality <- ifelse(life_expectancy$Adult_Mortality>= 456.5, 445 , life_expectancy$Adult_Mortality)

boxplot(life_expectancy$Adult_Mortality)


boxplot(life_expectancy$Hepatitis_B)
summary(life_expectancy$Hepatitis_B)
Q1 = 82.00  
Q3 = 96.00   
IQR = Q3 - Q1
IQR = 14
pos_outlier = Q3 + 1.5*IQR
pos_outlier = 117

neg_oulier = Q1 - 1.5*IQR
neg_oulier = 61

life_expectancy$Hepatitis_B <- ifelse(life_expectancy$Hepatitis_B<=61,64, life_expectancy$Hepatitis_B)

boxplot(life_expectancy$Hepatitis_B)



boxplot(life_expectancy$Polio)
summary(life_expectancy$Polio)
Q1 = 78.00 
Q3 = 97.00 
IQR = Q3 - Q1
IQR = 19
pos_outlier = Q3 + 1.5*IQR
pos_outlier = 125.5

neg_outlier = Q1 - 1.5*IQR
neg_outlier = 49.5

life_expectancy$Polio <- ifelse(life_expectancy$Polio<=49.5,53, life_expectancy$Polio)

boxplot(life_expectancy$Polio)


boxplot(life_expectancy$Total_Expenditure)
summary(life_expectancy$Total_Expenditure)
Q1 = 4.370 
Q3 = 7.330 
IQR = Q3 - Q1
IQR = 2.96
pos_outlier = Q3 + 1.5*IQR
pos_outlier = 11.77

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -0.07

life_expectancy$Total_Expenditure <- ifelse(life_expectancy$Total_Expenditure>=11.77,10.9, life_expectancy$Total_Expenditure)

boxplot(life_expectancy$Total_Expenditure)



boxplot(life_expectancy$Diphtheria)
summary(life_expectancy$Diphtheria)
Q1 = 78.00 
Q3 = 97.00 
IQR = Q3 - Q1
IQR = 19
pos_outlier = Q3 + 1.5*IQR
pos_outlier = 125.5

neg_outlier = Q1 - 1.5*IQR
neg_outlier = 49.5

life_expectancy$Diphtheria <- ifelse(life_expectancy$Diphtheria<=49.5, 51, life_expectancy$Diphtheria)

boxplot(life_expectancy$Diphtheria)



boxplot(life_expectancy$GDP)
summary(life_expectancy$GDP)
Q1 = 5.729e+09 
Q3 = 1.363e+11
IQR = Q3 - Q1
IQR = 1.30571e+11
pos_outlier = Q3 + 1.5*IQR
pos_outlier = 332156500000

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -190127500000

life_expectancy$GDP <- ifelse(life_expectancy$GDP>=332156500000, 332156499989, life_expectancy$GDP)

boxplot(life_expectancy$GDP)



boxplot(life_expectancy$Per_Capita_GDP)
summary(life_expectancy$Per_Capita_GDP)
Q1 = 1120.9 
Q3 = 12384.3
IQR = Q3 - Q1
IQR = 11263.4
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  29279.4

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -15774.2

life_expectancy$Per_Capita_GDP <- ifelse(life_expectancy$Per_Capita_GDP>=29279.4, 29269, life_expectancy$Per_Capita_GDP)

boxplot(life_expectancy$Per_Capita_GDP)


boxplot(life_expectancy$Population)
summary(life_expectancy$Population)
Q1 = 2.345e+06 
Q3 = 2.467e+07
IQR = Q3 - Q1
IQR = 22325000
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  58157500

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -31142500

life_expectancy$Population <- ifelse(life_expectancy$Population>=58157500,58157470, life_expectancy$Population)

boxplot(life_expectancy$Population)


boxplot(life_expectancy$Thinness_1.19_Years)
summary(life_expectancy$Thinness_1.19_Years)
Q1 = 1.600 
Q3 = 7.100
IQR = Q3 - Q1
IQR = 5.5
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  15.35

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -6.65

life_expectancy$Thinness_1.19_Years <- ifelse(life_expectancy$Thinness_1.19_Years>=15.35,14.9, life_expectancy$Thinness_1.19_Years)

boxplot(life_expectancy$Thinness_1.19_Years)


boxplot(life_expectancy$Thinness_5.9_Years)
summary(life_expectancy$Thinness_5.9_Years)
Q1 = 1.600 
Q3 = 7.200
IQR = Q3 - Q1
IQR = 5.5
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  15.35

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -6.65

life_expectancy$Thinness_5.9_Years <- ifelse(life_expectancy$Thinness_5.9_Years>=15.35,14.9, life_expectancy$Thinness_5.9_Years)

boxplot(life_expectancy$Thinness_5.9_Years)


boxplot(life_expectancy$Income_Composition_of_Resources)
summary(life_expectancy$Income_Composition_of_Resources)
Q1 = 0.5042 
Q3 = 0.7720
IQR = Q3 - Q1
IQR = 0.2678
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  1.1737

neg_outlier = Q1 - 1.5*IQR
neg_outlier = 0.1025

life_expectancy$Income_Composition_of_Resources <- ifelse(life_expectancy$Income_Composition_of_Resources<=0.1025,0.1045, life_expectancy$Income_Composition_of_Resources)

boxplot(life_expectancy$Income_Composition_of_Resources)



boxplot(life_expectancy$Schooling)
summary(life_expectancy$Schooling)
Q1 = 10.30 
Q3 = 14.10
IQR = Q3 - Q1
IQR = 0.2678
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  1.1737

neg_outlier = Q1 - 1.5*IQR
neg_outlier = 0.1025

life_expectancy$Schooling <- ifelse(life_expectancy$Schooling<=0.1025,0.3, life_expectancy$Schooling)

boxplot(life_expectancy$Schooling)


life_expectancy$Schooling <- ifelse(life_expectancy$Schooling>=1.1737,1.15, life_expectancy$Schooling)

boxplot(life_expectancy$Schooling)


boxplot(life_expectancy$Infant_Deaths)
summary(life_expectancy$Infant_Deaths)
Q1 = 0
Q3 = 22
IQR = Q3 - Q1
IQR = 22
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  55

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -33

life_expectancy$Infant_Deaths <- ifelse(life_expectancy$Infant_Deaths>=55,53, life_expectancy$Infant_Deaths)



boxplot(life_expectancy$Infant_Deaths)

boxplot(life_expectancy$Measles)
summary(life_expectancy$Measles)
Q1 = 0
Q3 = 360.2
IQR = Q3 - Q1
IQR = 360.2
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  900.5

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -540.3

life_expectancy$Measles <- ifelse(life_expectancy$Measles>=900.5,895, life_expectancy$Measles)

boxplot(life_expectancy$Measles)





boxplot(life_expectancy$Under.five_Deaths)
summary(life_expectancy$Under.five_Deaths)
Q1 = 0 
Q3 = 28
IQR = Q3 - Q1
IQR = 28
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  70

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -42

life_expectancy$Under.five_Deaths <- ifelse(life_expectancy$Under.five_Deaths>=70,65, life_expectancy$Under.five_Deaths)

boxplot(life_expectancy$Under.five_Deaths)




boxplot(life_expectancy$HIV.AIDS)
summary(life_expectancy$HIV.AIDS)
Q1 = 0.10
Q3 = 0.80
IQR = Q3 - Q1
IQR = 0.7
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  1.85

neg_outlier = Q1 - 1.5*IQR
neg_outlier = -0.95

life_expectancy$HIV.AIDS <- ifelse(life_expectancy$HIV.AIDS>=1.85,1.5, life_expectancy$HIV.AIDS)

boxplot(life_expectancy$HIV.AIDS)




boxplot(life_expectancy$Life_Expectancy)
summary(life_expectancy$Life_Expectancy)
Q1 = 63.20
Q3 = 75.60
IQR = Q3 - Q1
IQR = 12.4
pos_outlier = Q3 + 1.5*IQR
pos_outlier =  94.2

neg_outlier = Q1 - 1.5*IQR
neg_outlier = 44.6

life_expectancy$Life_Expectancy <- ifelse(life_expectancy$Life_Expectancy<=44.6,49, life_expectancy$Life_Expectancy)

boxplot(life_expectancy$Life_Expectancy)



# Feature scaling

head(life_expectancy)
life_expectancy$Adult_Mortality<- scale(life_expectancy$Adult_Mortality)
life_expectancy$Hepatitis_B <- scale(life_expectancy$Hepatitis_B)
life_expectancy$Polio <- scale(life_expectancy$Polio)
life_expectancy$Total_Expenditure <- scale(life_expectancy$Total_Expenditure)
life_expectancy$Diphtheria <- scale(life_expectancy$Diphtheria)
life_expectancy$GDP <- scale(life_expectancy$GDP)
life_expectancy$Per_Capita_GDP <- scale(life_expectancy$Per_Capita_GDP)
life_expectancy$Population <- scale(life_expectancy$Population)
life_expectancy$Thinness_1.19_Years <- scale(life_expectancy$Thinness_1.19_Years)
life_expectancy$Thinness_5.9_Years <- scale(life_expectancy$Thinness_5.9_Years)
life_expectancy$Income_Composition_of_Resources <- scale(life_expectancy$Income_Composition_of_Resources)
life_expectancy$Schooling <- scale(life_expectancy$Schooling)
life_expectancy$Country <- scale(life_expectancy$Country)
life_expectancy$Status <- scale(life_expectancy$Status)


#Encoding


life_expectancy$Country <-factor(life_expectancy$Country)
life_expectancy$Country <-as.numeric(life_expectancy$Country)

life_expectancy$Status <-factor(life_expectancy$Status)
life_expectancy$Status <-as.numeric(life_expectancy$Status)


str(life_expectancy)

#splitting the data

library(caTools)
set.seed(123)
split <- sample.split(life_expectancy$Life_Expectancy, SplitRatio = 0.75)
split


table(split)
training <- subset(life_expectancy, split == TRUE)
test <- subset(life_expectancy, split == FALSE)
nrow(training)
nrow(test)


#build linear regression model with training dataset

linear_reg <- lm(Life_Expectancy~., data=training)
linear_reg
summary(linear_reg)

#Removing insignificant variables


linear_reg1 <- lm(Life_Expectancy~.-Thinness_1.19_Years -Infant_Deaths -BMI, data=training)
linear_reg1
summary(linear_reg1)

#Adjusted R-squared:  0.8702 


reg_pred <- predict(linear_reg1, newdata = test)
reg_pred

#combining actual vs predicted data

reg_pred_cbind <- cbind(actuals=test$Life_Expectancy, predicteds=reg_pred)
reg_pred_cbind




#Multicollinearity test


vif(linear_reg1)

#no multicolliearity

#autocorrelation

dwtest(linear_reg1)

# DW = 0.69718 p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0
# no autocorrelation

#data should be a linear line
plot(test$Life_Expectancy, col="blue", type = "s", lty=1.7)

lines(reg_pred, col="red", type = "o", lty=1.9) 

#BP Test

bptest(linear_reg1)


#data:  linear_reg1
#BP = 301.9, df = 19, p-value < 2.2e-16



#Normality test


plot(density(test$Life_Expectancy))

install.packages('nortest')
library(nortest)

ad.test(test$Life_Expectancy)

shapiro.test(test$Life_Expectancy)

#Anderson-Darling normality test

#data:  test$Life_Expectancy
#A = 11.13, p-value < 2.2e-16

#the curve is bell shaped

#MAPE

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 

install.packages("MLmetrics")

library(MLmetrics)

install.packages('DMwR')
library(DMwR)

MAPE(test$Life_Expectancy,reg_pred)

#0.03933627

















