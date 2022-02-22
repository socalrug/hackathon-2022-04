
install.packages('caTools')
library(caTools)

library(ade4)
library(dplyr)
library(data.table)
library(caret)






###########Model 1 : Overall
df <- read.table("df_final_ish.csv", sep=",", stringsAsFactors = FALSE,header = TRUE)
head(df)
df['zero_previous'] <- ifelse(df$previous == 0,1,0)
head(df)
df['pdays'] <- ifelse(df$pdays == -1,1,0)
head(df)
df_new <- subset(df,select = -c(day,month,pdays,previous,job_unknown,education_unknown,contact_unknown,poutcome_unknown,marital_single,X,zero_previous) )
head(df_new)

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

a = df_new$balance    
quantile(a, c(.99)) #13164.9 

b = df_new$duration    
quantile(b, c(.95)) #751

df_new['balance_new']<- ifelse(df_new$balance > 13164.9,13164.9,df_new$balance)
df_new['duration_new']<- ifelse(df_new$duration > 751,751,df_new$duration)
df_new1 <- subset(df_new,select = -c(balance,duration) )

data <- sapply(df_new1,normalize)
head(data)
#df['dep'] <- ifelse(df$y == "yes",1,0)
set.seed(33333) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  

head(data)

sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]



head(train)

head(test)

train1 <- as.data.frame(train)
class(train1)
test1 <- as.data.frame(test)
class(test1)
#logistic regression model
model <- glm (y_yes ~ ., data = train1, family = binomial)
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(train1$y_yes, predict > 0.5)

pred <- predict(model, newdata = test1, type = "response")
hist(pred)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test1$y_yes
y_act
table(y_pred_num,y_act)
mean(y_pred == y_act)


#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, train1$y_yes)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



###########Model 2 : Smote , Overall


df <- read.table("df_final_ish.csv", sep=",", stringsAsFactors = FALSE,header = TRUE)
head(df)
df['zero_previous'] <- ifelse(df$previous == 0,1,0)
head(df)

#-1 is new customer
df['pdays'] <- ifelse(df$pdays == -1,0,1)
head(df)
df_new1 <- subset(df,select = -c(day,month,pdays,job_unknown,education_unknown,contact_unknown,poutcome_unknown,marital_single,X,zero_previous) )

summary(df_new1$duration)

duration = df_new1$balance    
quantile(duration, c(.99)) #13164.9 

duration = df_new1$balance    
quantile(duration, c(.01)) 
#outler treatment for balance
df_new1['balance_new']<- ifelse(df_new1$balance > 13164.9,13164.9,df_new1$balance)
df_new1['duration_new']<- ifelse(df_new1$duration > 751,751,df_new1$duration)


df_new2 <- subset(df_new1,select = -c(balance,duration) )


normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

data <- sapply(df_new2,normalize)


head(data)
class(data)
library(caret)
set.seed(1234)

sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]


train1 <- as.data.frame(train)
class(train1)


test1 <- as.data.frame(test)
class(test1)

# write.csv(train1, "train1_smoted.csv")
# write.csv(test1, "test1_smoted.csv")

prop.table(table(train1$y_yes))
##        0         1 
#0.8813147 0.1186853 
prop.table(table(test1$y_yes))
##     0         1 
##0.8847158 0.1152842 
dim(train1)
#22606    30
dim(train1)


library(DMwR)
train1$y_yes <- as.factor(train1$y_yes)
train1 <- SMOTE(y_yes ~ ., train1, perc.over = 100, perc.under=200)

model <- glm (y_yes ~ ., data = train1, family = binomial) 
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(train1$y_yes, predict > 0.5)

pred <- predict(model, newdata = train1, type = "response")
hist(pred)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- train1$y_yes
y_act
table(y_pred_num,y_act)
mean(y_pred == y_act)
y_pred


#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, train1$y_yes)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


###########Model 3 : Old Customer


df <- read.table("df_final_ish.csv", sep=",", stringsAsFactors = FALSE,header = TRUE)
head(df)
df['zero_previous'] <- ifelse(df$previous == 0,1,0)
head(df)

#-1 is new customer
df['pdays'] <- ifelse(df$pdays == -1,0,1)
head(df)
df_new1 <- subset(df,select = -c(day,month,pdays,job_unknown,education_unknown,contact_unknown,poutcome_unknown,marital_single,X,zero_previous) )

summary(df_new1$duration)

duration = df_new1$balance    
quantile(duration, c(.99)) #13164.9 

duration = df_new1$balance    
quantile(duration, c(.01)) 
#outler treatment for balance
df_new1['balance_new']<- ifelse(df_new1$balance > 13164.9,13164.9,df_new1$balance)
head(df_new)

df_new2 <- subset(df_new1,select = -c(balance) )


normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

data <- sapply(df_new2,normalize)

summary(data$duration)

head(data)
class(data)
library(caret)
set.seed(1234)

sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]


train1 <- as.data.frame(train)
class(train1)


test1 <- as.data.frame(test)
class(test1)

write.csv(train1, "train1_smoted.csv")
write.csv(test1, "test1_smoted.csv")

prop.table(table(train1$y_yes))
##        0         1 
#0.8813147 0.1186853 
prop.table(table(test1$y_yes))
##     0         1 
##0.8847158 0.1152842 
dim(train1)
#22606    30
dim(train1)


library(DMwR)
train1$y_yes <- as.factor(train1$y_yes)
train1 <- SMOTE(y_yes ~ ., train1, perc.over = 100, perc.under=200)

model <- glm (y_yes ~ ., data = train1, family = binomial) 
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(train1$y_yes, predict > 0.5)

pred <- predict(model, newdata = train1, type = "response")
hist(pred)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- train1$y_yes
y_act
table(y_pred_num,y_act)
mean(y_pred == y_act)
y_pred


###########Model 4 : Old Customer

df <- read.table("~/df_not_neg1.csv", sep=",", stringsAsFactors = FALSE,header = TRUE)
head(df)
df['zero_previous'] <- ifelse(df$previous == 0,1,0)
head(df)

#-1 is new customer
head(df)
df_new1 <- subset(df,select = -c(day,month,pdays,job_unknown,education_unknown,contact_unknown,poutcome_unknown,marital_single,X,zero_previous) )

colnames(df_new1)

summary(df_new1$duration)

duration = df_new1$balance    
quantile(duration, c(.99)) #12887.36 

duration = df_new1$balance    
quantile(duration, c(.01)) 
#outler treatment for balance
df_new1['balance_new']<- ifelse(df_new1$balance > 12887.36 ,12887.36,df_new1$balance)
head(df_new2)
df_new2 <- subset(df_new1,select = -c(balance) )

dur <- df$duration
quantile(dur, c(.95)) 
df_new2['duration']<- ifelse(df_new1$duration > 707 ,707,df_new1$duration)
head(df_new2)


normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}
data <- sapply(df_new2,normalize)


head(data)
class(data)
library(caret)
set.seed(1234)

sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]


train1 <- as.data.frame(train)
class(train1)


test1 <- as.data.frame(test)
class(test1)

write.csv(train1, "train1_smoted.csv")
write.csv(test1, "test1_smoted.csv")

prop.table(table(train1$y_yes))
##        0         1 
#0.7683573 0.2316427 
prop.table(table(test1$y_yes))
##     0         1 
#0.7730024 0.2269976
dim(train1)
#6605   30
dim(train1)


library(DMwR)
train1$y_yes <- as.factor(train1$y_yes)
train1 <- SMOTE(y_yes ~ ., train1, perc.over = 100, perc.under=200)

model <- glm (y_yes ~ ., data = train1, family = binomial) 
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(train1$y_yes, predict > 0.5)

pred <- predict(model, newdata = train1, type = "response")
hist(pred)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- train1$y_yes
y_act
table(y_pred_num,y_act)
mean(y_pred == y_act)
y_pred

confusionMatrix(y_pred,y_act)

library(ROCR)
ROCRpred <- prediction(predict, train1$y_yes)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))




