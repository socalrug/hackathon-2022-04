# Import packages
library(dplyr)
library(ggplot2)
library(smbinning)
library(GoodmanKruskal)
library(caret)
library(pROC)
library(ROCR)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
# Load data 
df <- read.table(file.choose(), header=T, sep=";")
str(df)
summary(df)
df1 <- df
df <- df1

# Categorize jobs into job groups
df_clustering <- subset(df, select = c(age, job, marital, education, default, 
                                       balance, housing, loan))
df_clustering <- df_clustering %>%
  mutate(single = ifelse(marital == 'single', 1, 0)) %>%
  mutate(married = ifelse(marital == 'married', 1, 0)) %>%
  mutate(primary = ifelse(education == 'primary', 1, 0)) %>%
  mutate(secondary = ifelse(education == 'secondary', 1, 0)) %>%
  mutate(tertiary = ifelse(education == 'tertiary', 1, 0)) %>%
  mutate(default = ifelse(default == 'yes', 1, 0)) %>%
  mutate(housing = ifelse(housing == 'yes', 1, 0)) %>%
  mutate(loan = ifelse(loan  == 'yes', 1, 0))
df_clustering2 <- subset(df_clustering, select = -c(job, marital, education))

# Normalize the data 
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

# Normalize data
df_clustering2 <- df_clustering2 %>%
  mutate(age = normalize(age)) %>%
  mutate(balance = normalize(balance))

# Using the SSE curve method
set.seed(1234)
SSE_curve <- c()
for (n in 1:15) {
  kcluster <- kmeans(df_clustering2, n, nstart = 5)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}
SSE_curve
plot(1:15, SSE_curve, type = "b", xlab = "Number of Clusters", ylab = "SSE")

# k = 5 as the optimal number
set.seed(1234)
k5 <- kmeans(df_clustering2, centers = 5)
df_clustering <- mutate(df_clustering, Cluster = as.factor(k5$cluster))
ggplot(df_clustering, aes(job, fill = Cluster)) + geom_bar(position = 'dodge')

# Categorize job into cluster groups
jobgroup1 <- c("entrepreneur","management","self-employed")
jobgroup2 <- c("blue-collar","housemaid","retired")
jobgroup3 <- c("unknown")
jobgroup4 <- c("student")
jobgroup5 <- c("admin.","services","technician","unemployed")

df_new <- df %>%
  mutate(jobgroup = ifelse(job %in% jobgroup1, "JGroup_1",
                           ifelse(job %in% jobgroup2, "JGroup_2",
                                  ifelse(job %in% jobgroup3, "JGroup_3",
                                         ifelse(job %in% jobgroup4, "JGroup_4",
                                                ifelse(job %in% jobgroup5, "JGroup_5", "NA"))))))
df_new$jobgroup <- factor(df_new$jobgroup, levels = c("JGroup_1", "JGroup_2", "JGroup_3", 
                                            "JGroup_4", "JGroup_5"))

# Decide the optimal bin number for continuous variables
df_new <- df_new %>% 
  mutate(y_num = ifelse(y == "no", 0, 1))
result_a <- smbinning(df=df_new, y="y_num", x="age")
result_a$ivtable
result_b <- smbinning(df=df_new, y="y_num", x="balance")
result_b$ivtable
result_d <- smbinning(df=df_new, y="y_num", x="duration", p=0.1)
result_d$ivtable
result_c <- smbinning(df=df_new, y="y_num", x="campaign", p=0.1)
result_c$ivtable
# Convert continuous data into categories
## Age
df_new <- mutate(df_new, age.group = ifelse(age <= 29.00, "29-", 
                                    ifelse(age <= 37.00, "30-37",
                                           ifelse(age <= 58.00, "38-58", "59+"))))
df_new$age.group <- factor(df_new$age.group, levels = c("29-", "30-37", "38-58", "59+"))
## balance
df_new <- mutate(df_new, balance.group = ifelse(balance <= -47, "-47-", 
                                        ifelse(balance <= 60, "-46-60",
                                               ifelse(balance <= 798, "61-798", "799+"))))
df_new$balance.group <- factor(df_new$balance.group, levels = c("-47-", "-46-60", "61-798", "799+"))
## duration
df_new <- mutate(df_new, duration.group = ifelse(duration <= 77, "77-", 
                                         ifelse(duration <= 129, "78-129",
                                                ifelse(duration <= 163, "130-163",
                                                       ifelse(duration <= 205, "164-205",
                                                              ifelse(duration <= 259, "206-259",
                                                                     ifelse(duration <= 348, "260-348",
                                                                            ifelse(duration <= 521, "349-521", "522+"))))))))
df_new$duration.group <- factor(df_new$duration.group, 
                                levels = c("77-", "78-129", "130-163", "164-205", 
                                           "206-259", "260-348", "349-521", "522+"))
## campaign
df_new <- mutate(df_new, campaign.group = ifelse(campaign <= 1, "1", 
                                         ifelse(campaign <= 2, "2", 
                                                ifelse(campaign <= 4, "3-4", "5+"))))
df_new$campaign.group <- factor(df_new$campaign.group, levels = c("1", "2", "3-4", "5+"))
## pdays
df_new <- mutate(df_new, contacted = ifelse(pdays == 1, "yes", "no"))
df_new$contacted <- factor(df_new$contacted, levels = c("yes", "no"))
summary(df_new)
str(df_new)

# Subset the continuous or less correlated variables
df_new_m <- subset(df_new, select = -c(age, job, balance, day, y_num, duration, campaign, pdays, previous))
write.csv(df_new_m, file = 'bank_CleanedData.csv')
## Visualization 
colNames <- names(df_new_m)[-9]
for (i in colNames){
  ggplot(df_new_m, aes(i, fill = y)) + geom_bar(position = 'fill')
}

ggplot(df_new_m, aes(marital, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(education, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(default, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(housing, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(loan, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(contact, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(month, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(poutcome, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(jobgroup, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(age.group, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(balance.group, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(duration.group, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(campaign.group, fill = y)) + geom_bar(position = 'fill')
ggplot(df_new_m, aes(contacted, fill = y)) + geom_bar(position = 'fill')
## Correlation matrix to remove default, contacted
GKmat <- GKtauDataframe(df_new_m, dgts = 5)
plot(GKmat, diagSize = 0.8)
GKmat
df_new_m <- subset(df_new_m, select = -c(default, contacted))

# Machine learning models
set.seed(1234)
tv.rows <- createDataPartition(y= df_new_m$y, p=0.90, list = FALSE)
tv.data <- df_new_m[tv.rows,] 
table(tv.data$y)
test.data<- df_new_m[-tv.rows,] 
table(test.data$y)

set.seed(1234)
train.rows <- createDataPartition(y= tv.data$y, p=0.75, list = FALSE)
train.data <- tv.data[train.rows,] 
table(train.data$y)
validate.data <- tv.data[-train.rows,] 
table(validate.data$y)

## Navie Bayes
set.seed(1234)
fitControl <- trainControl(method = "cv", number = 10, classProbs = TRUE)
set.seed(1234)
nb <- train(x = train.data[,-8], y = train.data$y, method = "nb", 
            trControl = fitControl, metric = "ROC")

### Validate
nb.pred <- predict(nb, newdata = validate.data)
confusionMatrix(nb.pred, validate.data$y)
### Level of importance
x.nb <- varImp(nb)
impTab <- x.nb$importance
ggplot(impTab, aes(x= reorder(row.names(impTab), +yes), y=yes)) + 
  geom_bar(stat = 'identity', aes(fill = row.names(impTab))) +  
  labs(title = "Variable in predicting term deposit", x = "Variables", y = "Importance") +  
  scale_fill_brewer(palette = "Set3") + coord_flip() +
  theme_classic() +
  theme(legend.position = "none")
### ROC
roc.nb.pred <- prediction(predict(nb, type = "prob")[, 2], train.data$y)
plot(performance(roc.nb.pred, "tpr", "fpr"))
abline(a=0, b= 1)

## Decision Tree
### Tree
set.seed(1234)
dtree <- rpart(y ~ ., data = train.data, method="class", parms=list(split="information"))
### pruned tree
dtree$cptable
opt <- which.min(dtree$cptable[,"xerror"])
cp <- dtree$cptable[opt, "CP"]
dtree.pruned <- prune(dtree, cp)
rpart.plot(dtree.pruned)
### Evaluate tree
dtree.pred <- predict(dtree.pruned, validate.data, type="class")
confusionMatrix(dtree.pred, validate.data$y)
### Variable importance
x.dt <- dtree.pruned$variable.importance
ImpTab2 <- as.data.frame(x.dt)
ggplot(ImpTab2, aes(x= reorder(row.names(ImpTab2), +x.dt), y=x.dt)) + 
  geom_bar(stat = 'identity', aes(fill = row.names(ImpTab2))) +  
  labs(title = "Variable in predicting term deposit", x = "Variables", y = "Importance") +  
  scale_fill_brewer(palette = "Set1") + coord_flip() +
  theme_classic() +
  theme(legend.position = "none")
### ROC
roc.dtree.pred <- prediction(predict(dtree.pruned, validate.data, type = "prob")[, 2], validate.data$y)
plot(performance(roc.dtree.pred, "tpr", "fpr"))
abline(a=0, b = 1)

## Test accuracy
### Evaluate Tree with test dataset
dtree.test <- predict(dtree.pruned, test.data, type="class")
confusionMatrix(dtree.test, test.data$y)

