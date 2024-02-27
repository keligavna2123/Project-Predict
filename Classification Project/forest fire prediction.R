getwd()
setwd('C:/Users/Dell/Desktop/balsingh project (1)/balsingh project')
library(readxl)
df1 <- read_excel("bajaia dataset.xlsx")
#View(df1)
df2 <- read_excel("sidi-bel Abbes dataset.xlsx")
#View(df2)

#checking the null values across all the columns of df1 and df2
for(i in colnames(df1)){
  print(sum(is.na(df1[,i])))
}

for(j in colnames(df2)){
  print(sum(is.na(df2[,j])))
}


onehot.encoding <- function(v){
  if(v == 'not fire'){
    print(0)
  }else{
    print(1)
  }
}

df1$Classes <- sapply(df1$Classes,onehot.encoding)
#View(df1)
df2$Classes <- sapply(df2$Classes,onehot.encoding)
#View(df2)

#deleting useless variable
df1 <- subset(df1,select = -c(1,2,3))
df2 <- subset(df2,select = -c(1,2,3))

#View(df1)
#View(df2)

str(df1)
str(df2)

df1$Classes <- as.numeric(df1$Classes)
df2$Classes <- as.numeric(df2$Classes)


#changing the DC to integer type
df2$DC <- as.numeric(as.character(df2$DC))
mean(df2$DC,na.rm = T)
df2$DC[is.na(df2$DC)] <- mean(df2$DC,na.rm = T)


#plotting
library(ggplot2)
ggplot(df1,aes(x=RH))+geom_density()
shapiro.test(df1$RH)
ggplot(df1,aes(x=Ws))+geom_density()
shapiro.test(df1$Ws)
ggplot(df1,aes(x))
ggplot(df1,aes(x=Rain))+geom_density()
shapiro.test(df1$Rain)
ggplot(df1,aes(x=FFMC))+geom_density()
shapiro.test(df1$FFMC)
ggplot(df1,aes(x=DMC))+geom_density()
shapiro.test(df1$DMC)
ggplot(df1,aes(x=DC))+geom_density()
shapiro.test(df1$DC)
ggplot(df1,aes(x=ISI))+geom_density()
shapiro.test(df1$ISI)
ggplot(df1,aes(x=BUI))+geom_density()
shapiro.test(df1$BUI)
ggplot(df1,aes(x=FWI))+geom_density()
shapiro.test(df1$FWI)


# Install and load the necessary packages
install.packages("GGally")
library(GGally)

# Create pair plot
pair_plot <- ggpairs(df1)

# Display the pair plot
print(pair_plot)


ggplot(df1,aes(x=Temperature,color=factor(Classes)))+geom_bar()+theme_bw()
ggplot(df2,aes(x=Temperature,color=factor(Classes)))+geom_bar()+theme_light()
#pending


shapiro.test(df1$ISI)
shapiro.test(df1$FWI)
shapiro.test(df1$BUI)
shapiro.test(df1$DMC)
shapiro.test(df1$DC)
shapiro.test(df1$FFMC)
shapiro.test(df1$Rain)


#objective to check the difference of means using wilcox_test 
library(ggplot2)
ggplot(df1,aes(y=Temperature,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$Temperature~df1$Classes, data = df1)
ggplot(df1,aes(y=Rain,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$Rain~df1$Classes,data=df1)
ggplot(df1,aes(y=RH,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$RH~df1$Classes,data=df1)
ggplot(df1,aes(y=Ws,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$Ws~df1$Classes,data=df1)
ggplot(df1,aes(y=FFMC,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$FFMC~df1$Classes,data=df1)
ggplot(df1,aes(y=DMC,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$DMC~df1$Classes,data=df1)
ggplot(df1,aes(y=DC,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$DC~df1$Classes,data=df1)
ggplot(df1,aes(y=ISI,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$ISI~df1$Classes,data=df1)
ggplot(df1,aes(y=BUI,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$BUI~df1$Classes,data=df1)
ggplot(df1,aes(y=FWI,x=factor(Classes)))+geom_boxplot()+theme_light()
wilcox.test(df1$FWI~df1$Classes,data=df1)



# Create the training and testing sets using createDataPartition()
train_proportion <- 0.8
set.seed(300)
num_rows <- train_proportion*dim(df1)[1]
random_indices <- sample(nrow(df1),num_rows)
train_dataset <- df1[random_indices,]
test_dataset <- df1[-random_indices,]
test_dataset_X <- test_dataset[,-11]
test_dataset_Y <- test_dataset[,11]
dim(train_dataset)
dim(test_dataset)

#model running on train dataset  


## Logistic regression 

#install.packages('Metrics')
library(Metrics)
?glm
model1 <- glm(train_dataset$Classes~.,data=train_dataset,family=binomial)
summary(model1)
?predict
predictions <- predict(model1,newdata = test_dataset_X,respose='class')
predictions
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
binary_predictions

# Create the confusion matrix
conf_matrix <- table(binary_predictions, test_dataset_Y$Classes)
conf_matrix

# Calculate evaluation metrics based on the confusion matrix
accuracy_value <- sum(diag(conf_matrix)) / sum(conf_matrix)
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf_matrix)
print(paste("test data Accuracy:", accuracy_value))


#overfitting and underfitting

#View(train_dataset)
## training data accuracy
train_predictions <- predict(model1,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
train_accuracy <- accuracy(binary_predictions_train, train_dataset$Classes)
train_accuracy
print(paste("train dataset accuracy",train_accuracy))

## ROC CURVE

install.packages("ROCR")
library(ROCR)
predictions=prediction(binary_predictions, test_dataset_Y$Classes)
perf1 <- performance(predictions, "tpr", "fpr")
auc_value <- performance(predictions, "auc")@y.values[[1]]
par(mar = c(2, 2, 2, 2))
plot(perf1, colorize = TRUE, main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "black", lty = 1, cex = 0.8)

### PRECISION MEASURE , RECALL MEASURE

install.packages("caret")
install.packages("MLmetrics")

library(caret)
library(MLmetrics)


# Create a confusion matrix
cm <- confusionMatrix(factor(binary_predictions), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary_predictions,test_dataset_Y$Classes)
precision
recall <- Recall(binary_predictions,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary_predictions,test_dataset_Y$Classes)
f1_score




## FITIING SVM MODEL

#install.packages("e1071")
library(e1071)
## checked model for linear,polynomial,sigmoid kernel highest acccuracy was obtained for radial kernel with varying cost
svm_model=svm(train_dataset$Classes~.,data=train_dataset,kernel='radial',cost=10)
svm_model
summary(result)


predictions <- predict(svm_model,newdata = test_dataset_X,type="response")
predictions
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
binary_predictions
length(binary_predictions)
length(test_dataset_Y$Classes)
# Create the confusion matrix
conf_matrix <- table(binary_predictions, test_dataset_Y$Classes)
conf_matrix

# Calculate evaluation metrics based on the confusion matrix
accuracy_value <- sum(diag(conf_matrix)) / sum(conf_matrix)
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf_matrix)
print(paste("Accuracy:", accuracy_value))

## training data accuracy
train_predictions <- predict(svm_model,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
train_accuracy <- accuracy(binary_predictions_train, train_dataset$Classes)
train_accuracy
print(paste("train dataset accuracy",train_accuracy))


## plotiing ROC CURVE

predictions <- prediction(binary_predictions,test_dataset_Y$Classes)
perf2 <- performance(predictions, "tpr", "fpr")
auc_value <- performance(predictions, "auc")@y.values[[1]]
par(mar = c(2, 2, 2, 2))
plot(perf2, main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "black", lty = 1, cex = 0.8)



## PRECISON AND RECALL FOR SVM

library(caret)
library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary_predictions), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary_predictions,test_dataset_Y$Classes)
precision
recall <- Recall(binary_predictions,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary_predictions,test_dataset_Y$Classes)
f1_score

## descion trees
 

#install.packages("rpart")
library(rpart)
?rpart
fit2 <- rpart(train_dataset$Classes~.,data=train_dataset,parms = list(prior = c(.65,.35), split = "information"))
fit3 <- rpart(train_dataset$Classes~.,data=train_dataset,control = rpart.control(cp = 0.01))
fit2
fit3
summary(fit2)
par(xpd=TRUE)
plot(fit2,compress = TRUE); text(fit2, all=TRUE, cex=0.5,use.n=TRUE)
plot(fit3,compress = TRUE); text(fit3, all=TRUE, cex=0.5,use.n=TRUE)
prediction <- predict(fit2,newdata = test_dataset_X)
prediction
?par
binary <- ifelse(prediction >= 0.5, 1, 0)
length(binary)
length(test_dataset_Y$Classes)
# Create the confusion matrix
conf<- table(binary, test_dataset_Y$Classes)
conf
# Calculate evaluation metrics based on the confusion matrix
accuracy_value12 <- sum(diag(conf)) / sum(conf)
accuracy_value12
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf)
print(paste("Accuracy:", accuracy_value12))


## training data accuracy
train_predictions <- predict(fit2,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
train_accuracy <- accuracy(binary_predictions_train, train_dataset$Classes)
train_accuracy
print(paste("train dataset accuracy",train_accuracy))


## plotiing ROC CURVE

predictions <- prediction(binary,test_dataset_Y$Classes)
perf3 <- performance(predictions, "tpr", "fpr")
auc_value <- performance(predictions, "auc")@y.values[[1]]
par(mar = c(2, 2, 2, 2))
plot(perf3, main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "black", lty = 1, cex = 0.8)



## PRECISON AND RECALL FOR SVM

library(caret)
library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary,test_dataset_Y$Classes)
precision
recall <- Recall(binary,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary,test_dataset_Y$Classes)
f1_score




## random forest

##install.packages("randomForest")
library(randomForest)
?randomForest
model=randomForest(train_dataset$Classes~.,data=train_dataset,ntree=10,method="classification")
model
summary(model1)
prediction <- predict(model,newdata =test_dataset_X)
prediction
?par
binary <- ifelse(prediction >= 0.5, 1, 0)
length(binary)
length(test_dataset_Y$Classes)
# Create the confusion matrix
conf1<- table(binary, test_dataset_Y$Classes)
conf1
# Calculate evaluation metrics based on the confusion matrix
accuracy_value123 <- sum(diag(conf1)) / sum(conf1)
accuracy_value123
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf1)
print(paste("Accuracy:", accuracy_value123))
## training data accuracy
train_predictions <- predict(model,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
train_accuracy <- accuracy(binary_predictions_train, train_dataset$Classes)
train_accuracy
print(paste("train dataset accuracy",train_accuracy))


## plotiing ROC CURVE

predictions <- prediction(binary,test_dataset_Y$Classes)
perf4 <- performance(predictions, "tpr", "fpr")
auc_value <- performance(predictions, "auc")@y.values[[1]]
par(mar = c(2, 2, 2, 2))
plot(perf4, main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "black", lty = 1, cex = 0.8)



## PRECISON AND RECALL FOR random forest

library(caret)
library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary,test_dataset_Y$Classes)
precision
recall <- Recall(binary,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary,test_dataset_Y$Classes)
f1_score










## ANN 
# Install and load the neuralnet package
#install.packages("neuralnet")
library(neuralnet)

# Create a neural network model
?neuralnet
nn <- neuralnet(train_dataset$Classes~.,data=train_dataset,hidden = c(5,2),linear.output=FALSE,act.fct = "logistic")
nn
summary(nn)
# Plot the neural network
plot(nn)

predicted <-predict(nn,newdata=test_dataset_X)
predicted
binary <- ifelse(predicted>= 0.5, 1, 0)
length(binary)
length(test_dataset_Y$Classes)
# Create the confusion matrix
conf1<- table(binary, test_dataset_Y$Classes)
conf1
# Calculate evaluation metrics based on the confusion matrix
accuracy_value123 <- sum(diag(conf1)) / sum(conf1)
accuracy_value123
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf1)
print(paste("Accuracy:", accuracy_value123))
              

## training data accuracy
train_predictions <- predict(nn,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
train_accuracy <- accuracy(binary_predictions_train, train_dataset$Classes)
train_accuracy
print(paste("train dataset accuracy",train_accuracy))


## plotiing ROC CURVE

predictions <- prediction(binary,test_dataset_Y$Classes)
perf <- performance(predictions, "tpr", "fpr")
auc_value <- performance(predictions, "auc")@y.values[[1]]
par(mar = c(2, 2, 2, 2))
plot(perf, main = "ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "black", lty = 1, cex = 0.8)



## PRECISON AND RECALL FOR SVM

library(caret)
library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary,test_dataset_Y$Classes)
precision
recall <- Recall(binary,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary,test_dataset_Y$Classes)
f1_score









library(ggplot2)

# Create a data frame
df <- data.frame(perf1,perf2,perf3,perf4)

# Plot the lines
ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line() +
  labs(x = "X", y = "Y", title = "Multiple Lines on Same Plot")




