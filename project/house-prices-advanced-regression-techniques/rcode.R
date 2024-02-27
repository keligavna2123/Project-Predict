regression['target_variable']
target.variable = regression['target_variable']
feature.variable = subset(regression,select = -target_variable)





#removing features
#####-----------------------------------------------------
#forward selection method
initial.model <- lm(target_variable~., data = regression)
final_model <- step(initial.model, direction='backward')
summary(final_model)


#principal component analysis
pca_result = prcomp(regression[,-1],scale=TRUE)

summary(pca_result)


#proportion of variance explained by each principal component
prop_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
prop_var


plot(prop_var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

#extract the principal components
View(pca_result$x)
pcs = pca_result$x

#checking the target variable
target.variable

#performing linear regression using principal component regression
model <- lm(regression$target_variable~pcs[,1:300])
summary(model)



#performing ridge regression
library(glmnet)
feature.variable
target.variable

x=as.matrix(feature.variable)
y=target.variable

#fit ridge regression 
ridge_model <- glmnet(x,y,alpha=0,lambda=0.1)

summary(ridge_model)



#####------------------------------------------------------------------



#splitting the data into train test
set.seed(1)
n_rows <- nrow(regression)

#number of rows to select
nrow_to_select <- round(0.20*n_rows)
#randomly select 20 % of the rows
selected_indices <- sample(1:n_rows,size=nrow_to_select,replace=FALSE)
selected_indices


test_data <- regression[selected_indices,]
dim(test_data)
train_data <- regression[-selected_indices,]
dim(train_data)
model <- lm(train_data$target_variable~.,data=train_data)
summary(model)






test_prediction <-predict(model,subset(test_data,select=-target_variable))


mean_response = mean(test_response)
TSS<- sum((test_data$target_variable-mean_response)^2)
RSS<- sum((test_data$target_variable-test_prediction)^2)
n <- nrow(test_data)
num_predictors <- length(coef(model))-1

# Calculate the adjusted R-squared value for the test data
adjusted_r_squared <- 1 - ((RSS / (n - num_predictors - 1)) / (TSS / (n - 1)))




#checking homoscedastic
head(regression)
colnames(regression)
model <- lm(target_variable~.,data = regression)
residuals(model)
plot(residuals(model))



#checking multicollinearity
#checking multicollinearity through vif values
library(car)
vif_values <- vif(model)
#there are aliased coefficients  in the model

model1 <- lm(target_variable~., data=new_regression)
vif_values1 <- vif(model1)

#checking correlation------------------------------------

# Create or load your data
# For demonstration purposes, let's create a sample dataset
set.seed(123)
data <- as.data.frame(new_regression)

# Calculate the correlation matrix
correlation_matrix <- cor(data)

# Find pairs of variables with correlation greater than 0.9
high_correlation_pairs <- which(correlation_matrix > 0.9 & correlation_matrix != 1, arr.ind = TRUE)

# Extract the variable pairs
variable_pairs <- rownames(correlation_matrix)[high_correlation_pairs[, 1]]
correlated_variable_pairs <- cbind(variable_pairs, colnames(correlation_matrix)[high_correlation_pairs[, 2]])

print(correlated_variable_pairs)
length(variable_pairs)

#removing the variables with high correlation in the data
library(dplyr)
#new dataframe with removed correlation 
select(new_regression,-variable_pairs)

model2 <- lm(target_variable~.,data = select(new_regression,-variable_pairs))
vif(model2)




