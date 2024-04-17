#assignment8
mydata<- data.frame(wine)
column_names <- colnames(wine)
print(column_names)
summary(mydata)
library(ggplot2)
#

library(dplyr)
columns_to_check <- c("Alcohol", "Sulphates", "TotalSulfurDioxide", 
                      "ResidualSugar", "FreeSulfurDioxide", "pH", "Chlorides")
rows_with_nas <- rowSums(is.na(mydata[, columns_to_check])) > 0
clean1 <- mydata[!rows_with_nas, ]
sub1 <- subset(clean1, select = -STARS)
#cleandata <- na.omit(mydata)
#summary(sub1)
table(sub1$STARS)
summary(clean1)
table(clean1$STARS)
ggplot(clean1) +
  geom_bar( aes(STARS) ) +
  ggtitle("Status Variable") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#decision tree to fill stars missing value
#install.packages("rpart")
#install.packages("dplyr")
library(rpart)
#library(dplyr)
train_data <- clean1[!is.na(clean1$STARS), ]
predict_data <- clean1[is.na(clean1$STARS), ]
model <- rpart(STARS ~ ., data = train_data, method = "class")
predicted_STARS <- predict(model, newdata = predict_data, type = "class")
clean1$STARS[is.na(clean1$STARS)] <- predicted_STARS
ggplot(clean1) +
  geom_bar( aes(STARS) ) +
  ggtitle("After Filling Missing Value STARS") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(clean1$STARS)
ggplot(sub1) +
  geom_bar( aes(Purchase) ) +
  ggtitle("Status Variable") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(sub1$Purchase)
ggplot(sub1) +
  geom_bar( aes(Cases) ) +
  ggtitle("Cases") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(sub1$Cases)

ggplot(sub1, aes(x=FixedAcidity)) + 
  geom_histogram(color="black", binwidth= 1) +
  labs(title="Distribution of FixedAcidity") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=VolatileAcidity)) + 
  geom_histogram(color="black", binwidth= 0.1) +
  labs(title="Distribution of VolatileAcidity") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=CitricAcid)) + 
  geom_histogram(color="black", binwidth= 0.1) +
  labs(title="Distribution of CitricAcid") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=ResidualSugar)) + 
  geom_histogram(color="black", binwidth= 1) +
  labs(title="Distribution of ResidualSugar") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=Chlorides)) + 
  geom_histogram(color="black", binwidth= 0.1) +
  labs(title="Distribution of Chlorides") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=FreeSulfurDioxide)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of FreeSulfurDioxide") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=TotalSulfurDioxide)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of TotalSulfurDioxide") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=Density)) + 
  geom_histogram(color="black", binwidth= 0.1) +
  labs(title="Distribution of Density") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=pH)) + 
  geom_histogram(color="black", binwidth= 0.1) +
  labs(title="Distribution of pH") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=Sulphates)) + 
  geom_histogram(color="black", binwidth= 0.1) +
  labs(title="Distribution of Sulphates") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1, aes(x=Alcohol)) + 
  geom_histogram(color="black", binwidth= 3) +
  labs(title="Distribution of Alcohol") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1) +
  geom_bar( aes(LabelAppeal) ) +
  ggtitle("LabelAppeal") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(sub1$LabelAppeal)
ggplot(sub1) +
  geom_bar( aes(AcidIndex) ) +
  ggtitle("AcidIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(mydata$AcidIndex)
#bivariate

ggplot(sub1, aes(x = factor(Cases), fill = factor(Purchase))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), 
                    name = "Purchase", 
                    labels = c("0" = "No Purchase", "1" = "Purchase")) +
  labs(title = "Purchase Count by AcidIndex",
       x = "AcidIndex",
       y = "Count") +
  theme_minimal()


#correlation
res1 <- clean1$Purchase
res2 <- clean1$INDEX# Extract the response column
names(clean1)
# Remove the response column from the dataframe for correlation calculation
df_without_response<- clean1[, !names(clean1) %in% c("Purchase", "INDEX")]

# Compute correlation of the response with each of the other columns
correlations <- sapply(df_without_response, function(x) cor(x, res1, use = "complete.obs"))
print(correlations)

#extreme value
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 6 * IQR
  upper_bound <- Q3 + 6 * IQR
  
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Apply the function to each numeric column in the dataset
outliers_list <- lapply(clean1[sapply(clean1, is.numeric)], find_outliers)

# Print the outliers for each variable
outliers_list



num_negative_values <- sum(clean1$FixedAcidity < 0, na.rm = TRUE)
print(paste("The number of negative values in the variable is:",
            num_negative_values))


#model fitting
#categorical variables
clean1 <- clean1[ , !(names(clean1) %in% c("INDEX"))]

#robust scaling
robust_scale <- function(x) {
  median_x <- median(x, na.rm = TRUE)
  iqr_x <- IQR(x, na.rm = TRUE)
  return ((x - median_x) / iqr_x)
}

# Apply robust scaling 
clean1$RB_FreeSulfurDioxide <- robust_scale(clean1$FreeSulfurDioxide)
clean1$RB_TotalSulfurDioxide <- robust_scale(clean1$TotalSulfurDioxide)
clean1$RB_FixedAcidity <- robust_scale(clean1$FixedAcidity)
clean1$RB_ResidualSugar <- robust_scale(clean1$ResidualSugar)


clean2 <- clean1
clean2 <-clean2[ , !(names(clean2) %in% c("Cases","Cases0",
                                          "LabelAppeal",
                                          "STARS",
                                          "AcidIndex"
                                          ))]
#"FreeSulfurDioxide"
#"TotalSulfurDioxide", 
#"FixedAcidity",
#"ResidualSugar"
clean1$Cases <- as.factor(clean1$Cases)
clean1$STARS <- as.factor(clean1$STARS)
clean1$LabelAppeal <- as.factor(clean1$LabelAppeal)
clean1$AcidIndex <- as.factor(clean1$AcidIndex)

ggplot(clean1, aes(x = factor(Purchase), y = RB_FreeSulfurDioxide, fill = factor(Purchase))) +
  geom_boxplot() +
  labs(x = "Purchase", y = "TotalSulfurDioxide", title = "Boxplot of TotalSulfurDioxide by PURCHASE") +
  scale_fill_manual(values = c("blue", "green","black", "pink","yellow","orange"), name = "Purchase", labels = c("0", "1")) +
  theme_minimal()

ggplot(clean1, aes(x = factor(Purchase), y = pH, color = factor(Purchase))) +
  geom_boxplot(outlier.shape = 8) + # using a different shape for outliers
  labs(title = "Boxplot of Alcohol by Purchase",
       x = "Purchase",
       y = "Alcohol") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

# Create dummy variables
data1 <- data.frame(model.matrix(~ Cases + STARS + LabelAppeal + AcidIndex - 1, data = clean1))
combined_data <- cbind(clean2, data1)

# View the first few rows of the combined dataset
head(combined_data)

# View the structure of the combined dataset to ensure the combine was successful
str(combined_data)
names(combined_data)

#stepwise
library(MASS) 
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(combined_data), 0.7 * nrow(combined_data))
train_data <- combined_data[train_indices, ]
test_data <- combined_data[-train_indices, ]

#initial modeling
mod1 <- glm(Purchase ~ VolatileAcidity +CitricAcid +
              Chlorides + Alcohol + Density+pH +RB_FreeSulfurDioxide
            +RB_FixedAcidity+RB_ResidualSugar+RB_TotalSulfurDioxide +
              Sulphates + 
              #Cases1 +Cases2 + Cases3+Cases4+Cases5+Cases6+Cases7
              #+Cases8+
              STARS2+STARS3+STARS4+
              LabelAppeal.1+LabelAppeal0+LabelAppeal1+LabelAppeal2+
              AcidIndex5+AcidIndex6+AcidIndex7+AcidIndex8+AcidIndex9
              +AcidIndex10+AcidIndex11+AcidIndex12+AcidIndex13+
                AcidIndex14+AcidIndex15+AcidIndex16+
                AcidIndex17,
            data = train_data, family = binomial)
stepwise_model <- stepAIC(mod1, direction = "both")
library(car)
sort(vif(stepwise_model),decreasing=TRUE)
anova(stepwise_model)
summary(stepwise_model)
par(mfrow=c(2,2))
plot(stepwise_model)

library(influence.ME)
# Calculate Cook's distance for each observation
cooks.distance <- cooks.distance(stepwise_model)
cooks_threshold <- 4 / (nrow(stepwise_model$model) - length(coef(stepwise_model)) - 1)
# Calculate leverage (hat) values
hatvalues <- hatvalues(stepwise_model)
leverage_threshold <- 2 * (length(coef(stepwise_model)) + 1) / nrow(stepwise_model$model)
# Calculate DFFITS
dffits <- dffits(stepwise_model)
dffits_threshold <- 2 * sqrt(length(coef(stepwise_model)) / nrow(stepwise_model$model))

# Plot Cook's distance
plot(cooks.distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/(length(cooks.distance)-2), col = "red", lty = 2)

# Plot leverage values
plot(hatvalues, type = "h", main = "Leverage Values", ylab = "Hat Values")
abline(h = 2*mean(hatvalues), col = "red", lty = 2)

# Plot DFFITS values
plot(dffits, type = "h", main = "DFFITS", ylab = "DFFITS")
abline(h = c(-1, 1) * 2*sqrt(2/length(dffits)), col = "red", lty = 2)

#Deleting high leverage points
# Identify high leverage points
high_leverage_points <- which(hatvalues > leverage_threshold)

# Identify influential points based on Cook's distance
influential_points_cooks <- which(cooks.distance > cooks_threshold)

# Identify influential points based on DFFITS
influential_points_dffits <- which(abs(dffits) > dffits_threshold)

# Combine the three criteria to get a list of points to potentially remove
points_to_remove <- union(high_leverage_points, influential_points_cooks)
points_to_remove <- union(points_to_remove, influential_points_dffits)

# Step 5: Remove the points from the dataset
cleaned_data <- stepwise_model$model[-points_to_remove, ]
mod2 <- glm(Purchase ~ .,
            data = cleaned_data, family = binomial)
stepwisemodel2 <- stepAIC(mod2, direction = "both")

# Step 6: Perform stepwise selection on the cleaned data if desired
# (Optional) Use stepAIC to perform stepwise selection on the cleaned data
# cleaned_stepwise_model <- stepAIC(cleaned_stepwise_model, direction = "both")

# Summary of the new model
sort(vif(stepwisemodel2),decreasing=TRUE)
anova(stepwisemodel2)
summary(stepwisemodel2)
par(mfrow=c(2,2))
plot(stepwisemodel2)


#model3
mod3 <- glm(Purchase ~ VolatileAcidity+Chlorides+Alcohol+pH+RB_FreeSulfurDioxide
            +RB_TotalSulfurDioxide+Sulphates, data = cleaned_data,
            family = binomial)
stepwisemodel3 <- stepAIC(mod3, direction = "both")
anova(stepwisemodel3)
summary(stepwisemodel3)
par(mfrow=c(2,2))
plot(stepwisemodel3)
summary(mod3)

test_data$predicted1 <- predict(stepwise_model, newdata = test_data, 
                               type = "link")

test_data$predicted_class1 <- ifelse(test_data$predicted1 > 0.5, 1, 0)
test_data$predicted2 <- predict(stepwisemodel2, newdata = test_data, 
                                type = "link")

test_data$predicted_class2 <- ifelse(test_data$predicted2 > 0.5, 1, 0)


#Predictions:
library(caret)
confusionMatrix1 <- confusionMatrix(as.factor(test_data$predicted_class1), as.factor(test_data$Purchase))
print(confusionMatrix1)

# Confusion Matrix for the second model
confusionMatrix2 <- confusionMatrix(as.factor(test_data$predicted_class2), as.factor(test_data$Purchase))
print(confusionMatrix2)

# Calculate Accuracy for both models
accuracy1 <- sum(test_data$predicted_class1 == test_data$Purchase) / length(test_data$Purchase)
accuracy2 <- sum(test_data$predicted_class2 == test_data$Purchase) / length(test_data$Purchase)

# Compare Accuracy
print(paste("Accuracy of Model 1:", accuracy1))
print(paste("Accuracy of Model 2:", accuracy2))

# If you want to compute other metrics manually, such as Precision, Recall, and F1-Score:
precision1 <- posPredValue(as.factor(test_data$predicted_class1), as.factor(test_data$Purchase), positive = "1")
recall1 <- sensitivity(as.factor(test_data$predicted_class1), as.factor(test_data$Purchase), positive = "1")
f1_score1 <- (2 * precision1 * recall1) / (precision1 + recall1)

precision2 <- posPredValue(as.factor(test_data$predicted_class2), as.factor(test_data$Purchase), positive = "1")
recall2 <- sensitivity(as.factor(test_data$predicted_class2), as.factor(test_data$Purchase), positive = "1")
f1_score2 <- (2 * precision2 * recall2) / (precision2 + recall2)

# Compare Precision, Recall, and F1-Score
print(paste("Precision of Model 1:", precision1))
print(paste("Recall of Model 1:", recall1))
print(paste("F1-Score of Model 1:", f1_score1))

print(paste("Precision of Model 2:", precision2))
print(paste("Recall of Model 2:", recall2))
print(paste("F1-Score of Model 2:", f1_score2))

#roc curve
install.packages("pROC")
library(pROC)
roc1 <- roc(test_data$Purchase, test_data$predicted1)
roc2 <- roc(test_data$Purchase, test_data$predicted2)

# Plot the ROC curve for Model 1
plot(roc1, main="ROC Curves", col="blue")

# Add the ROC curve for Model 2
plot(roc2, add=TRUE, col="red")

# Add a legend
legend("bottomright", legend=c("Model 1", "Model 2"), col=c("blue", "red"), lwd=2)
auc1 <- auc(roc1)
auc2 <- auc(roc2)
roc.test(roc1, roc2)