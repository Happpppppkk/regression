#assignment4
mydata<- data.frame(ames_housing_data)
str(mydata)
head(mydata)
names(mydata)
library(ggplot2)
ggplot(mydata, aes(x = YearBuilt)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Year Built in Ames Housing Dataset",
       x = "Year Built"
      )
#Define the sample population
#excluding house older than 70 when it sold
#Only with subclass of "020", "030", "040", "045", "050", "060","070","075"
library(dplyr)
mydata <- mydata %>%
  mutate(HouseAge = YrSold - YearBuilt,Remodel = YearRemodel - YearBuilt)
sub1 <- filter(mydata, HouseAge < 70, YearBuilt < 1940 | YearBuilt > 1980 | Remodel > 0, 
               SubClass %in% c(020, 030, 040, 045, 
                                 050, 060, 070, 075) )
head(sub1)
#EDA
#Only use the numerical and continuous variable
col_n1 = c(
  "SubClass", "LotFrontage", "LotArea", "OverallQual", "OverallCond", 
  "YearBuilt", "YearRemodel", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
  "BsmtUnfSF", "TotalBsmtSF", "FirstFlrSF", "SecondFlrSF", "LowQualFinSF", 
  "GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", 
  "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", 
  "GarageYrBlt", "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", 
  "EnclosedPorch", "ThreeSsnPorch", "ScreenPorch", "PoolArea", "MiscVal", 
  "MoSold", "YrSold", "SalePrice"
)
##"TotalPorchSF"  "TotalFullBath" "TotalHalfBath"
#combine some column info
sub1 <- sub1 %>%
  mutate(TotalPorchSF = OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch,
         TotalFullBath = BsmtFullBath + FullBath,
         TotalHalfBath = BsmtHalfBath + HalfBath)
col_n = c(
  "SubClass", "LotFrontage", "LotArea", "OverallQual", "OverallCond", 
  "YearBuilt", "YearRemodel", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
  "BsmtUnfSF", "TotalBsmtSF", "FirstFlrSF", "SecondFlrSF", "LowQualFinSF", 
  "GrLivArea", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", 
  "GarageYrBlt", "GarageCars", "GarageArea", "WoodDeckSF", "PoolArea", 
 "MiscVal", "TotalPorchSF", "TotalFullBath", "TotalHalfBath", 
  "MoSold", "YrSold", "SalePrice"
)
#new set only takes interested variables
sub1_subset <- sub1[, col_n]
head(sub1_subset)
#continuous var
col_c = c("LotFrontage", "LotArea","OverallQual",
         "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
          "BsmtUnfSF", "TotalBsmtSF", "FirstFlrSF", "SecondFlrSF", "LowQualFinSF", 
          "GrLivArea", 
         "GarageArea", "WoodDeckSF", "PoolArea", 
          "MiscVal", "TotalPorchSF",  
          "SalePrice"
)
sub1_conVar <- sub1[,col_n]
#statistics summary
summary_statistics <- function(data) {
  stats <- sapply(data, function(x) {
    num_nulls <- sum(is.na(x))
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    median_val <- median(x, na.rm = TRUE)
    
    return(c(NumNulls = num_nulls, Mean = mean_val, SD = sd_val, Min = min_val, Max = max_val, Median = median_val))
  })
  
  return(as.data.frame(t(stats)))
}
summary_stats <- summary_statistics(sub1_conVar)
#outliers
## Function to identify outliers in a column
find_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- column[column < lower_bound | column > upper_bound]
  return(outliers)
}

outliers_list <- lapply(sub1_conVar, find_outliers)

## View outliers for each column
outliers_list

#histogram for each continuous var
create_histogram <- function(data, column_name) {
  ggplot(data, aes_string(x = column_name)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", column_name), 
         x = column_name, 
         y = "Count") +
    theme_minimal()
}
for (column_name in names(sub1_conVar)) {
  print(create_histogram(sub1_conVar, column_name))
}
#Correlation
# Load necessary libraries
library(ggplot2)
library(reshape2)
# Calculate the correlation matrix
cor_matrix <- cor(sub1_conVar, use = "complete.obs")

# Melt the correlation matrix for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap')

#scatterplots
column_names <- setdiff(names(sub1_conVar), "SalePrice")
create_scatterplot <- function(data, column_name) {
  ggplot(data, aes_string(x = column_name, y = "SalePrice")) +
    geom_point(color = "blue", size = 2) +
    ggtitle(paste("Scatterplot of", column_name, "vs SalePrice")) +
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = 0.5)) +
    geom_smooth(method = "lm", se = FALSE, color = "red")  # Red regression line
}

for (column_name in column_names) {
  print(create_scatterplot(sub1_conVar, column_name))
}

#subset2 for the modeling 
col_c2 = c( "LotArea","OverallQual", 
           "BsmtFinSF1", "BsmtFinSF2", 
          "BsmtUnfSF", "TotalBsmtSF", "FirstFlrSF", "SecondFlrSF", "LowQualFinSF", 
          "GrLivArea", 
          "GarageArea", "WoodDeckSF", "PoolArea", 
          "MiscVal", "TotalPorchSF",  
          "SalePrice"
)
#subset2<- sub1_conVar[,col_c2]
subset2<- sub1_conVar[,]
#Simple Linear Regression Modeling
model1 = lm(SalePrice ~ GrLivArea, data=subset2)
anova(model1)
summary(model1)
##MODEL1 Histogram of the standardized residuals
##MODEL1 Scatterplot of standardized residuals 
res1 <- resid(model1)
std_res1 <- (res1- mean(res1)) / sd(res1)

ggplot(data.frame(StandardizedResiduals = std_res1), aes(x = StandardizedResiduals)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme_minimal()

pred1 <- predict(model1)
plot_data <- data.frame(Prediction = pred1, StandardizedResiduals = std_res1)


ggplot(plot_data, aes(x = Prediction, y = StandardizedResiduals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Standardized Residuals vs. Fitted Values") +
  xlab("Fitted Values (Y_hat)") +
  ylab("Standardized Residuals (Y)") +
  theme_minimal()
##check several stat
library(influence.ME)
# Assume model1 is already created using lm()
# For example: model1 <- lm(SalePrice ~ GrLivArea, data = dataset)

# Calculate Cook's distance for each observation
cooks.distance <- cooks.distance(model1)

# Calculate leverage (hat) values
hatvalues <- hatvalues(model1)

# Calculate DFFITS
dffits <- dffits(model1)

# Plot Cook's distance
plot(cooks.distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
  abline(h = 4/(length(cooks.distance)-2), col = "red", lty = 2)

# Plot leverage values
plot(hatvalues, type = "h", main = "Leverage Values", ylab = "Hat Values")
abline(h = 2*mean(hatvalues), col = "red", lty = 2)

# Plot DFFITS values
plot(dffits, type = "h", main = "DFFITS", ylab = "DFFITS")
abline(h = c(-1, 1) * 2*sqrt(2/length(dffits)), col = "red", lty = 2)

#Model2
model2 = lm(SalePrice ~ GrLivArea + OverallQual, data = subset2)
anova(model2)
summary(model2)

#residual
res2 <- resid(model2)
std_res2 <- (res2- mean(res2)) / sd(res2)

ggplot(data.frame(StandardizedResiduals = std_res2), aes(x = StandardizedResiduals)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme_minimal()

pred2 <- predict(model2)
plot_data <- data.frame(Prediction = pred2, StandardizedResiduals = std_res2)


ggplot(plot_data, aes(x = Prediction, y = StandardizedResiduals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Standardized Residuals vs. Predict Values") +
  xlab("Predict Values (Y_hat)") +
  ylab("Standardized Residuals (Y)") +
  theme_minimal()
#stat
# Calculate Cook's distance for each observation
cooks.distance <- cooks.distance(model2)

# Calculate leverage (hat) values
hatvalues <- hatvalues(model2)

# Calculate DFFITS
dffits <- dffits(model2)

# Plot Cook's distance
plot(cooks.distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/(length(cooks.distance)-2), col = "red", lty = 2)

# Plot leverage values
plot(hatvalues, type = "h", main = "Leverage Values", ylab = "Hat Values")
abline(h = 2*mean(hatvalues), col = "red", lty = 2)

# Plot DFFITS values
plot(dffits, type = "h", main = "DFFITS", ylab = "DFFITS")
abline(h = c(-1, 1) * 2*sqrt(2/length(dffits)), col = "red", lty = 2)

#model 3
model3 <- lm(SalePrice ~ GrLivArea + OverallQual + GarageArea, data = subset2)
anova(model3)
summary(model3)
#residual model 3
res3 <- resid(model3)
std_res3 <- (res3- mean(res3)) / sd(res3)

ggplot(data.frame(StandardizedResiduals = std_res3), aes(x = StandardizedResiduals)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme_minimal()

pred3 <- predict(model3)
plot_data <- data.frame(Prediction = pred3, StandardizedResiduals = std_res3)


ggplot(plot_data, aes(x = Prediction, y = StandardizedResiduals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Standardized Residuals vs. Predict Values") +
  xlab("Predict Values (Y_hat)") +
  ylab("Standardized Residuals (Y)") +
  theme_minimal()

cooks.distance <- cooks.distance(model3)

# Calculate leverage (hat) values
hatvalues <- hatvalues(model3)

# Calculate DFFITS
dffits <- dffits(model3)

# Plot Cook's distance
plot(cooks.distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/(length(cooks.distance)-2), col = "red", lty = 2)

# Plot leverage values
plot(hatvalues, type = "h", main = "Leverage Values", ylab = "Hat Values")
abline(h = 2*mean(hatvalues), col = "red", lty = 2)

# Plot DFFITS values
plot(dffits, type = "h", main = "DFFITS", ylab = "DFFITS")
abline(h = c(-1, 1) * 2*sqrt(2/length(dffits)), col = "red", lty = 2)

#Model 4
model4 <- lm(log(SalePrice) ~GrLivArea + OverallQual + GarageArea, data = subset2 )
anova(model4)
summary(model4)
par(mfrow = c(2, 2))
plot(model3)
plot(model4)

#Remove influential points from model3
cooksd <- cooks.distance(model3)
influential_points <- which(cooksd > (4/(nrow(subset2)-length(coef(model3))-2)))
num_influential_points <- length(influential_points)
subset3 <-subset2[-influential_points, ]

str(subset3)

model5 <- lm(log(SalePrice) ~ GrLivArea + OverallQual + GarageArea, data = subset3)
anova(model5)
summary(model5)
res5 <- resid(model5)
std_res5 <- (res5- mean(res5)) / sd(res5)

ggplot(data.frame(StandardizedResiduals = std_res5), aes(x = StandardizedResiduals)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme_minimal()

pred5 <- predict(model5)
plot_data <- data.frame(Prediction = pred5, StandardizedResiduals = std_res5)


ggplot(plot_data, aes(x = Prediction, y = StandardizedResiduals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Standardized Residuals vs. Predict Values") +
  xlab("Predict Values (Y_hat)") +
  ylab("Standardized Residuals (Y)") +
  theme_minimal()

cooks.distance <- cooks.distance(model5)

# Calculate leverage (hat) values
hatvalues <- hatvalues(model5)

# Calculate DFFITS
dffits <- dffits(model5)

# Plot Cook's distance
plot(cooks.distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/(length(cooks.distance)-2), col = "red", lty = 2)

# Plot leverage values
plot(hatvalues, type = "h", main = "Leverage Values", ylab = "Hat Values")
abline(h = 2*mean(hatvalues), col = "red", lty = 2)

# Plot DFFITS values
plot(dffits, type = "h", main = "DFFITS", ylab = "DFFITS")
abline(h = c(-1, 1) * 2*sqrt(2/length(dffits)), col = "red", lty = 2)

#removed set

removedset <-subset2[influential_points, ]

#question 6


model6 <- lm(SalePrice ~ GrLivArea + OverallQual + GarageArea + TotalBsmtSF + 
               MasVnrArea + TotalFullBath+ BsmtFinSF1 + Fireplaces
             + YearRemodel , data = subset3)
anova(model6)
summary(model6)
res6 <- resid(model6)
std_res6 <- (res5- mean(res6)) / sd(res6)



ggplot(data.frame(StandardizedResiduals = std_res6), aes(x = StandardizedResiduals)) +
  geom_histogram(fill = "blue", color = "black") +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme_minimal()

pred6 <- predict(model6)
plot_data <- data.frame(Prediction = pred6, StandardizedResiduals = std_res6)


ggplot(plot_data, aes(x = Prediction, y = StandardizedResiduals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Standardized Residuals vs. Predict Values") +
  xlab("Predict Values (Y_hat)") +
  ylab("Standardized Residuals (Y)") +
  theme_minimal()
