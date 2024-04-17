#assignment2#
#eda
library(readxl)
file_path <- "USStates.xlsx"
mydata <- read_excel(file_path)
head(mydata)
str(mydata)
names(mydata)
#assignment2 questions3--
#q3 demographic variables subset
sub1<- subset(mydata, select=c("State", "Region", "Population", 
                               "HouseholdIncome"))
sub2<- subset(mydata, select=c("HighSchool", "College", "Smokers", 
                               "PhysicalActivity", 
                               "Obese", "NonWhite", "HeavyDrinkers", 
                               "TwoParents", "Insured", "HouseholdIncome"))
library(psych)
describe(mydata) 

#q3 create non demo variables against income
# Load necessary library
library(ggplot2)

# Assuming 'data' is your data frame containing the variables
# List of non-demographic explanatory variables (Xs)
x_ndemo <- c("HighSchool", "College", "Smokers", "PhysicalActivity", 
            "Obese", "NonWhite", "HeavyDrinkers", "TwoParents", "Insured")

# Creating scatterplots for each X variable against HouseholdIncome
for (x_var in x_vars) {
  plot <- ggplot(mydata, aes_string(x = x_var, y = "HouseholdIncome")) +
    geom_point() +  # Add points
    geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
    ggtitle(paste("Scatterplot of", x_var, "vs HouseholdIncome")) +
    xlab(x_var) +
    ylab("HouseholdIncome")
  
  print(plot)  # Display the plot
}

#question4

# Extracting only the required columns from the data frame
subset_data <- mydata[c("HouseholdIncome", x_ndemo)]

# Calculating the correlation matrix
correlation_matrix <- cor(subset_data, use = "complete.obs", method = "pearson")

# Extracting only the correlations with HouseholdIncome
correlations_with_Y <- correlation_matrix["HouseholdIncome", -1]  # Exclude self-correlation

# Convert to a data frame for better readability
correlation_table <- data.frame(Variable = names(correlations_with_Y), 
                                Correlation_with_HouseholdIncome = correlations_with_Y)

# Print the table
print(correlation_table)

#question5-Model1
model1 <- lm(HouseholdIncome ~ College, data = mydata)
summary(model1)
#question6 
anova(model1)

#question7
#predict y hat
y_hat <- predict(model1)

#create residuals
res1 <- mydata$HouseholdIncome - y_hat
#Sum of squared residuals
SSR1 <- sum(res1^2)
#mean of Y
y_bar <- mean(mydata$HouseholdIncome)
#Sum of squares total
SST1 <- sum((mydata$HouseholdIncome - y_bar)^2)
#Sum of squares due to regression
SSRe1 <- sum((y_hat - y_bar)^2)
#Calculate a statistic that is:
#(Sum of Squares due to Regression) / (Sum of squares Total)
Rsqrt <- SSRe1/SST1

#print out
cat("Sum of Squared Residuals (SSR):", SSR1, "\n")
cat("Sum of Squares Total (SST):", SST1, "\n")
cat("Sum of Squares due to Regression (SSR):", SSRe1, "\n")
cat("R-squared Statistic:", Rsqrt, "\n")

#question8
#using residuals res1
res1_mean <- mean(res1)
#standardized residuals
res1_sd <-sd(res1)
stand_res1 <- (res1 - res1_mean)/res1_sd

#plot
hist(stand_res1)
plot(y_hat, stand_res1)

#question 9
#using variable 
model2 <- lm(HouseholdIncome ~ Smokers, data = mydata)
summary(model2)
anova(model2)
#residual analysis
y_hat2 <- predict(model2)

#create residuals
res2 <- mydata$HouseholdIncome - y_hat2

#using residuals res2
res2_mean <- mean(res2)
#standardized residuals
res2_sd <-sd(res2)
stand_res2 <- (res2 - res2_mean)/res2_sd
#plot
hist(stand_res2)
plot(y_hat, stand_res2)

#question10
model3 <- lm(Insured ~ College, data = mydata)
summary(model3)
anova(model3)
#residual analysis
y_hat3 <- predict(model3)

#create residuals
res3 <- mydata$Insured - y_hat3

#using residuals res2
res3_mean <- mean(res3)
#standardized residuals
res3_sd <-sd(res3)
stand_res3 <- (res3 - res3_mean)/res3_sd
#plot
hist(stand_res3)
plot(y_hat, stand_res3)

