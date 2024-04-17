#assignment6
#assignment4 data preparation

mydata<- data.frame(ames_housing_data)
library(dplyr)
library(ggplot2)
library(caret)    # For data partitioning
library(MASS)     # For stepAIC
library(fastDummies)
mydata <- mydata %>%
  mutate(HouseAge = YrSold - YearBuilt,Remodel = YearRemodel - YearBuilt)
Q1 <- quantile(mydata$SalePrice, 0.25)
Q3 <- quantile(mydata$SalePrice, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out the outliers
mydata <- mydata %>%
  filter(SalePrice >= lower_bound, SalePrice <= upper_bound)

# View the dimensions before and after outlier removal
#dim(mydata)
#dim(mydata_clean)
sub1 <- filter(mydata, HouseAge < 70, YearBuilt > 1945 | Remodel > 0, 
               SubClass %in% c(020, 030, 040, 045, 
                               050, 060, 070, 075, 080, 085, 090, 120,
                               150, 160, 180),
               !BldgType %in% c('Duplx', '2FmCon'))

sub1$logSalePrice <- log(sub1$SalePrice)
head(sub1)
sub1 <- sub1 %>%
  mutate(TotalPorchSF = OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch,
         TotalFullBath = BsmtFullBath + FullBath,
         TotalHalfBath = BsmtHalfBath + HalfBath)

sub1$QualityIndex <- sub1$OverallQual*sub1$OverallCond
sub1$TotalSqftCalc <- sub1$BsmtFinSF1+sub1$BsmtFinSF2+sub1$GrLivArea

col_n = c(
  "LotArea", "QualityIndex", 
  "YearRemodel", "MasVnrArea" ,"YearRemodel",
  "TotalBsmtSF", "TotalSqftCalc" ,
  "GrLivArea",  "TotRmsAbvGrd", "Fireplaces", 
   "GarageArea","BsmtFullBath",
  "MiscVal", "TotalPorchSF", "TotalFullBath", "TotalHalfBath", 
  "logSalePrice",
  "LandContour", "LotShape", "Exterior1","ExterQual", "KitchenQual","BsmtQual"
)

sub2 <- sub1[, col_n]
#sub2 <- na.omit(sub2)

summary(sub2)

  #
#categorical variables
col_c = c( "LandContour", "LotShape", "Exterior1", "Exterior2","ExterQual", "KitchenQual","BsmtQual")
subset1 <- dummy_cols(sub2, select_columns = col_c, remove_selected_columns = TRUE)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(sub1$logSalePrice, p = .7, list = FALSE)
train.df <- sub1[trainIndex, ]
test.df <- sub1[-trainIndex, ]
train <- subset1[trainIndex, ]
test <- subset1[-trainIndex, ]

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(subset1)[1]
dim(train)[1]
dim(test)[1]
dim(train)[1]+dim(test)[1]

train1 <- na.omit(train)
test1 <- na.omit(test)
upperModel <- lm(logSalePrice ~ ., data = train1)
lowerModel <- lm(logSalePrice ~ 1, data = train1)
junk.lm <- lm(logSalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)

# Forward selection
forward.lm <- stepAIC(lowerModel, direction = "forward", scope = list(lower = formula(lowerModel), upper = formula(upperModel)))

# Backward elimination
backward.lm <- stepAIC(upperModel, direction = "backward")

# Stepwise selection
stepwise.lm <- stepAIC(lowerModel, direction = "both", scope = list(lower = formula(lowerModel), upper = formula(upperModel)))


# Display summaries of the models
summary(forward.lm)
summary(backward.lm)
summary(stepwise.lm)

library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

forward.test <- predict(forward.lm, newdata=test1)
forward.train <- predict(forward.lm, newdata=train1)

backward.test <- predict(backward.lm, newdata=test1)
backward.train <- predict(backward.lm, newdata=train1)

stepwise.test <- predict(stepwise.lm, newdata=test1)
stepwise.train<- predict(stepwise.lm, newdata=train1)

junk.test <- predict(junk.lm, newdata=test.df)
junk.train <- predict(junk.lm, newdata=train.df)
# Function to calculate MSE
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Function to calculate MAE
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Compute MSE and MAE for forward selection model
mse_forward <- mse(test1$logSalePrice, forward.test)
mae_forward <- mae(test1$logSalePrice, forward.test)
mse_forward_in <- mse(train1$logSalePrice, forward.train)
mae_forward_in <- mae(train1$logSalePrice, forward.train)

# Repeat calculations for other models
mse_backward <- mse(test1$logSalePrice, backward.test)
mae_backward <- mae(test1$logSalePrice, backward.test)
mse_backward_in <- mse(train1$logSalePrice, backward.train)
mae_backward_in <- mae(train1$logSalePrice, backward.train)


mse_stepwise <- mse(test1$logSalePrice, stepwise.test)
mae_stepwise <- mae(test1$logSalePrice, stepwise.test)
mse_stepwise_in <- mse(train1$logSalePrice, stepwise.train)
mae_stepwise_in <- mae(train1$logSalePrice, stepwise.train)


mse_junk <- mse(test.df$logSalePrice, junk.test)
mae_junk <- mae(test.df$logSalePrice, junk.test)
mse_junk_in <- mse(train.df$logSalePrice, junk.train)
mae_junk_in <- mae(train.df$logSalePrice, junk.train)

# Print the results
cat("out of sample: MSE (Forward):", mse_forward, "\nMAE (Forward):", mae_forward, "\n")
cat("in sample: MSE (Forward):", mse_forward_in, "\nMAE (Forward):", mae_forward_in, "\n")
cat("out of sampleMSE (Backward):", mse_backward, "\nMAE (Backward):", mae_backward, "\n")
cat("in sample: MSE (backward):", mse_backward_in, "\nMAE (backward:", mae_backward_in, "\n")
cat("out of sample: MSE (Stepwise):", mse_stepwise, "\nMAE (Stepwise):", mae_stepwise, "\n")
cat("in sample: MSE (Stepwise):", mse_stepwise_in, "\nMAE (Stepwise):", mae_stepwise_in, "\n")
cat("out of sample: MSE (Junk):", mse_junk, "\nMAE (Junk):", mae_junk, "\n")
cat("in smaple: MSE (Junk):", mse_junk_in, "\nMAE (Junk):", mae_junk_in, "\n")


#Validation
forward.pct <- abs(forward.lm$residuals)/train1$logSalePrice;

# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)


# Test Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train1$logSalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train1$logSalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train1$logSalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.df$logSalePrice;
MAPE <- mean(junk.pct)
MAPE
# Assign Prediction Grades;
# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)

backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

backward.trainTable <- table(backward.PredictionGrade)
backward.trainTable/sum(backward.trainTable)

stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)

junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

junk.trainTable <- table(junk.PredictionGrade)
junk.trainTable/sum(junk.trainTable)

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test1$logSalePrice-forward.test)/test1$logSalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test1$logSalePrice-backward.test)/test1$logSalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test1$logSalePrice-stepwise.test)/test1$logSalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$logSalePrice-junk.test)/test.df$logSalePrice;
MAPE <- mean(junk.testPCT)
MAPE

forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)

stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)

junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)

vif(backward.lm)
par(mfrow=c(2,2))
plot(stepwise.lm)

model6 <- lm(formula = logSalePrice ~ GrLivArea + YearRemodel + TotalBsmtSF + 
                QualityIndex + TotalFullBath + ExterQual_Gd + Fireplaces + 
                GarageArea + ExterQual_Fa + MiscVal + BsmtQual_TA + Exterior1_AsbShng + 
                BsmtQual_NA + BsmtQual_Ex + LotArea + Exterior1_BrkFace + 
                LotShape_Reg + Exterior1_CemntBd + KitchenQual_Fa + TotalPorchSF + 
                Exterior1_BrkComm + KitchenQual_Ex + BsmtQual_Fa + TotalSqftCalc + 
                Exterior1_VinylSd + KitchenQual_TA + LandContour_HLS + LandContour_Lvl + 
                Exterior1_Plywood, data = subset1)

anova(model6)
summary(model6)
par(mfrow=c(2,2))
plot(model6)