#assignment5
mydata<- data.frame(NutritionStudy)
str(mydata)
head(mydata)
names(mydata)
#q1
ANOVA(Cholesterol ~ PriorSmoke,data=mydata)
fit1 <- lm(Cholesterol ~ PriorSmoke, data = mydata)
plot(x = mydata$PriorSmoke, y = mydata$Cholesterol)
summary(fit1)
anova(fit1)
plot(fit1)

#q2
d_PS1 <-ifelse(mydata$PriorSmoke==1,1,0)
d_PS2 <-ifelse(mydata$PriorSmoke==2,1,0)
d_PS3<-ifelse(mydata$PriorSmoke==3,1,0)
mydata <-cbind.data.frame(mydata,d_PS1, d_PS2, d_PS3)
model1 <- lm(Cholesterol ~ d_PS1+ d_PS2, data = mydata)
model1
summary(model1)
plot(model1)
anova(model1)
#q3
model2 <- lm(Cholesterol ~ Fat + d_PS1 + d_PS2, data = mydata)
summary(model2)
plot(model2)
anova(model2)

cooks.distance <- cooks.distance(model2)
influential_points <- cooks_d > 1
# Calculate leverage (hat) values
hat_values <- hatvalues(model2)
average_hat_value <- mean(hat_values)
high_leverage_points <- hat_values > 2 * average_hat_value
# Calculate DFFITS
dffits <- dffits(model2)
# For standardized residuals
std_residuals <- rstandard(model2)

# For studentized residuals (more commonly used)
studentized_residuals <- rstudent(model2)

outliers <- abs(studentized_residuals) > 3

# Create a data frame to hold the diagnostic measures
diagnostic_data <- data.frame(hat_values, cooks.distance, studentized_residuals)

# Add logical vectors identifying whether each condition is met
diagnostic_data$high_leverage <- diagnostic_data$hat_values > 2 * average_hat_value
diagnostic_data$influential <- diagnostic_data$cooks.distance > 1
diagnostic_data$outliers <- abs(diagnostic_data$studentized_residuals) > 3

# Find the row numbers of points that are high leverage, influential, or outliers
which(diagnostic_data$high_leverage)
which(diagnostic_data$influential)
which(diagnostic_data$outliers)

# If you want to identify observations that meet any of the conditions
which(diagnostic_data$high_leverage | diagnostic_data$influential | diagnostic_data$outliers)

# Plot Cook's distance
plot(cooks.distance, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/(length(cooks.distance)-2), col = "red", lty = 2)

# Plot leverage values
plot(hatvalues, type = "h", main = "Leverage Values", ylab = "Hat Values")
abline(h = 2*mean(hatvalues), col = "red", lty = 2)

# Plot DFFITS values
plot(dffits, type = "h", main = "DFFITS", ylab = "DFFITS")
abline(h = c(-1, 1) * 2*sqrt(2/length(dffits)), col = "red", lty = 2)



#q4
mydata$pred_m2 <- predict(model2, mydata)
mydata$PRIORSMOKE <- factor(mydata$PriorSmoke, levels = c(1, 2, 3))

library(ggplot2)
ggplot(mydata, aes(x=Fat, y=Cholesterol, color=PRIORSMOKE)) +
  geom_point() +
  labs(title="Cholesterol Levels By Fat",
       x="Fat",
       y="Cholesterol") +
  scale_color_manual(values = c("blue", "green", "red"),
                     name = "PRIORSMOKE Group",
                     breaks = c(1, 2, 3),
                     labels = c("Group 1", "Group 2", "Group 3")) +
  theme_minimal()
#q5-interaction Fat x PriorSmoke
mydata$FatPS1 <- mydata$Fat*d_PS1
mydata$FatPS2 <- mydata$Fat*d_PS2
mydata$FatPS3 <- mydata$Fat*d_PS3
model3 <- lm(Cholesterol ~ Fat + d_PS1 + d_PS2 + FatPS1 + FatPS2, data = mydata)
summary(model3)
plot(model3)
anova(model3)

cooks.distance <- cooks.distance(model3)
influential_points <- cooks_d > 1
# Calculate leverage (hat) values
hat_values <- hatvalues(model3)
average_hat_value <- mean(hat_values)
high_leverage_points <- hat_values > 2 * average_hat_value
# Calculate DFFITS
dffits <- dffits(model3)
# For standardized residuals
std_residuals <- rstandard(model3)

# For studentized residuals (more commonly used)
studentized_residuals <- rstudent(model3)

outliers <- abs(studentized_residuals) > 3

# Create a data frame to hold the diagnostic measures
diagnostic_data <- data.frame(hat_values, cooks.distance, studentized_residuals)

# Add logical vectors identifying whether each condition is met
diagnostic_data$high_leverage <- diagnostic_data$hat_values > 2 * average_hat_value
diagnostic_data$influential <- diagnostic_data$cooks.distance > 1
diagnostic_data$outliers <- abs(diagnostic_data$studentized_residuals) > 3

# Find the row numbers of points that are high leverage, influential, or outliers
which(diagnostic_data$high_leverage)
which(diagnostic_data$influential)
which(diagnostic_data$outliers)

# If you want to identify observations that meet any of the conditions
which(diagnostic_data$high_leverage | diagnostic_data$influential | diagnostic_data$outliers)

#q6model3 prediction
mydata$pred_m3 <- predict(model3, mydata)

library(ggplot2)
ggplot(mydata, aes(x=Fat, y=pred_m3, color = PRIORSMOKE)) +
  geom_point() +
  labs(title="Cholesterol Levels By Fat",
       x="Fat",
       y="Cholesterol") +
  scale_color_manual(values = c("blue", "green", "red"),
                     name = "PRIORSMOKE Group",
                     breaks = c(1, 2, 3),
                     labels = c("Group 1", "Group 2", "Group 3")) +
  theme_minimal()

#q8
data1 <- mydata[-c(94, 112, 188, 257), ]
#Smoke
m1 <- lm(Cholesterol ~ Smoke + Fat, data = data1)
summary(m1)
anova(m1)
plot(m1)
#dummy 
data1$d_SN <- ifelse(data1$Smoke == "No", 1, 0)
data1$d_SY <- ifelse(data1$Gender == "Yes", 1, 0)
m2 <- lm(Cholesterol ~ Fat + d_SN, data = data1)
summary(m2)
anova(m2)

data1$FatSY <- data1$Fat*data1$d_SN
data1$FatSN <- data1$Fat*data1$d_SY

m3 <- lm(Cholesterol ~ Fat + d_SN + FatSN, data = data1)
summary(m3)
anova(m3)
