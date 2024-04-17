#assignment7
mydata<- data.frame(icu)
summary(mydata)
library(ggplot2)

#2
con_table <- table(mydata$SEX, mydata$STA)
print(con_table)

contable2 <- table(mydata$TYP, mydata$STA)
print(contable2)
library(lessR)
Plot(AGE, STA, data=mydata)

# Create the AGE_CAT column in the mydata dataframe
mydata$AGE_CAT <- ifelse(mydata$AGE >= 15 & mydata$AGE < 25, 1,
                         ifelse(mydata$AGE >= 25 & mydata$AGE < 35, 2,
                                ifelse(mydata$AGE >= 35 & mydata$AGE < 45, 3,
                                       ifelse(mydata$AGE >= 45 & mydata$AGE < 55, 4,
                                              ifelse(mydata$AGE >= 55 & mydata$AGE < 65, 5,
                                                     ifelse(mydata$AGE >= 65 & mydata$AGE < 75, 6,
                                                            ifelse(mydata$AGE >= 75 & mydata$AGE < 85, 7,
                                                                   ifelse(mydata$AGE >= 85 & mydata$AGE < 95, 8,
                                                                          ifelse(mydata$AGE >= 95, 9, NA)))))))))

# Print the first few rows to verify the new column
head(mydata)

# Assume the data frame is named mydata and the AGE_CAT variable has been created as per previous instructions

# Calculate the mean of STA for each AGE_CAT
sta_means_by_age_cat <- aggregate(mydata$STA, by=list(mydata$AGE_CAT), FUN=mean)
names(sta_means_by_age_cat) <- c("AGE_CAT", "STA_Mean")

# Plot the means versus the AGE_CAT categorical variable
library(ggplot2)
ggplot(sta_means_by_age_cat, aes(x=AGE_CAT, y=STA_Mean)) +
  geom_bar(stat="identity", width=.5) +
  labs(x="Age Category", y="STA mean", title="STA mean by Age Interval") +
  theme_minimal()


#modeling
# Assuming the dataframe is named mydata and contains 'STA' as the response variable and 'AGE' as the predictor

# Load the necessary package for logistic regression
library(stats)

# Fit the logistic regression model
log1 <- glm(STA ~ AGE, data=mydata, family=binomial)
    
# Report the model's coefficients
anova(log1)
summary(log1)
print(BIC(log1))

#pred
mydata$pred1 <- predict(log1, type = "link")
Plot(pred1, STA, data=mydata)
ggplot(mydata, aes(x = AGE, y = pred1)) +
  geom_point(alpha = 0.6) +  # Use semi-transparent points to handle overplotting
  labs(x = "Age", y = "Predicted Logits", title = "Scatterplot of Predicted Logits by Age") +
  theme_minimal()
mydata$prob1 <- plogis(mydata$pred1)
plot(mydata$AGE, mydata$prob1, xlab = "Age", ylab = "Predicted Probability of Survival", 
     main = "Predicted Probabilities and Raw Data by Age", pch = 20, col = "blue", ylim = c(0, 1))
points(mydata$AGE, mydata$STA, pch = 4, col = "red")

mydata$fit_prob <- exp(mydata$pred1)/(1+exp(mydata$pred1))
library(ggplot2)
ggplot(mydata, aes(x=AGE, y=prob1)) + 
  geom_point() +
  geom_line(aes(x=AGE, y=fit_prob))

ggplot(mydata, aes(x = AGE, y = prob1)) +
  geom_point(alpha = 0.6, color = "blue") +  # Plot the predicted probabilities
  geom_jitter(aes(y = STA), alpha = 0.6, color = "red") +  # Overlay the raw data
  labs(x = "Age", y = "Predicted Probability of Survival", title = "Predicted Probabilities and Raw Data by Age") +
  theme_minimal()
#age of 28

log_28 <- -2.287
probb <- exp(log_28)/(1+exp(log_28))
print(probb)

ggplot(mydata) +
  geom_bar( aes(STA) ) +
  ggtitle("Status Variable") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(mydata$STA)
ggplot(mydata, aes(x=AGE)) + 
  geom_histogram(color="black", binwidth= 5) +
  labs(title="Age") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata) +
  geom_bar( aes(SEX) ) +
  ggtitle("Sex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(mydata$SEX)
ggplot(mydata) +
  geom_bar( aes(RACE) ) +
  ggtitle("Race") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
table(mydata$RACE)
ggplot(mydata) +
  geom_bar( aes(SER) ) +
  ggtitle("Service at ICU") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(CAN) ) +
  ggtitle("Canver Part of Problem") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata) +
  geom_bar( aes(CRN) ) +
  ggtitle("History of Chronic Renal Failure") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(INF) ) +
  ggtitle("Infection Probable") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(CPR) ) +
  ggtitle("CPR Prior to ICU") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata, aes(x=SYS)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of Systolic Blood Pressure") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata, aes(x=HRA)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of Heart Rate") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(PRE) ) +
  ggtitle("Previous Admission") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(TYP) ) +
  ggtitle("Type of Admission") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(FRA) ) +
  ggtitle("Bone Fracture") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(PO2) ) +
  ggtitle("PO2 from Initial") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(PH) ) +
  ggtitle("PH from Initial") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(PCO) ) +
  ggtitle("PCO2 from Initial") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(BIC) ) +
  ggtitle("BIC from Initial") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(CRE) ) +
  ggtitle("Creatinine from Initial") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(mydata) +
  geom_bar( aes(LOC) ) +
  ggtitle("Level of Consciousness") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata, aes(x = factor(STA), y = SYS, fill = factor(STA))) +
  geom_boxplot() +
  labs(x = "STA", y = "SYS", title = "Boxplot of SYS by STA") +
  scale_fill_manual(values = c("blue", "green"), name = "STA Group", labels = c("0", "1")) +
  theme_minimal()

ggplot(mydata, aes(x = factor(STA), y = AGE, fill = factor(STA))) +
  geom_boxplot() +
  labs(x = "STA", y = "AGE", title = "Boxplot of age by STA") +
  scale_fill_manual(values = c("blue", "green"), name = "STA Group", labels = c("0", "1")) +
  theme_minimal()

ggplot(mydata, aes(x=AGE, y=STA)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Age vs STA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata, aes(x=STA, y=SYS)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

