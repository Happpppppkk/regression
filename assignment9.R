#assignment9
mydata <- data.frame(STRESS)
head(mydata)
summary(mydata)
library(readxl)
library(ggplot2)
#library(qqplotr) # For enhanced Q-Q plots
#q1
summary(mydata$STRESS)
variance <- var(mydata$STRESS)
print(variance)
ggplot(mydata, aes(x=as.factor(STRESS))) + 
  geom_bar(fill="grey", color="black") +
  scale_x_discrete(limits=as.character(0:9)) +
  labs(title="Histogram of STRESS", x="STRESS", y="Frequency") +
  theme_minimal()

observed_quantiles <- sort(mydata$STRESS)
n <- length(observed_quantiles)
pp <- (1:n) / (n + 1) # Calculating percentiles
theoretical_quantiles <- qnorm(pp, mean = mean(observed_quantiles, 
                                               na.rm = TRUE), 
                               sd = sd(observed_quantiles, na.rm = TRUE))

# Combine into a dataframe for ggplot
qq_data <- data.frame(Theoretical = theoretical_quantiles, 
                      Observed = observed_quantiles)

# Create the Q-Q plot
ggplot(qq_data, aes(x = Theoretical, y = Observed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + # Set x-axis breaks
  labs(title = "Q-Q Plot for STRESS", x = "Theoretical Quantiles", 
       y = "STRESS data Quantiles") +
  theme_minimal()

#q2
lmmodel <- lm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = mydata)
summary(lmmodel)
anova(lmmodel)
par(mfrow=c(2,2))
plot(lmmodel)
#prediction
mydata$pred1<- predict(lmmodel, data = mydata)
summary(mydata$pred1)
max_pred <- max(pred1)
bin_width <- 1
breaks <- seq(from = 0)
hist(mydata$pred1, main = "Histogram of Predicted Values", 
     xlab = "Predicted Values", col = "blue")

ggplot(mydata, aes(x = pred1)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Histogram of Predicted Values", x = "Predicted Values", y = "Frequency") +
  theme_minimal() +
  xlim(0, max(mydata$pred1)) # Set thed x-axis limits
#q3
mydata$LN_STRESS <- log(mydata$STRESS + 1)
lmmodel2 <-lm(LN_STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = mydata)
summary(lmmodel2)
anova(lmmodel2)
par(mfrow=c(2,2))
plot(lmmodel2)
mydata$pred2 <- predict(lmmodel2, data = mydata)
hist(mydata$pred2, main = "Histogram of Predicted Values", 
     xlab = "Predicted Values", col = "blue")
#q4 a
pmodel1 <- glm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, 
                     data = mydata, family = "poisson")
summary(pmodel1)
print(BIC(pmodel1))
par(mfrow = c(2,2))
plot(pmodel1)
anova(pmodel1)
anova(lmmodel2)
AIC(lmmodel)
AIC(lmmodel2)
AIC(pmodel1)
#q4 b
library(MASS)
nbmodel <- glm.nb(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, 
                  data = mydata)
summary(nbmodel)
print(BIC(nbmodel))
par(mfrow = c(2,2))
plot(nbmodel)
AIC(nbmodel)
#q5
newd <- data.frame()
cohes_mean <- mean(mydata$COHES, na.rm = TRUE)
print(cohes_mean)
cohes_sd <- sd(mydata$COHES)
print(cohes_sd)
summary(mydata$cohe_group)
print(table(mydata$STRESS, mydata$cohe_group))

mydata$cohe_group <- cut(mydata$COHES,
                         breaks = c(-Inf, cohes_mean - cohes_sd, 
                                    cohes_mean + cohes_sd, Inf),
                         labels = c("low", "middle", "high"))
mydata$pred_counts1 <- predict(pmodel1, data = mydata,type = "response")
mydata$pred_counts1 <- fitted(pmodel1)
Histogram(pred_counts1=NULL, data=mydata, bin.start=0, bin.width=1)
#print(table(mydata$pred_counts1, mydata$cohe_group))
breaks <- c(-Inf, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)
labels <- c(0, 1, 2, 3, 4, 5)
mydata$round_predcount1<- round(mydata$pred_counts1)
print(table(mydata$round_predcount1))
print(table(mydata$round_predcount1, mydata$cohe_group))
# Use cut to create a factor variable with levels based on the intervals
#mydata$pmodelpred <- as.integer(cut(mydata$pred_counts1,
                                      #breaks = breaks, labels = labels, right = TRUE))
#Histogram(pmodelpred=NULL, data=mydata, bin.start=0, bin.width=1)
# Check the first few rows of the dataframe to verify the new assigned values

#q7
rsid <- residuals(pmodel1, type = "deviance")
d <- deviance(pmodel1)
ggplot(mydata, aes(x = mydata$pred_counts1, y = rsid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted Values", y = "Deviance Residuals",
       title = "Deviance Residuals vs Predicted Values")

#q8
mydata$Y_IND <- ifelse(mydata$STRESS == 0,0,1)
logmodel <- glm(Y_IND ~COHES + ESTEEM + GRADES + SATTACH, 
                data = mydata, family = "binomial" )
summary(logmodel)
anova(logmodel)
mydata$predlog <- predict(logmodel, type = "response")
logLik(logmodel)
#q9
zipfit1 <- glm(Y_IND ~COHES + ESTEEM + GRADES + SATTACH, 
               data = mydata, family = "binomial" )
zipfit1
anova(zipfit1)
summary(zipfit1)

y_hatzip1<- 3.5167 - 0.0207*mydata$COHES - 0.0189*mydata$ESTEEM -
  0.0255*mydata$GRADES - 0.0277*mydata$SATTACH

zipodds <- exp(y_hatzip1)
prob_hatzip1<- zipodds / (1+zipodds)
Histogram(prob_hatzip1)

mydata$STRESS_Y <- mydata$STRESS
mydata$STRESS_Y[mydata$STRESS_Y ==0] <- NA



pmodel3 <- glm(STRESS_Y ~ COHES + ESTEEM + GRADES + SATTACH, 
               data = mydata, family = "poisson" )
summary(pmodel3)
y_hatzip2 <- 2.3117 - 0.0063*mydata$COHES - 0.0195*mydata$ESTEEM -
  0.0147*mydata$GRADES - 0.008*mydata$SATTACH
pred_hatzip2<- exp(y_hatzip2)

mydata$y_hat6<-ifelse(prob_hatzip1<0.50, 0, pred_hatzip2)
y_hat6
Histogram(y_hat6)

mydata$reds <- mydata$STRESS - mydata$y_hat6
ggplot(mydata, aes(x = mydata$y_hat6, y = mydata$reds)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted Values", y = "Deviance Residuals",
       title = "Deviance Residuals vs Predicted Values")
#10
install.packages("pscl")
library(pscl)
data <- data.frame(STRESS)
zip_model_same <- zeroinfl(STRESS ~ COHES + 
                             ESTEEM + GRADES + SATTACH | 
                             COHES + ESTEEM + GRADES + SATTACH, 
                           data = data, dist = "poisson")
summary(zip_model_same)
data$y1 <- fitted(zip_model_same)
Histogram(y1=NULL, data=data, bin.start=0, bin.width=1)

# Goodness of fit measures can include AIC and BIC from the model summary
AIC(zip_model_same)
BIC(zip_model_same)
rzip_model_same <- residuals(zip_model_same)

m4 <- zeroinfl(STRESS ~ COHES +SATTACH +ESTEEM| 
                 SATTACH +COHES, 
                           data = data, dist = "poisson")
summary(m4)
data$y2 <- fitted(m4)
Histogram(y2=NULL, data=data, bin.start=0, bin.width=1)
AIC(m4)
BIC(m4)
rm4 <- residuals(m4)

plot(fitted(zip_model_same), rzip_model_same, xlab = "Fitted values of first model",
     ylab = "Deviance residuals")
abline(h = 0, col = "red", lty = 2) # Adding a horizontal line at zero for reference

plot(fitted(m4), rm4, xlab = "Fitted values of Second model",
     ylab = "Deviance residuals")
abline(h = 0, col = "red", lty = 2) # Adding a horizontal line at zero for reference