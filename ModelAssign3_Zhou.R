#Assignment3
mydata<- data.frame(ames_housing_data)
str(mydata)
head(mydata)
names(mydata)
#Question10
#Find the 10 continuous variables
cont_vars10 <- c("LotArea", "TotalBsmtSF",
                     "GrLivArea", "GarageArea", "OpenPorchSF",
                    "YearBuilt", "YearRemodel", "FullBath",
                 "TotRmsAbvGrd", "GarageCars",
                 "SalePrice")
#question 11
data1 <- subset(mydata, select = c("LotArea", "TotalBsmtSF",
                                   "GrLivArea", "GarageArea", "OpenPorchSF",
                                   "SalePrice"))
model3 = lm(SalePrice ~ LotArea+TotalBsmtSF+GrLivArea
                   +GarageArea+OpenPorchSF, data=data1)
anova(model1)
summary(model1)
#QUESTION 13
data2 <- subset(mydata, select = c("LotArea", "TotalBsmtSF",
                                   "GrLivArea", "GarageArea", "OpenPorchSF",
                                   "FullBath", "TotRmsAbvGrd", 
                                   "GarageCars", "SalePrice"))
model4 = lm(SalePrice ~ LotArea+TotalBsmtSF+GrLivArea
            +GarageArea+OpenPorchSF+FullBath+TotRmsAbvGrd
            +GarageCars, data=data2)
anova(model4)
summary(model4)
#question14
anova(model3, model4)
