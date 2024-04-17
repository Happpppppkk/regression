mydata<- data.frame(ames_housing_data)

str(mydata)
head(mydata)
names(mydata)
#single family as popuolation of interest#
subset_mydata <- mydata[!(mydata$Zoning == 'C (all)') & 
                          mydata$GarageCars <= 4 & 
                          mydata$PoolArea <= 400 & 
                          !(mydata$MiscFeature %in% c('TenC', 'Elev', 'Othr')), ]

#Pick 20 variables
sub1 <- subset(subset_mydata, select=c("GarageCars","HeatingQC",
                            "BldgType", "YearRemodel","RoofStyle",
                              "FirstFlrSF","SecondFlrSF", "YearBuilt",
                                 "YrSold","SalePrice","LotArea","Neighborhood",
                                    "HouseStyle","OverallCond","TotRmsAbvGrd",
                                    "LotShape","OverallQual","ExterQual",
                                    "TotalBsmtSF","KitchenQual"))


#quality check

column_20var <- c("GarageCars","HeatingQC", "BldgType", "YearRemodel", "RoofStyle",
                      "FirstFlrSF", "SecondFlrSF", "YearBuilt", "YrSold", "SalePrice",
                      "LotArea", "Neighborhood", "HouseStyle", "OverallCond", "TotRmsAbvGrd",
                      "LotShape", "OverallQual", "ExterQual", "TotalBsmtSF", "KitchenQual")

# Function to apply quality check based on column type
quality_check <- function(column) {
  if (is.numeric(column)) {
    list(
      Summary = summary(column),
      Quantiles = quantile(column, na.rm = TRUE),
      Mean = mean(column, na.rm = TRUE),
      SD = sd(column, na.rm = TRUE)
    )
  } else {
    list(
      Frequency = table(column),
      Summary = summary(column)
    )
  }
}

# Apply the function to specified columns
result <- lapply(sub1, quality_check)
print(result)

#Data wrangling
#create a new var that use FirstFlrSF and SecondFlrSF
sub1$GrLivArea <- sub1$FirstFlrSF + sub1$SecondFlrSF
#EDA1
#Continuous Var

require(ggplot2)
ggplot(sub1, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sub1, aes(x=YearBuilt)) + 
  geom_histogram(color="black", binwidth= 1) +
  labs(title="Distribution of YearBuilt") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sub1, aes(x=TotalBsmtSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalBsmtSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sub1, aes(x=GrLivArea)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sub1, aes(x=LotArea)) + 
  geom_histogram(color="black", binwidth= 1000) +
  labs(title="Distribution of LotArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
#Discrete

ggplot(sub1) +
  geom_bar( aes(OverallQual) ) +
  ggtitle("Housing OverallQuality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1) +
  geom_bar( aes(GarageCars) ) +
  ggtitle("Housing Garage Capacity") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1) +
  geom_bar( aes(HeatingQC) ) +
  ggtitle("Housing Heating Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1) +
  geom_bar( aes(Neighborhood) ) +
  ggtitle("Housing Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
ggplot(sub1) +
  geom_bar( aes(KitchenQual) ) +
  ggtitle("Housing Kitchen Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


#bivariate#
ggplot(sub1, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sub1, aes(x=SalePrice, y=GarageCars)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Garage Capacity") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sub1, aes(x=YearBuilt, y=OverallQual)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#model focus#
ggplot(sub1, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs general living area") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###

ggplot(sub1, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Year Built") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

sub1$log_SalePrice <- log(sub1$SalePrice)

ggplot(sub1, aes(x =log_SalePrice, y = YearBuilt, color = as.factor(GarageCars))) +
  geom_point() +
  labs(title = "Scatter Plot with GrLivArea, Year Built, and GarageCars",
       x = "Sale Price",
       y = "Year Built ",
       color = "Garage Car Number") +
  theme_minimal()

ggplot(sub1, aes(x =log_SalePrice, y = GrLivArea, color = as.factor(GarageCars))) +
  geom_point() +
  labs(title = "Scatter Plot with GrLivArea, Year Built, and GarageCars",
       x = "Sale Price",
       y = "GrLivArea",
       color = "Garage Car Number") +
  theme_minimal()

model <- lm(SalePrice ~ GrLivArea + YearBuilt + GarageCars, data = sub1)

# Summary of the model
summary(model)
