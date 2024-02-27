library("MASS")
?Boston

str(Boston)
# Check for missing values
colSums(is.na(Boston))

# Drop rows with missing values
df <- Boston[complete.cases(Boston),]

# Check for duplicates
sum(duplicated(Boston))

# Drop duplicates
df <- Boston[!duplicated(Boston),]

#Explanatory Data Analysis

library(ggplot2)
library(psych)

summary(Boston)
describe(Boston$crim, quant = c(0.25, 0.5, 0.75), skew = TRUE)
describe(Boston$zn, quant = c(0.25, 0.5, 0.75), skew = TRUE)
describe(Boston$dis, quant = c(0.25, 0.5, 0.75), skew = TRUE)
describe(Boston$rad, quant = c(0.25, 0.5, 0.75), skew = TRUE)


# Plot histograms of numeric variables
ggplot(Boston, aes(x = crim)) + geom_histogram()
ggplot(Boston, aes(x = zn)) + geom_histogram()
ggplot(Boston, aes(x = dis)) + geom_histogram()
ggplot(Boston, aes(x = rad)) + geom_histogram()
ggplot(Boston, aes(x = medv)) + geom_histogram()


#Plotting boxplots and outliers for the variables
boxplot(Boston$crim, main="Crime")$out
boxplot(Boston$zn, main="Proportion of Residential Land")$out
boxplot(Boston$dis, main="Distance")$out
boxplot(Boston$rad, main="Radial Highway")$out
boxplot(Boston$medv, main="Median Value")$out

#Checking for correlation
correlation_coefficient <- cor(Boston$crim, Boston$zn)
correlation_coefficient
correlation_coefficient <- cor(Boston$dis, Boston$rad)
correlation_coefficient
correlation_coefficient <- cor(Boston$crim, Boston$dis)
correlation_coefficient
correlation_coefficient <- cor(Boston$crim, Boston$rad)
correlation_coefficient

#Conducting linear regression
install.packages("tidyr")
install.packages("carData")

library (dplyr)
library (tidyr)
library (car)

model <- lm("medv~crim+zn+dis+rad", data = Boston)
summary(model)

#ASSUMPTIONS FOR LINEAR REGRESSION

# Check for multicollinearity
vif(model)
#tolerance
1/vif(model)
#mean vif
mean(vif(model))

#Check for independence 
dwt(model)




# Check for normality of the residuals
residuals <- resid(model)
ggplot(data.frame(residuals), aes(x = residuals)) + geom_histogram()

plot(model)
plot
# Check for heteroscedasticity of the residuals
ggplot(df, aes(x = fitted(model), y = residuals)) + geom_point()


