

# Data Analysis # 

data = read.csv(file.choose(), sep = "\t")

head(data)

str(data)

# 1 

# price ~ baths

#Normality
shapiro.test(data$price) #p-value = 2.17e-05
shapiro.test(data$baths) #p-value = 1.662e-05
# we have strong evidence that the variables follow normal

# Correlation
cor.test(data$price, data$baths) # p-value = 0.0002302, rho = 0.3938008

# the Pearson's test showed p-value < 0.05 and rho = 0.3938008, indicating 
# a statistically significant positive correlation between two variables

# Visualization 

plot(data$baths, data$price,
col = "darkred",
main = "Scatterplot between price and baths", 
xlab = "baths",
ylab = "price")

# 2 

model1 = lm(price~baths, data = data)
summary(model1) 

# Linearity

plot(data$baths, data$price,
xlab = "number of bathrooms",
ylab = "house price",
col = "blue")

abline(model1,col="red",lwd=1)

# Residuals normality

residuals = rstandard(model1)

shapiro.test(residuals) # p-value = 4.877e-05

# residuals don't follow normal distribution

# Homoscedasticity

x1 = cut(data$baths, breaks = quantile(data$baths), indclude.lowest = T)

library(car)
leveneTest(residuals, x1)

# The assumption of homoscedasticity of the residuals is satisfied

# 3 

summary(model1)

# price = 176593 + 130108 * baths

# Intercept : If the number of bathrooms is 0
# the expected price of the house is 176593

# baths : If we increase the number of bathrooms by 1
# the expected price of the house increases by 130108

# 4 

data$log_price = log(data$price)

model2 = lm(log_price~baths, data = data)
summary(model2)

# Linearity

plot(data$baths, data$log_price,
col = "blue",
xlab = "number of bathrooms",
ylab = "house price"
)
abline(model2,col="red",lwd = 1)

# Residuals normality

residuals2 = rstandard(model2)

shapiro.test(residuals2)

# The assumption of normality of the residuals is satisfied in model2 

# Homoscedasticity

x2 = cut(data$baths, breaks = quantile(data$baths), include.lowest = T)
leveneTest(residuals2,x2)

# The assumption of homoscedasticity of the residuals is satisfied 

# Comment : It is observed that while in model1 the normality of the residuals
# was not satisfied, in model2 (after the logarithmic transformation) the normality
# condition was satisfied

# 5 

summary(model2)

# log_price =  12.2799 + 0.2707 * baths

# Intercept : If there are no bathrooms, the expected price of the logarithm of the house price 
# is 12.2799

# baths : If the number of bathrooms increases by 1, the expected increase in the logarithm of 
# the house price is 0.2707

# Predictive ability 

# Residual standard error : 0.5525
# The residual standard error indicates the average deviation of the observations from the 
# predicted values

# Multiple R - Squared : 0.1578
# Multiple R - Squared represents the proportion of variance in the observations explained by 
# the model. In our case, the multiple r squared is 0.1578, meaning the model explains only 
# 15.78% of the variance in the observations.

# Adjusted R - Squared : 0.1474
# Adjusted R - Squared, like Multiple R-squared, measures the proportion of variance in the 
# observations exlpained by the model, but it also takes into account the number of 
# independent variables

# F - statistic : 15.17 & p-value = 0.0002007
# The F-statistic with a p-value of 0.0002007 indicates that the model is statistically 
# significant and the independent variable has an effect on the logarithm of the dependent
# variable

# 6 

four_baths = data[data$baths == 4, ]

predict(model2, newdata = four_baths, interval ="confidence", level = 0.95)

# 7

# Check outliers
# Cooks distance
par(mfrow = c(1,2))

plot(model1,which = 4, 
main = "Ouliers model 1")

plot(model2,which = 4,
main = "Outliers model 2")

# Model 1 : 11, 36, 43

# Model 2 : 16, 18, 25










































