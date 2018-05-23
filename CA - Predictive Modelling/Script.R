#Load the data
data <- read.csv("Data/Rent and Income Data.csv")
head(data)

#Lets Plot
plot(data)

#Lets compare Income and Rent
plot(data$Income, type = "o", col = "blue")
par(new = TRUE)
plot(data$Rent, type = "b")

#Relation between Income and Rent
plot(data$Income, data$Rent, type = "o", col = "blue", xlab = "Income", ylab = "House Rent")
title(main = "Income and House Rent Relation", col.main = "blue", font.main = 4)

#Scatter Plot
scatter.smooth(x = data$Income, y = data$Rent, maintainer = "Rent ~ Income")

#Density Plot
library(e1071)
par(mfrow = c(1, 2))
plot(density(data$Income), main = "Density Plot :Income",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(data$Income), 2)))
polygon(density(data$Income), col = "red")

plot(density(data$Rent), main = "Density Plot :Rent",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(data$Rent), 2)))
polygon(density(data$Rent), col = "red")

#Correlation Test
cor(data$Income, data$Rent)

#Linear Model
linearMod <- lm(Rent ~ Income, data = data)
print(linearMod)
summary(linearMod)

#Polynomial Model
polynomialMod <- lm(Rent ~ Income + I(Income ^ 2), data = data)
print(polynomialMod)
summary(polynomialMod)

#Sampling
no_of_records <- sample(1:nrow(data), 0.8 * nrow(data))
training_data <- data[no_of_records,]
testing_data <- data[-no_of_records,]

#Training Linear Model
lr_model <- lm(Rent ~ Income, data = training_data)
lm_predicted <- predict(lr_model, testing_data)
lm_predicted

lm_actual_preds <- data.frame(cbind(actuals = testing_data$Rent, predicted = lm_predicted))
lm_actual_preds

#Training Polynomial Model(Second Order)
pl_model <- lm(Rent ~ Income + I(Income ^ 2), data = training_data)
pl_predicted <- predict(pl_model, testing_data)
pl_predicted

pl_actual_preds <- data.frame(cbind(actuals = testing_data$Rent, predicted = pl_predicted))
pl_actual_preds

#Lets validate, Compare and Decide which model fits our data
#AIC
AIC(linearMod)
AIC(polynomialMod)

#BIC
BIC(linearMod)
BIC(polynomialMod)

#Correlation Accuracy
lm_correlation_accuracy <- cor(lm_actual_preds)
lm_correlation_accuracy

pl_correlation_accuracy <- cor(pl_actual_preds)
pl_correlation_accuracy

#Min_Max Accuracy
lm_min_max_accuracy <- mean(apply(lm_actual_preds, 1, min) / apply(lm_actual_preds, 1, max))
lm_min_max_accuracy

pl_min_max_accuracy <- mean(apply(pl_actual_preds, 1, min) / apply(pl_actual_preds, 1, max))
pl_min_max_accuracy

#Mape
lm_mape <- mean(abs(lm_actual_preds$predicted - lm_actual_preds$actuals) / lm_actual_preds$actuals)
lm_mape

pl_mape <- mean(abs(pl_actual_preds$predicted - pl_actual_preds$actuals) / pl_actual_preds$actuals)
pl_mape

#Summary
summary(lr_model)
summary(pl_model)
