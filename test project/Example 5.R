library(pwr)

a <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50), sig.level = 0.05, power = 0.80, alternative = "greater")
plot(a)

data(diamonds, package = "ggplot2")

x <- c(0.354, 0.54, 0.684, 0.846, 0.852, 0.963, 0.147, 0.258, 0.369)
round(exp(diff(log(x))), 1)

library(magrittr)

x %>% log() %>%
    diff() %>%
    exp() %>%
    round(1)
library(dplyr)
head(diamonds)
diamonds %>% head()
diamonds %>% slice(c(1:5, 8, 15:20)) %>% head()

diamonds %>% select(carat, price) %>% mutate(ratio = price / carat, Double = ratio * 2)

set <- diamonds %>% select(carat, price)
ratio <- set %>% mutate(ratio = price / carat)
ratio

diamonds %>% summarise(AvgPrice = mean(price), MedianPrice = median(price), AvgCarat = mean(carat))

diamonds %>% group_by(cut) %>% summarise(AvgPrice = mean(price), SumCarat = sum(carat)) %>% arrange(AvgPrice)

lotto <- read.csv("C:/Users/Goutham Siddhaarth/Source/Repos/DataScience/test project/Data/Lotto.csv")

head(lotto)

install.packages("hflights")
library(hflights)
library(dplyr)
head(hflights)
str(hflights)
class(hflights)
hflights_df <- tbl_df(hflights)
str(hflights_df)

f_df <- filter(hflights_df, Month == 1, UniqueCarrier == "AA")
f_df
hflights_df %>% filter(UniqueCarrier == "AA" | UniqueCarrier == "UA")

hflights_df %>% arrange(desc(Month, DayofMonth, ArrTime))
hflights_df %>% select(c(1:4, 9, 10))
Gain <- hflights_df %>% mutate(Gain = ArrDelay - DepDelay, Gain_PerHour = Gain / (AirTime / 60))

str(Gain)

dose <- c(20, 30, 40, 45, 60)
DrugA <- c(16, 20, 27, 40, 60)
DrugB <- c(15, 18, 25, 31, 40)

drugs <- data.frame(dose, DrugA, DrugB)
colnames(drugs) <- c("Dose", "DrugA", "DrugB")
drugs
plot(drugs)
plot(dose, type = "o", col = "blue")

plot(dose, DrugA, type = "b")
par(new = TRUE)
plot(dose, DrugB, type = "b")

opar <- par(no.readonly = TRUE)

par(lty = 2, pch = 17)

plot(dose, DrugA, type = "b")

par(opar)

plot(DrugA, type = "b", lty = 3, lwd = 3, pch = 15, cex = 2, ylim = graph_range, axes = FALSE, xlab = "Milliliters")
lines(DrugB, type = "o", pch = 22, lty = 2, col = "red")
axis(1, at = 1:5, labels = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml"))
axis(2, las = 2, at = 5 * 0:graph_range[2])
box()
title(main = "Drug dosage", col.main = "blue", font.main = 4)

range(DrugA)
graph_range <- range(0, DrugA, DrugB)
graph_range

scatter.smooth(x = cars$speed, y = cars$dist, maintainer = "Dist ~ Speed")

library(e1071)
par(mfrow = c(1, 2))
plot(density(cars$speed), main = "Density Plot :Speed",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(cars$speed), 2)))

polygon(density(cars$speed), col = "red")

plot(density(cars$dist), main = "Density Plot :Distance",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(cars$dist), 2)))

polygon(density(cars$dist), col = "red")

cor(cars$speed, cars$dist)

linearMod <- lm(dist ~ speed, data = cars)
print(linearMod)
summary(linearMod)
AIC(linearMod)
BIC(linearMod)

no_of_records <- sample(1:nrow(cars), 0.8 * nrow(cars))
training_data <- cars[no_of_records,]
testing_data <- cars[-no_of_records,]

lr_model <- lm(dist ~ speed, data = training_data)
dist_predicted <- predict(lr_model, testing_data)
dist_predicted
summary(lr_model)

actual_preds <- data.frame(cbind(actuals = testing_data$dist, predicted = dist_predicted))
actual_preds

correlation_accuracy <- cor(actual_preds)
correlation_accuracy

min_max_accuracy <- mean(apply(actual_preds, 1, min) / apply(actual_preds, 1, max))
min_max_accuracy

mape <- mean(abs(actual_preds$predicted - actual_preds$actuals) / actual_preds$actuals)
mape

install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = cars, form.lm = dist ~ speed, m = 5, dots = FALSE, seed = 29, legend.pos = "topleft", printit = FALSE, main = "Small symbols are predicted values while bigger ones are actuals"))

ts_data <- EuStockMarkets[, 1]
opar <- par()
par(mfrow = c(1, 2))
decomposed_result <- decompose(ts_data, type = "mult")
plot(decomposed_result)
decomposed_additive_result <- decompose(ts_data, type = "additive")
plot(decomposed_additive_result)
seasonal_trend_error <- stl(ts_data, s.window = "periodic")
par <- opar
seasonal_trend_error$time.series
lagged_ts <- lag(ts_data, 3)







library(DataCombine)

my_dataframe <- as.data.frame(ts_data)
my_dataframe <- slide(my_dataframe, "x", NewVar = "xLag1", slideBy = -1)

my_dataframe <- slide(my_dataframe, "x", NewVar = "xLead", slideBy = -1)
head(my_dataframe)

acf_res <- acf(AirPassengers)
pacf_res <- pacf(AirPassengers)

par(mfrow = c(2, 2))

plot(JohnsonJohnson)
traned_model <- lm(JohnsonJohnson ~ c(1:length(JohnsonJohnson)))
plot(resid(traned_model), type = "l")

library(forecast)
ts_decompose <- stl(AirPassengers, "periodic")
ts_seasonal_adjust <- seasadj(ts_decompose)
plot(AirPassengers, type = "l")
plot(ts_seasonal_adjust, type = "l")

par(mfrow = c(1, 1))
seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels = TRUE, main = "Seasonal plot:Airpassengers")

library(tseries)
adf.test(ts_data)
kpss.test(ts_data)
nsdiffs(AirPassengers)

AirPassengers_seasdiff <- diff(AirPassengers, lag = frequency(AirPassengers), differences = 1)
plot(AirPassengers_seasdiff, type = "l", maintainer = "Seasonally Differenced")
par(mfrow = c(1, 1))
library(tseries)
plot(Nile)
ndiffs(Nile)
d_nile <- diff(Nile)
plot(d_nile)
ndiffs(d_nile)
adf.test(d_nile)
Acf(d_nile)
Pacf(d_nile)
fit <- Arima(Nile, order = c(0, 1, 1))
fit
accuracy(fit)
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
forecast(fit, 3)
plot(forecast(fit, 3))

fit <- auto.arima(Nile)
fit