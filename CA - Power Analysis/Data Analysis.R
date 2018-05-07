library(pwr)
library(dplyr)
effetive_size <- cohen.ES(test = "r", size = "large")
effetive_size

sample_size <- pwr.r.test(r = effetive_size$effect.size, sig.level = 0.05, power = 0.8)
sample_size

plot(sample_size)

income_data <- read.csv("Data/Average Quarterly Income.csv")
rent_data <- read.csv("Data/Average Quarterly Rent.csv")

data <- merge(income_data, rent_data)
head(data)

sample_data <- sample_n(data, 29)
head(sample_data)
nrow(sample_data)

cor.test(sample_data$Rent, sample_data$Income)