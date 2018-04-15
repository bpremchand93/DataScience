colnames <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")
date_col <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99)
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2)
q5_col <- c(5, 5, 2, NA, 1)

my_data <- data.frame(date_col, country_col, gender_col, age_col, q1_col, q2_col, q3_col, q4_col, q5_col)
colnames(my_data) <- colnames
my_data
head(my_data)
str(my_data)

my_data$Age[my_data$Age == 99] <- NA

#Create a new column called AgeCat and categorise
my_data$AgeCat[my_data$Age >= 45] <- "Elder"
my_data$AgeCat[my_data$Age >= 26 & my_data$Age <= 44] <- "Middle Aged"
my_data$AgeCat[my_data$Age <= 25] <- "Young"
my_data$AgeCat[is.na(my_data$Age)] <- "Elder"
#Show contents of my_data data frame
my_data

AgeCat <- factor(my_data$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))
AgeCat
my_data$AgeCat <- AgeCat

summary_col <- q1_col + q2_col + q3_col + q4_col + q5_col
summary_col


strDates <- c("29/08/2018", "27/09/2018")
strDates

date_format <- "%b %d %Y"
today <- Sys.Date()
output_date <- c(today, format = date_format)
output_date




startDate <- as.Date("2004-02-13")
endDate <- as.Date("2018-01-22")
days <- endDate - startDate
days

? difftime
dob <- as.Date("1993-10-11")
age <- difftime(Sys.Date(), dob, units = c("weeks"))
age


my_data
new_data <- my_data[order(my_data$Age),]
new_data

attach(my_data)
str(my_data$Age)
str(my_data$Gender)
new_data <- my_data[order(decreasing = TRUE, Gender, Age),]
new_data
new_data <- my_data[order(AgeCat),]

complete.cases(my_data)
my_data
new_data <- na.omit(my_data)
new_data
complete_data <- my_data[complete.cases(my_data),]
complete_data
missing_data <- my_data[!complete.cases(my_data),]
missing_data


new_data <- complete.cases(my_data)
new_data
mean(complete.cases(my_data$Age))

my_data
my_data$Date <- as.Date(my_data$Date, "%Y-%d-%m")
startdate <- as.Date("2018-10-01", "%Y-%d-%m")
enddate <- as.Date("2018-31-01", "%Y-%d-%m")
new_data <- my_data[which(my_data$Date >= startdate & my_data$Date <= enddate),]
new_data

attach(my_data)
new_data <- subset(my_data, Gender = "M" & Age > 25, select = c(Gender: Q4))
new_data

add_two_numbers <- function(no1, no2)
{
    result <- no1 + no2
    return (result)
}


value <- add_two_numbers(4, 5)
value
