# To import csv file to a data frame
postcode_data <- read.csv("C:/Users/Goutham Siddhaarth/Source/Repos/DataScience/Practicals/Data/NIPostcodes.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

#Step 1: Show the structure and first 10 rows of the dataframe containing all of the NIPostcode data.
str(postcode_data)
head(postcode_data, 10)

#Step 2: Show the total number and mean missing values of the NIPostcode data.
#Number of missing values in the whole data
sum(is.na(postcode_data))
mean(is.na(postcode_data))
#Number of rows missing values
missing_values <- !complete.cases(postcode_data)
sum(missing_values)
mean(missing_values)
#Number of missing values in each column
sapply(postcode_data, function(x) sum(is.na(x)))
sapply(postcode_data, function(x) mean(is.na(x)))

#Step 3: Removing the columns which had more NA values and which are not much important for the postcode
postcode_data <- postcode_data[, - c(1, 2, 3, 6, 7)]
head(postcode_data)
str(postcode_data)


#Creating a new column counting the number of NA in each row
#postcode_data$'NA Count' <- apply(postcode_data, 1, function(x) sum(is.na(x)))
#head(postcode_data)
#cleaned_postcode_data <- postcode_data[postcode_data$`NA Count` < 8,]
#cleaned_postcode_data <- cleaned_postcode_data[1:15]
#str(cleaned_postcode_data)

#Step 4: Add a suitable title to each attribute of the data.
col_name <- c("Number", "Primary Thorfare", "Locality", "Townland", "Town", "County", "Postcode", "x - coordinates", "y - coordinates", "ID")
colnames(postcode_data) <- col_name
head(postcode_data)
str(postcode_data)

#Step 5: Modify the County attribute to a categorising factor.
postcode_data$County <- factor(postcode_data$County)
str(postcode_data)


#Step7: Move the primary key identifier to the start of the dataset.
postcode_data <- postcode_data[, c(10, 1:9)]
head(postcode_data)
str(postcode_data)

#Step 8: Create a new dataset called Limavady_data and store it as a csv.
Limavady_data <- subset(postcode_data, grepl("LIMAVADY", Locality) & grepl("LIMAVADY", Townland) & grepl("LIMAVADY", Town))
str(Limavady_data)
head(Limavady_data)
write.csv(Limavady_data, file = "C:/Users/Goutham Siddhaarth/Source/Repos/DataScience/Practicals/Data/Limavady.csv", row.names = FALSE)

#Step 9: Save the modified dataset in a csv file called CleanNIPostcodeData.
write.csv(postcode_data, file = "C:/Users/Goutham Siddhaarth/Source/Repos/DataScience/Practicals/Data/CLeanNIPostcodeData.csv", row.names = FALSE)

#Step 6: Align all attributes and relevant data
library(knitr)
postcode_data <- kable(postcode_data)
head(postcode_data, 10)
str(postcode_data)
class(postcode_data)
