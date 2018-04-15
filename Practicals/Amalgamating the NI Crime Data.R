#Task 1:
csv_file_list <- list.files(path = "Data/NI Crime Data/", pattern = "*.csv")
combine_results <- function(file_list) {
    data_frame <- NULL
    for (file in file_list) {
        data <- read.csv(header = TRUE, paste("Data/NI Crime Data/", file, sep = ""), stringsAsFactors = FALSE)
        data_frame <- rbind(data_frame, data)
    }
    return(data_frame)
}

AllNICrimeData <- combine_results(csv_file_list)
head(AllNICrimeData, 10)
str(AllNICrimeData)
NROW(AllNICrimeData)
write.csv(AllNICrimeData, file = "Data/AllNICrimeData.csv", row.names = FALSE)

#Task 2:
AllNICrimeData <- AllNICrimeData[, - c(1, 3, 4, 8, 9, 11, 12)]
#Removing the duplicate records
AllNICrimeData <- unique(AllNICrimeData) #Removing duplictes
#Removing records with no latitude, longitude and location
AllNICrimeData <- AllNICrimeData[!is.na(AllNICrimeData$Latitude & AllNICrimeData$Longitude) & (AllNICrimeData$Location != "No Location"),]

head(AllNICrimeData, 10)
str(AllNICrimeData)
NROW(AllNICrimeData)


#Task3:
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
head(AllNICrimeData, 10)
str(AllNICrimeData)

#Task4:
#Replacing a string by null
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
# Replacing the empty values with NA
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
#Converting the location attribute to upper case
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)
str(AllNICrimeData)
head(AllNICrimeData, 10)




#Task5:

library(tidyverse)

tidy_location <- function(lat, lon) {
    index <- which.min(sqrt((complete_location$Latitude - lat) ^ 2 + (complete_location$Longitude - lon) ^ 2))
    complete_location[index,]$Location
}
#Spliiting the data into missing and complete data
missing_location <- AllNICrimeData[!complete.cases(AllNICrimeData$Location),]
complete_location <- AllNICrimeData[complete.cases(AllNICrimeData$Location),]
head(missing_location)

#Finding the location for the missing records by looking into the complete records using tidy_location function
missing_location <- mutate(missing_location, Location = map2_chr(Latitude, Longitude, tidy_location))
head(missing_location)

#Combining the two complete data and the records which were filled with location using tidy_location function
AllNICrimeData <- rbind(complete_location, missing_location)
head(AllNICrimeData, 10)
str(AllNICrimeData)


#Task6
library(dplyr)

postcode_data <- read.csv("Data/CLeanNIPostcodeData.csv", stringsAsFactors = FALSE)
#Removing duplicates
postcode_data <- unique(postcode_data)
#Loading only the Primary.Thorfare and Postcode to the tbl_df data frame
postcode_tbl <- tbl_df(postcode_data[, c(3, 8)])
str(postcode_tbl)
find_a_postcode <- function(location) {
    #Filtering the row matching the location using the filter function of dplyr library 
    filtered_row <- filter(postcode_tbl, Primary.Thorfare == location)
    #Finding the most repeated value and extracting its postcode using which.names function
    result <- names(which.max(table(filtered_row$Postcode)))
    return(result)
}

#Finding the postcode for each location using lapply the find_a_postcode function
postcodes <- lapply(AllNICrimeData$Location, find_a_postcode)
#Converting the list vector to character vector using sapply
postcodes <- sapply(postcodes, paste0, collapse = "")

class(postcodes)
NROW(postcodes)
head(postcodes)

#Task7
AllNICrimeData$Postcode <- postcodes
head(AllNICrimeData, 10)
str(AllNICrimeData)


#Task8
#Joining the records based on location and postcode using left_join function of dplyr library
AllNICrimeData <- left_join(AllNICrimeData, postcode_data[, c("Town", "County", "Primary.Thorfare", "Postcode")], by = c("Location" = "Primary.Thorfare", "Postcode" = "Postcode"))
#removing duplicate records
AllNICrimeData <- unique(AllNICrimeData)
#Ordering by Month
AllNICrimeData <- arrange(AllNICrimeData, Month)
#Removing unwanted records
AllNICrimeData <- AllNICrimeData[, c(1, 5, 2:4, 7, 8, 6)]
#Setting the County attribute to unordered factor
AllNICrimeData$County <- factor(AllNICrimeData$County, ordered = FALSE)
#Setting the Town attribute to unordered factor
AllNICrimeData$Town <- factor(AllNICrimeData$Town, ordered = FALSE)
head(AllNICrimeData, 10)
str(AllNICrimeData)


#Task9
write.csv(AllNICrimeData, file = "Data/FinalNICrimeData.csv ", row.names = FALSE)

#Task10
Strabane_crime_data <- filter(AllNICrimeData, grepl("STRABANE", Town) & grepl("BT82", Postcode))
NROW(Strabane_crime_data)
head(Strabane_crime_data, 10)
str(Strabane_crime_data)