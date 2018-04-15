#Task 1:
csv_file_list <- list.files(path = "C:/LYIT/Data Science/Practicals/Data/NI Crime Data/", pattern = "*.csv", recursive = TRUE)
combine_results <- function(file_list) {
    data_frame <- NULL
    for (file in file_list) {
        data <- read.csv(header = TRUE, paste("C:/LYIT/Data Science/Practicals/Data/NI Crime Data/", file, sep = ""), stringsAsFactors = FALSE)
        data_frame <- rbind(data_frame, data)
    }
    return(data_frame)
}

AllNICrimeData <- combine_results(csv_file_list)
str(AllNICrimeData)
write.csv(AllNICrimeData, file = "C:/LYIT/Data Science/Practicals/Data/AllNICrimeData.csv", row.names = FALSE)
#AllNICrimeData <- read.csv("C:/LYIT/Data Science/Practicals/Data/sample1.csv")

#Task 2:
AllNICrimeData <- AllNICrimeData[, - c(1, 3, 4, 8, 9, 11, 12)]
str(AllNICrimeData)

#Task3:
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
str(AllNICrimeData)
head(AllNICrimeData)
tail(AllNICrimeData)

#Task4:
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
AllNICrimeData <- AllNICrimeData[AllNICrimeData$Location != "No Location",]
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA # To replace the empty values with NA
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)
AllNICrimeData <- unique(AllNICrimeData)
str(AllNICrimeData)
head(AllNICrimeData)
tail(AllNICrimeData)


#new <- subset(AllNICrimeData, grepl("No Location", Location))
#str(new)



#Task5:

library(tidyverse)

tidy_location <- function(lat, lon) {
    index <- which.min(sqrt((complete_location$Latitude - lat) ^ 2 + (complete_location$Longitude - lon) ^ 2))
    complete_location[index,]$Location
}

missing_location <- AllNICrimeData[!complete.cases(AllNICrimeData$Location),]
head(missing_location)
str(missing_location)

complete_location <- AllNICrimeData[complete.cases(AllNICrimeData$Location),]
head(complete_location)
str(complete_location)


missing_location <- mutate(missing_location, Location = map2_chr(Latitude, Longitude, tidy_location))

str(missing_location)
head(missing_location)

AllNICrimeData <- rbind(complete_location, missing_location)
str(AllNICrimeData)
head(AllNICrimeData)

#Task6
postcode_data <- read.csv("C:/LYIT/Data Science/Practicals/Data/CLeanNIPostcodeData.csv", stringsAsFactors = FALSE)
library(dplyr)
postcode_data <- unique(postcode_data)
postcode_tbl <- tbl_df(postcode_data[, c(3, 8)])
str(postcode_tbl)
find_a_postcode <- function(location) {
    filtered_row <- filter(postcode_tbl, Primary.Thorfare == location)
    result <- names(which.max(table(filtered_row$Postcode)))
    return(result)
}

postcodes <- lapply(AllNICrimeData$Location, find_a_postcode)
postcodes <- sapply(postcodes, paste0, collapse = "")

class(postcodes)
NROW(postcodes)
head(postcodes)

#Task7
AllNICrimeData$Postcode <- postcodes
str(AllNICrimeData)
head(AllNICrimeData)


#Task8
AllNICrimeData <- left_join(AllNICrimeData, postcode_data[, c("Town", "County", "Primary.Thorfare", "Postcode")], by = c("Location" = "Primary.Thorfare", "Postcode" = "Postcode"))
AllNICrimeData <- unique(AllNICrimeData) #removing duplicate records
AllNICrimeData <- arrange(AllNICrimeData, Month) #Ordering by Month
AllNICrimeData <- AllNICrimeData[, c(1, 5, 2:4, 7, 8, 6)]
AllNICrimeData$County <- factor(AllNICrimeData$County, ordered = FALSE)
AllNICrimeData$Town <- factor(AllNICrimeData$Town, ordered = FALSE)
str(AllNICrimeData)
head(AllNICrimeData)


#Task9
write.csv(AllNICrimeData, file = "C:/LYIT/Data Science/Practicals/Data/FinalNICrimeData.csv", row.names = FALSE)

#Task10
Strabane_crime_data <- filter(AllNICrimeData, grepl("STRABANE", Town) & grepl("BT82", Postcode))
NROW(Strabane_crime_data)
head(Strabane_crime_data)