library(rvest)
library(dplyr)
library(readr)

url <- 'http://www.property.ie/property-to-let/ireland'


web_page <- read_html(url)

head(web_page)
#Scrapping the location data
location_data_html <- html_nodes(web_page, 'div.sresult_address > h2 > a')
location_data <- html_text(location_data_html)
location_data <- gsub("\n *", "", location_data)
location_data <- gsub(".*,", "", location_data)
location_data <- trimws(location_data)
location_data
length(location_data)
class(location_data)

#Scrapping the price data
covert_rent_from_weekly_to_monthly <- function(value) {
    if (grepl("weekly", value)) {
        result <- gsub(" weekly", "", value)
        result <- readr::parse_number(result) #Removing the currency and converting to numeric
        result <- result * 4 #Converting the price to monthly
        return(result)
    }
    else if (grepl("monthly", value)) {
        result <- gsub(" monthly", "", value)
        result <- readr::parse_number(result) #Removing the currency and converting to numeric
        return(result)
    }
}
price_data_html <- html_nodes(web_page, 'div.sresult_description > h3')
price_data <- html_text(price_data_html)
price_data <- gsub("\n *", "", price_data)
price_data <- trimws(price_data)
price_data <- lapply(price_data, covert_rent_from_weekly_to_monthly)
price_data <- sapply(price_data, paste0, collapse = "")
head(price_data, 10)
length(price_data)
class(price_data)

#Scrapping the house data. Splitting it into three house type, bedroom data, bathroom data
house_data_html <- html_nodes(web_page, ' div.sresult_description > h4')
house_data <- html_text(house_data_html)
house_data <- gsub("\n *", "", house_data)
head(house_data)

#This function helps to convert studio apartment to, 1 bedroom & 1bathroom
convert_studio_to_single <- function(value) {

    if (grepl("studio", value, ignore.case = TRUE)) {
        result <- 1
        return(result)
    }
    else {
        result <- as.numeric(value)
        return(result)
    }
}

bedroom_data <- gsub("bedroom.*", "", house_data)
bedroom_data <- lapply(bedroom_data, convert_studio_to_single)
bedroom_data <- sapply(bedroom_data, paste0, collapse = "")
head(bedroom_data, 10)
class(bedroom_data)

bathroom_data <- gsub(".*), ", "", house_data)
bathroom_data <- gsub("bathroom.*", "", bathroom_data)
bathroom_data
bathroom_data <- lapply(bathroom_data, convert_studio_to_single)
bathroom_data <- sapply(bathroom_data, paste0, collapse = "")
bathroom_data
head(bathroom_data)
length(bathroom_data)
class(bathroom_data)

#This function helps to find the house type
find_house_type <- function(value) {
    if (grepl("studio", value, ignore.case = TRUE)) {
        return("Studio")
    } else if (grepl("apartment", value, ignore.case = TRUE)) {
        return("Apartment")
    } else {
        return("House")
    }
}
house_type_data <- lapply(house_data, find_house_type)
house_type_data <- sapply(house_type_data, paste0, collapse = "")
house_type_data
class(house_type_data)

#Data frame is built
house_rent_data <- data.frame(house_type_data, bedroom_data, bathroom_data, price_data, location_data)
colnames(house_rent_data) <- c("House Type", "Number of Bedrooms", "Number of Bathrooms", "Rent per month in Euro", "Location")
tail(house_rent_data, 10)

saveRDS(house_rent_data,file = "house_rent.rds")