temp <- read.csv("C:/LYIT/Data Science/Lectures/Data/Lotto/1999.csv")
str(temp)
head(temp)
csv_file_list <- list.files(path = "C:/LYIT/Data Science/Lectures/Data/Lotto/", pattern = "*.csv")
csv_file_list

combine_results <- function(file_list) {
    data_frame <- NULL
    for (file in file_list) {
        data <- read.csv(header = TRUE, paste("C:/LYIT/Data Science/Lectures/Data/Lotto/", file, sep = ""), stringsAsFactors = FALSE)
        data_of_interest <- data[2:9]
        data_frame <- rbind(data_frame, data_of_interest)
    }
    return(data_frame)
}

new_data <- combine_results(csv_file_list)
str(new_data)

a <- matrix(1:9, 3)
b <- matrix(1:5)
b
c <- matrix(1:4, 2)
c
d <- matrix(2)
first_list <- list(a = matrix(1:9, 3), b = matrix(1:5), c = matrix(1:4, 2), d = matrix(2))
second_list <- list(a = matrix(1:9, 3), b = matrix(1:7), c = matrix(1:4, 2), d = matrix(2))
my_list
lapply(my_list, sum)
sapply(my_list, sum)
simple_function <- function(first_list, second_list) {
    result <- NROW(first_list) + NROW(second_list)
    return(result)
}

mapply(simple_function,first_list,second_list)