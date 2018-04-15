my_data <- read.csv("C:/LYIT/Data Science/Practicals/Data/Diabetes-md.csv") # To import csv file to a data frame
str(my_data) # To get the structure of the variable 
class(my_data) # To get the class type of the variable

column_names <- c("Patient name", "NI address", "Type", "Age", "Health status") 
colnames(my_data) <- column_names # Assigning the column names of a dataframe using the character variable
head(my_data) # To get the first few records in a data frame, By default it displays 6 records


Type <- factor(my_data$Type, levels = c("Type 1", "Type 2")) # Refactoring the levels
my_data$Type <- Type
Status <- factor(my_data$`Health status`, levels = c("Poor", "Improved", "Excellent"), order = TRUE) # Refactoring the order and levels
my_data$`Health status` <- Status

str(my_data) # To get the structure of the variable 

patient_names <- data.frame(my_data$`Patient name`) # To create a new dataframe based on values from another data frame
head(patient_names, 10) # Displays first 10 records in a dtaframe

colSums(is.na(my_data) | my_data == "") # To count the number of NA and emty values in each column
sum(is.na(my_data) | my_data == "") # To count the number of NA and emty values in the data frame

my_data[my_data == ""] <- NA # To replace the empty values with NA

my_data <- na.omit(my_data) # To remove the records having NA values
nrow(my_data) # To count the number of records
