x <- 2
y <- 5
x
y
class(x)
is.integer(x)
is.numeric(x)
TRUE * 5
TRUE
vect <- c(10L, 150L, 30L, 40L, 55L)
class(vect)
statement <- c("hello")
dil <- "hi"
dil
class(dil)
class(statement)
p <- c(4, 5, 8, 9, 6, 5)
q <- c(3, 4)
p + q

ls()
library(swirl)
install_course_zip("C:/LYIT/Data Science/Software/swirl_courses-master.zip", multi = TRUE, which_course = "R Programming")
install_course("C:/LYIT/Data Science/Software/Getting_and_Cleaning_Data.swc")
swirl::install_course("Getting and Cleaning Data")

swirl()
library(dplyr)
remotes::install_github()
packageVersion("dplyr")
getwd()
.libPaths()
