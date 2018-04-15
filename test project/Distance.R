install.packages("dirdel")
library(dirdel)
library(sp)

#Let's generate some random customer service locations
locations <- data.frame(x = runif(900), y = runif(900))

#This will calculate the Dirichlet tessellation:
dirichlet <- dirdel(locations$x, locations$y)

#And the tiles from the tessellation:
tiles <- tile.list(dirichlet)

#Let's have a quick look:
plot(tiles)

#Now let's get some customers
customers <- data.frame(custID = 1:7E6)
coordinates(customers) <- cbind(runif(7E6), runif(7E6))

#We'll turn our tile map into a SpatialPolygons object (this code comes straight from the Stack Overflow link above):
polys <- vector(mode = 'list', length = length(tiles))
for (i in seq(along = polys)) {
    pcrds <- cbind(tiles[[i]]$x, tiles[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID = as.character(i))
}
tiles <- SpatialPolygons(polys)

#This was the most time-consuming step, took about 5 min on my machine with 2 GB RAM:
closest <- over(customers, tiles)



#installed.packages()
#install.packages("geosphere")
library(geosphere)
tidy_location <- function(missing_data) {
    distance <- distm(missing_data[, c('Longitude', 'Latitude')], complete_location[, c('Longitude', 'Latitude')], fun = distVincentyEllipsoid)
    missing_data$Location <- complete_location$Location[max.col(-distance)]

    return(missing_data)
}


getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
