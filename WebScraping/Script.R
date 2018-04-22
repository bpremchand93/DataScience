#install.packages("rvest")
library(rvest)

url <- 'https://www.imdb.com/search/title?release_date=2017-01-01,2017-12-31&count=250'

web_page <- read_html(url)

head(web_page)
str(web_page)

Director_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(5) > a:nth-child(1)')
head(Director_data_html)
director_data <- html_text(Director_data_html)
head(director_data)

rank_data_html <- html_nodes(web_page, '.text-primary')
head(rank_data_html, 10)

rank_data <- html_text(rank_data_html)
head(rank_data)
length(rank_data)

rank_data <- as.numeric(rank_data)
head(rank_data)

title_data_html <- html_nodes(web_page, 'div.lister-item-content > h3 > a')
head(title_data_html)

title_data <- html_text(title_data_html)
head(title_data)
length(title_data)



description_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(4)')
head(description_data_html)

description_data <- html_text(description_data_html)
head(description_data)

description_data <- gsub("\n *", "", description_data)


stars_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(5) > a:nth-child(1)')
stars_data <- html_text(stars_data_html)
head(stars_data)
length(stars_data)


runtime_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(2) > span.runtime')
runtime_data <- html_text(runtime_data_html)
head(runtime_data)

runtime_data <- gsub(" min", "", runtime_data)
runtime_data <- as.numeric(runtime_data)


genre_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(2) > span.genre')
genre_data <- html_text(genre_data_html)
head(genre_data)
genre_data <- gsub("\n *", "", genre_data)
genre_data <- gsub(" ", "", genre_data)
genre_data <- gsub(",.*", "", genre_data)

head(genre_data)
genre_data <- trimws(genre_data)
length(genre_data)

ratings_data_html <- html_nodes(web_page, 'div.lister-item-content > div > div.inline-block.ratings-imdb-rating > strong')
ratings_data <- html_text(ratings_data_html)
head(ratings_data)
length(ratings_data)
ratings_data <- as.numeric(ratings_data)

director_and_stars_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(5)')
director_and_stars_data <- html_text(director_and_stars_data_html)
head(director_and_stars_data)
length(director_and_stars_data)

director_data <- gsub(" Stars:.*", "", director_and_stars_data)
head(director_data)
length(director_data)
director_data <- gsub("\n", "", director_data)
director_data <- gsub("|", "", director_data)
director_data <- trimws(director_data)