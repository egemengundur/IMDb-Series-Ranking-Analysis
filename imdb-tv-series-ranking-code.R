getwd()
setwd("C:/Users/gundu/OneDrive/Masaüstü/STAT291 FINAL PROJECT")

imdb_data <- read.csv("Series Rankings (IMDB Data).csv",
                      na.strings = c(NA,"-"))

imdb_data <- imdb_data[1:8]

# Some values can not be changed to numeric as these char values have commas in them.
# We first ommit the commas using gsub and then change the values to numeric.

imdb_data$Year <- gsub(",","",imdb_data$Year)
imdb_data$Year <- as.numeric(imdb_data$Year)

imdb_data$Votes <- gsub(",", "", imdb_data$Votes)
imdb_data$Votes <- as.numeric(imdb_data$Votes)

str(imdb_data)
summary(imdb_data)



# RESEARCH QUESTION 1: What is the distribution of ratings of tv series according to years?

rate_year <- table(imdb_data$Year, imdb_data$Rating)
unique_rates <- colnames(rate_year)
unique_years <- rownames(rate_year)
means <- c()

for (year in unique_years){
  sum_of_rating <- 0
  for (rate in unique_rates){
    current <- as.numeric(rate)*rate_year[year,][rate][[1]]
    sum_of_rating <- sum_of_rating + current
  }
  means <- c(means, sum_of_rating/sum(rate_year[year,]))
}

plot(unique_years,means,
     type = "o",
     pch = "*",
     xlim = c(2000,2021),
     xlab = "Years",
     ylab = "Rating Means",
     main = "DISTRIBUTION OF RATINGS OVER YEARS",
     col="purple")

hist(means,
     ylim = c(0,10),
     main = paste("HISTOGRAM OF RATING MEANS"),
     xlab = "Means of Rating",
     col = "pink")



# RESEARCH QUESTION 2: Is there any relation between ratings and votes? If yes, how can we describe this relation?


corimdb <- round(cor(imdb_data$Rating,imdb_data$Votes),4)

imdb_lm <- lm(Votes ~ Rating, data = imdb_data)

summary(imdb_lm)

plot(imdb_data$Rating,imdb_data$Votes,
     pch = 18,
     col = '#66CCCC',
     xlab = "RATE",
     ylab = "VOTE",
     main = "Relation between Ratings and Votes")

abline(imdb_lm,col= "#000033",lwd=2) #regression line

text(x = 8.5, y = 1500000, paste('Correlation :', corimdb))

# RESEARCH QUESTION 3: Which type of TV series do countries produce the most?

countries <- imdb_data$Country
genres <- strsplit(imdb_data$Genres, ",")

max_len = 0
for (item in 1:length(genres)){
  if (max_len < length(genres[[item]])){
    max_len <- length(genres[[item]])
  } 
}

countries_genres <- data.frame()
countries_genres[1, 1:(max_len+1)] <- rep(NA, max_len+1)


for (item in 1:length(genres)){
  countries_genres[item, 1:(length(genres[[item]])+1)] <- c(countries[item], genres[[item]])
}


unique_genres <- array()
for (level in 1:length(genres)){
  for (genre in 1:length(genres[[level]])){
    if (genres[[level]][genre] %in% unique_genres == FALSE){
      unique_genres <- c(unique_genres, genres[[level]][genre])
    }
  }
}

unique_genres_clean <- unique_genres[!is.na(unique_genres)]
unique_countries <- unique(countries)

count_countries_genres <- matrix(0, length(unique_countries),
                                 length(unique_genres_clean),
                                 dimnames = list(unique_countries, unique_genres_clean))

for (row in 1:length(countries_genres[, 1])){
  for (col in 1:max_len){
    if (is.na(countries_genres[row, col+1]) == FALSE){
      count_countries_genres[countries_genres[row, 1], countries_genres[row, col+1]] <- 
        count_countries_genres[countries_genres[row, 1], countries_genres[row, col+1]] +1
    }
  }
}
count_countries_genres

production <- sort(colSums(count_countries_genres), decreasing = T)

barplot(production, 
        main = "NUMBER OF GENRES PRODUCED BY COUNTRIES",
        col = '#CC9999',
        xlim = c(0,30),
        ylim = c(0,160),
        yaxt='n')
axis(side = 2, at= seq(0, 160, by=40))

rownames(count_countries_genres) <- c("IND","UK","US","JPN","TUR","DEU","SWE","IRL","CAN","EGY","MEX","ZAF","ESP","POL")

barplot(t(count_countries_genres),
        main = "GENRES PRODUCED BY COUNTRIES",
        col = c("darkslateblue","antiquewhite3","aquamarine3","azure4","black",
                "blue2","cornflowerblue","brown2","chartreuse1","chocolate1","cyan",
                "gold","palevioletred1","red1","darkorchid4","seagreen1","pink1",
                "salmon","yellow4","tomato","seashell3","powderblue","plum4"),
        ylim = c(0,400))
legend("topright",
       unique_genres_clean,
       fill = c("darkslateblue","antiquewhite3","aquamarine3","azure4","black",
                                    "blue2","cornflowerblue","brown2","chartreuse1","chocolate1","cyan",
                                    "gold","palevioletred1","red1","darkorchid4","seagreen1","pink1",
                                    "salmon","yellow4","tomato","seashell3","powderblue","plum4"))


barplot(t(count_countries_genres[rowSums(count_countries_genres) < 300,]),
        main = "GENRES PRODUCED BY COUNTRIES WITHOUT US",
        col = c("darkslateblue","antiquewhite3","aquamarine3","azure4","black",
                "blue2","cornflowerblue","brown2","chartreuse1","chocolate1","cyan",
                "gold","palevioletred1","red1","darkorchid4","seagreen1","pink1",
                "salmon","yellow4","tomato","seashell3","powderblue","plum4"),)
legend("topright",
       unique_genres_clean,
       fill = c("darkslateblue","antiquewhite3","aquamarine3","azure4","black",
                "blue2","cornflowerblue","brown2","chartreuse1","chocolate1","cyan",
                "gold","palevioletred1","red1","darkorchid4","seagreen1","pink1",
                "salmon","yellow4","tomato","seashell3","powderblue","plum4"))




