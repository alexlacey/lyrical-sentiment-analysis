### This analysis involves the entire discography of the 100 top-selling artists of all time, according to Wikipedia.

# Loading the necessary R packages
library(dplyr)
library(cognizer)

### Two notes on this section:
# The lyrics were scraped using the Genius API with code written by Jack Schultz (https://bigishdata.com/2016/10/25/talkin-bout-trucks-beer-and-love-in-country-songs-analyzing-genius-lyrics/)
# This specific analysis with Watson was not repeated for popular song lyrics, movie scripts, or books because the free-trial of Watson had been exceeded

# Running Watson on each atist's lyrics
path <- "~/alexlacey/artists_lyrics/"
artists_list_names <- c(dir(path))
artists_list_names_underscore <- gsub(" ", "_", artists_list_names) # change " " to "_" in the vector
artists_list <- as.list(rep(NA, length(artists_list_names_underscore)))
names(artists_list) <- artists_list_names_underscore # empty list that will be filled with a vector of scores for each artist
for(artist in 1:length(artists_list)) {
  assign("path2", paste(path, artists_list_names[artist], sep=""))
  tmp_names <- assign(paste(artists_list_names_underscore[artist], "_vector_names", sep = ""), c(dir(path2))) # NOTE: double-naming
  tmp <- assign(paste(artists_list_names_underscore[artist], "_vector", sep = ""), c(rep(NA, length(tmp_names)))) # NOTE: double-naming
  names(tmp) <- tmp_names
  for(song in 1:length(tmp)) {
    song_string <- read_file(paste(path2, tmp_names[song], sep = ""))
    song_string <- gsub("\\[[^\\]]*\\]", "", song_string, perl = TRUE) # remove square-bracketed structural subtitles in lyrics files
    if(nchar(song_string) <= 30) {
      tmp[song] <- NA
    } else if(text_sentiment(song_string, key)[[1]]$status == "ERROR") {
      tmp[song] <- 0
    } else if(text_sentiment(song_string, key)[[1]]$status == "OK") {
      tmp[song] <- as.numeric(text_sentiment(song_string, key)[[1]][[5]]$score)
    }
    artists_list[[artist]] <- tmp
  }
}

# Finding out the number of songs for each artist
number_of_songs_vector <- c(length(artists_list)) # empty vector that will be filled with average scores for each artist
names(number_of_songs_vector) <- names(artists_list)
for (artist in artists_list) {
  append(length([artist]_vector) onto number_of_songs_vector)
}

# Taking the average score for each artist
average_scores_vector <- c(length(artists_list)) # empty vector that will be filled with average scores for each artist
names(average_scores_vector) <- names(artists_list)
for (artist in artists_list) {
  append(mean([artist]_vector) onto average_scores_vector)
}

# Taking the standard deviation score for each artist
stdev_scores_vector <- c(length(artists_list)) # empty vector that will be filled with average scores for each artist
names(stdev_scores_vector) <- names(artists_list)
for (artist in artists_list) {
  append(stdev([artist]_vector) onto stdev_scores_vector)
}
sentiment_table = cbind(
  names(artists_list), 
  number_of_songs_vector, 
  average_scores_vector, 
  stdev_scores_vector
)
colnames(sentiment_table) <- c("Artist", "# of Songs", "Average Score", "Standard Deviation")

