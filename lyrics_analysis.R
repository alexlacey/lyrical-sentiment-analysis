# Loading the necessary R packages
library(readr)
library(syuzhet)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)

################################################################
### Analysis of Song Lyrics
################################################################

all_lyrics <- read.csv("https://raw.githubusercontent.com/walkerkq/musiclyrics/master/billboard_lyrics_1964-2015.csv", stringsAsFactors=FALSE)
# Those lyrics were scraped by Kaylin Walker, and her process was described on her website: http://kaylinwalker.com/50-years-of-pop-music/

all_lyrics$Lyrics <- sapply(all_lyrics$Lyrics, enc2utf8) # convert to a readable format
for (i in 1:nrow(all_lyrics)) { # quantify the length (number of characters) of each set of lyrics
  all_lyrics$lyrics_length[i] = nchar(all_lyrics$Lyrics[i])
}

# Get sentiment scores with all four lexicons available in the R "syuzhet" package
for (i in 1:nrow(all_lyrics)) {
  all_lyrics$afinn_score[i] <- round(get_sentiment(all_lyrics$Lyrics[i], method = "afinn"),  digits = 3)
  all_lyrics$nrc_score[i] <- round(get_sentiment(all_lyrics$Lyrics[i], method = "nrc"),  digits = 3)
  all_lyrics$bing_score[i] <- round(get_sentiment(all_lyrics$Lyrics[i], method = "bing"),  digits = 3)
  all_lyrics$syuzhet_score[i] <- round(get_sentiment(all_lyrics$Lyrics[i], method = "syuzhet"),  digits = 3)
}

# Adjust the scores so that the mean is 0 and the standard deviation is 1 (this allows for the lexicon results to be compared)
all_lyrics$afinn_scale <- scale(all_lyrics$afinn_score)
all_lyrics$nrc_scale <- scale(all_lyrics$nrc_score)
all_lyrics$bing_scale <- scale(all_lyrics$bing_score)
all_lyrics$syuzhet_scale <- scale(all_lyrics$syuzhet_score)
all_lyrics$Popularity <- 101 - all_lyrics$Rank

# Linear regression to determine the amount of significance in the trend (all were significant)
summary(lm(afinn_scale ~ Year, data = all_lyrics))
summary(lm(nrc_scale ~ Year, data = all_lyrics))
summary(lm(bing_scale ~ Year, data = all_lyrics))
summary(lm(syuzhet_scale ~ Year, data = all_lyrics))

# Plotting the results with one lexicon (identical graphs were also created for the other four lexicons)
afinn_graph <- ggplot(all_lyrics, aes(Year, afinn_scale)) + 
  geom_point(position = "jitter", size = 3, color = "red", alpha = 0.3) + 
  geom_smooth(method = lm, color = "grey0") +
  labs(x = "Year", y = "Sentiment Score") +
  ggtitle("Sentiment Scores of the Top 100 Songs Per Year") + 
  theme_few()
ggsave("~/Desktop/results/afinn_top_100.jpeg", plot = afinn_graph, width = 6, height = 6)

# Plotting the results with all four lexicons
multiplot <- grid.arrange(arrangeGrob(afinn_graph + theme(legend.position="none") + ylim(-4.5, 3.5),
                                      nrc_graph + theme(legend.position="none") + ylim(-4.5, 3.5),
                                      bing_graph + theme(legend.position="none") + ylim(-4.5, 3.5),
                                      syuzhet_graph + theme(legend.position="none") + ylim(-4.5, 3.5),
                                      nrow=1), top=textGrob("Comparison of Four Sentiment Analysis Methods for Top 100 Songs Per Year", gp=gpar(fontface="bold")))

# An F-test to compare the variation between the first 25 years and last 25 years of lyrics available in the dataset
era1_set <- all_lyrics %>% filter(date <= 1990)
era2_set <- all_lyrics %>% filter(date > 1990)
era1 <- lm(abs_res ~ date, era1_set)
era2 <- lm(abs_res ~ date, era2_set)
var.test(era1, era2) # the difference was significant

# All of the above code was also repeated for the top 10 songs of each decade.