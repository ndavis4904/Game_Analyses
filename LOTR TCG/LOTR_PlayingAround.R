library(mosaic)

cards <- read.csv("LOTR TCG/AllCardData.csv")

mark <- "•"

cards$Unique <- grepl(mark, cards$Title)

cards.sub <- na.omit(cards[,c(2,7:9)])

cards.sub$diff.str <- cards.sub$STR - cards.sub$Twilight
cards.sub$good.str <- cards.sub$diff.str > 0

cards.sub$diff.hlth <- cards.sub$HP - cards.sub$Twilight
cards.sub$good.hlth <- cards.sub$diff.hlth > 0

cards.sub$all.good <- cards.sub$good.str + cards.sub$good.hlth == 2

plot(STR ~ Twilight, data = cards.sub,
     pch = 16,
     main = "Strength vs Twilight")
abline(a = 0, b = 1)

plot(HP ~ Twilight, data = cards.sub,
     pch = 16,
     main = "Health vs Twilight",
     col = as.factor(good.hlth))
abline(a = 0, b = 1)


hist(cards.sub$Twilight, breaks = 25,
     col = "skyblue",
     xlab = "Twilight", main = "Histogram of Twilight Value")
abline(v = mean(cards.sub$Twilight), col = "firebrick", lwd = 2, lty = 3)
summary(cards.sub$Twilight)

hist(cards.sub$STR, breaks = 20,
     col = "skyblue",
     xlab = "Strength", main = "Histogram of Strength Value")
abline(v = mean(cards.sub$STR), col = "firebrick", lwd = 2, lty = 3)
summary(cards.sub$STR)

hist(cards.sub$HP,
     col = "skyblue",
     xlab = "Health", main = "Histogram of Health Value")
abline(v = mean(cards.sub$HP), col = "firebrick", lwd = 2, lty = 3)
summary(cards.sub$HP)


ttest.twlt <- t.test(~ cards.sub$Twilight, groups = cards.sub$Side, alternative = "two.sided")
ttest.str <- t.test(~ cards.sub$STR, groups = cards.sub$Side, alternative = "two.sided")
ttest.hp <- t.test(~ cards.sub$STR, groups = cards.sub$Side, alternative = "two.sided")


stuff.twlt <- NULL

for(i in 1:length(ttest.twlt)) {
  stuff.twlt[i] <- ttest.twlt[i]
}

# working with text
library(stringr)
my_string <- cards$Game.text[1]

## Remove all non-alphanumeric characters
str_replace_all(my_string, "[^[:alnum:]]", "")

## Remove all punctuation
str_replace_all(my_string, "[[:punct:]]", "")

## removef \n from the end
str_replace_all(my_string, "\n", "")

rem_dup.one <- function(x){
  paste(unique(tolower(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",
                                              fixed=F,perl=T))))),collapse = " ")
}

str_split(my_string, " ", simplify = TRUE)

all.words <- NULL

for(i in 1:length(cards$Game.text.unique)) {
  words <- str_subset(str_split(cards$Game.text.unique[i], " ", simplify = TRUE), ".+")
  all.words <- append(all.words, words)
}

Word_Freq_All <- data.frame("Words" = unique(all.words), "Freq" = rep(NA, length(unique(all.words))))

for(i in 1:length(Word_Freq_All$Words)) {
  Word_Freq_All$Freq[i] <- table(all.words)[[Word_Freq_All$Words[i]]]
}

transition.words <- c("be", "and", "of", "a", "in", "to", "have", "it", "the",
                      "that", "for", "you", "he", "with", "on", "do", "is",
                      "this", "at", "but", "his", "from", "that", "may", "an",
                      "not", "by", "she", "or", "as", "their", "can", "s", "x",
                      "who", "if", "would", "her", "all", "my", "make", "about",
                      "as", "up", "one", "time", "so", "non", "those", "order",
                      "when", "which", "them", "people", "take", "same", "been",
                      "out", "into", "him", "your", "per", "being", "still", "again",
                      "could", "now", "than", "other", "then", "using", "every",
                      "its", "two", "more", "these", "way", "look", "iii", "c",
                      "first", "also", "first", "also", "more", "off", "k",
                      "use", "no", "man", "here", "many", "t")

test.df <- NULL
for(i in 1:length(transition.words)) {
  test.df <- Word_Freq_All[!grepl(paste(transition.words, collapse="|"), Word_Freq_All$Words),]
}

table(test.df$Words)[transition.words[15]]

