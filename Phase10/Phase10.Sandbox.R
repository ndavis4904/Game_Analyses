library(mosaic)
library(combinat)

phase.dat <- read.csv("./Phase10/Data/Phase10Dat.csv")
#phase.dat$Number <- as.factor(phase.dat$Number)

phases <- c("2 Sets of 3", "1 Set of 3 & 1 Run of 4", "1 Set of 4 & 1 Run of 4",
            "1 Run of 7", "1 Run of 8", "1 Run of 9", "2 Sets of 4", "7 Cards of 1 Color",
            "1 Set of 5 & 1 Set of 2", "1 Set of 5 & 1 Set of 3")

draw.fun <- function(hand) {
  new.card <- phase.dat[sample(c(1:nrow(phase.dat)), 1),]
  hand <- rbind(hand, new.card)
}

difference <- function(x, y) {
  abs(x-y)
}

#Descriptive Stats of the Deck
table(phase.dat$Number)
table(phase.dat$Color)


#Drawing and sorting a hand
hand <- phase.dat[sample(c(1:nrow(phase.dat)), 10),]
hand <- draw.fun(hand)
hand <- hand[order(as.numeric(hand$Number)),]

if(sum(is.na(hand$Color)) > 0){
  hand_simple <- hand[is.na(hand$Color) == FALSE,]
} else {
  hand_simple <- hand
}


##Counting each of the numbers
hand.dim <- data.frame(table(hand$Number))
hand.dim <- hand.dim[order(hand.dim$Freq, decreasing = TRUE),]

hand.dim_simple <- data.frame(table(hand_simple$Number))
hand.dim_simple <- hand.dim_simple[order(hand.dim_simple$Freq),]

                          #First phase: 2 Sets of 3

##Comparing each of the numbers with each other
comb.hand <- unique(combn(hand_simple$Number,2, simplify = FALSE))

### Finding duplicate numbers (i.e. if there are two 5s, there may be a 5,5 comparison)
duplicates <- NULL
for(i in 1:length(comb.hand)) {
  coms <- comb.hand[[i]]
  ifelse(coms[1] == coms[2], duplicates <- append(duplicates, i), next)
}
comb.hand <- comb.hand[-duplicates]

###Comparing each of the generated combinations and generating completion message
card_frequency <- sum(grepl("Wild", hand$Number))
for(i in 1:length(comb.hand)) {
  comparison <- comb.hand[[i]]
  for(j in 1:nrow(hand.dim)) {
    if(comparison[1] == hand.dim$Var1[j]) {
      card_frequency <- append(card_frequency, hand.dim$Freq[j])
    } else{card_frequency <- append(card_frequency, 0)}
    if(comparison[2] == hand.dim$Var1[j]) {
      card_frequency <- append(card_frequency, hand.dim$Freq[j])
    } else{card_frequency <- append(card_frequency, 0)}
  }
  if(sum(card_frequency) >= 6) {
    print("Lay Down")
    break
  } else{next}
  
}

#Discard a card at the end of your turn
###Prioritize getting rid of skips
skips <- grep("Skip", hand$Number)
if(sum(grepl("Skip", hand$Number)) > 0) {
  hand[-skips,] 
} else {
  hand[-hand.dim_simple$Var1[1],]
}

                           #Second Phase: 1 Set of 3 & 1 Run of 4

                            #Third Phase: 1 Set of 4 & 1 Run of 4

                                #Fourth Phase: 1 Run of 7

### Idea: Order all the cards and take difference of each pair

diffs <- NULL
for(i in 1:nrow(hand)) {
  diffs[i] <- difference(as.numeric(hand$Number[i]), as.numeric(hand$Number[i+1]))
}

hand$diffs <- diffs

while (x < 8) {
  
}

dif.dat <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(dif.dat) <- c("Card Combos", "Diff 0", "Diff 1", "Diff 2", "Diff 3", "Diff 4", 
                       "Diff 5", "Diff 6", "Diff 7", "Diff 8", "Diff 9", "Diff 10", 
                       "Diff 11")
combos <- c("1-7", "2-8", "3-9", "4-10", "5-11")
for(j in 1:length(combos)) {
  dif.dat[j,1] <- combos[j]
}

#All 11 Cards
#1-7
#2-8
#3-9
#4-10
#5-11

#10 Cards
#1-7
#2-8
#3-9
#4-10

#9 Cards
#1-7
#2-8
#3-9

for(i in 1:nrow(dif.dat)) {
  for(j in 1:ncol(dif.dat)) {
    for(k in 1:(nrow(hand_simple) - 6)) {
      
    }
  }
}

test <- NULL
for(i in 1:(nrow(hand_simple) - 6)) {
  test[i] <- diffs[i:i+5]
}


                                #Fifth Phase: 1 Run of 8

                                #Sixth Phase: 1 Run of 9

                              #Seventh Phase: 2 Sets of 4

                            #Eighth Phase: 7 Cards of 1 Color

##Color Dimensions
color.dim <- data.frame(table(hand$Color))
color.dim <- color.dim[order(color.dim$Freq, decreasing = TRUE),]

##Looking for Win condition
win <- NULL
for(i in 1:nrow(color.dim)) {
  if(color.dim$Freq[i] + table(hand$Number)[["Wild"]] >= 7) {
    win <- "Lay Down"
    print(win)
  } else {
    win <- "Didn't Lay Down"
    print(win)
  }
}

if(color.dim$Freq[1] + sum(grepl("Wild", hand$Number)) >= 7) {
  win <- "Lay Down"
} else {
  win <- "Didn't Lay Down"
}

print(win)

###Discarding card if win condition isn't met
hand.disc <- subset(hand, Color != color.dim$Var1[1] & Number != "Wild")
hand.stay <- subset(hand, Color == color.dim$Var1[1] | Number == "Wild")

if(win == "Didn't Lay Down") {
  hand <- rbind(hand.stay, hand.disc[-sample(nrow(hand.disc), 1),])
}

                           #Ninth Phase: 1 Set of 5 & 1 Set of 2

                           #Tenth Phase: 1 Set of 5 & 1 Set of 3

