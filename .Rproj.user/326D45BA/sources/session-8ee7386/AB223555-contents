

#Function to roll a single die/Spin a spinner
roll <- function(){
  spinner <- c(1:6)
  sample(spinner, 1)
}

#Function to represent the player's turn
turn <- function(x) {
  x + roll()
}

#Test the Function
rolls <- NULL
for (i in 1:1000) {
  rolls[i] <- roll()
}
table(rolls)


#Space conversions (land on square with slide or ladder, go to next space)
#1 -> 38
#4 -> 14
#9 -> 31
#16 -> 6
#21 -> 42
#28 -> 84
#36 -> 44
#47 -> 26
#49 -> 11
#51 -> 67
#56 -> 53
#62 -> 19
#64 -> 60
#71 -> 91
#80 -> 100
#87 -> 24
#93 -> 73
#95 -> 75
#98 -> 78

df <- data.frame("Land" = c(1,4,9,16,21,28,36,47,49,51,56,62,64,71,80,87,93,95,98),
                 "GoTo" = c(38,14,31,6,42,84,44,26,11,67,53,19,60,91,100,24,73,75,78))

#Add column to identify Chute square or Ladder square
df$Cat <- rep(NA, 19)

for (i in 1:length(df$Land)) {
  if(df$Land[i]-df$GoTo[i] < 0) {
    df$Cat[i] <- "Ladder"
  } else {
    df$Cat[i] <- "Chute"
  }
}

player_test <- NULL
N <- 100
turns <- NULL

for (i in 1:N) {
  player <- 0
  turn_temp <- 0
  while (player < 101) {
    this_roll <- roll()
    if(player + this_roll == 100 | player == 100) {
      #player <- player + this_roll
      break
    } else if(player + this_roll >100){
      next
    } else {
      player <- player + this_roll
    }
    for (j in 1:length(df$Land)) {
      if(player == df$Land[j]) {
        player <- df$GoTo[j]
      } else {
        next
      }
    }
    turn_temp <- turn_temp + 1
  }
  #player_test[i] <- ifelse( player == 100, player, player + this_roll )
  #turns[i] <- turn_temp
  player_test <- append(player_test, ifelse(player == 100, player, player + this_roll))
  turns <- append(turns, turn_temp)
}

player <- 4
if(player == df$Land[2]) {
  player <- df$GoTo[2]
}

x <- 1
if(x == df$Land[1]) {
  x <- 38
}

test <- 0
while (test < 11) {
  test <- test + 1
}
