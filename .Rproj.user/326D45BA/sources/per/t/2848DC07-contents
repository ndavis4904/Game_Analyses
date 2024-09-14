library(jsonlite)
library(rjson)
library(tidyr)
library(Matrix)
library(data.table)
library(mosaic)

#3000 cards in total - from the Lorcana API - Actually read 816 cards
lorc.dat_page1 <- fromJSON("./Lorcana/Data/LorcanaCards_100_1.json")

json_file <- "https://api.lorcana-api.com/cards/all?/pagesize=100&page=1"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

json_data <- NULL

for(i in 1:30) {      #read the data from the API
  json_file <- paste0("https://api.lorcana-api.com/cards/all?/pagesize=100&page=", i)
  json_data <- append(json_data, fromJSON(paste(readLines(json_file), collapse="")))
}


lorc <- Map(as.data.frame, json_data)
lorc.dat <- rbindlist(lorc, fill = TRUE)

lorc.dat_clean <- lorc.dat[,c(11,2,3,4,6,7,9,10,12,13,14,17,18,19,23)]

write.csv(lorc.dat_clean, "./Lorcana/Data/LorcanaData_API.csv")
write.csv(lorc.dat, "./Lorcana/Data/LorcanaData_API_Full.csv")


lorc.dat_clean <- read.csv("./Lorcana/Data/LorcanaData_API.csv")

#Descriptive Playing
##Set_Name
table(lorc.dat_clean$Set_Name)

##Abilities
table(lorc.dat_clean$Abilities)

##Color
table(lorc.dat_clean$Color)

##Franchise
table(lorc.dat_clean$Franchise)

##Cost
favstats(lorc.dat_clean$Cost)

##Inkable
table(lorc.dat_clean$Inkable)

##Type
table(lorc.dat_clean$Type)

##Lore
table(lorc.dat_clean$Lore)
favstats(lorc.dat_clean$Lore)

##Rarity
table(lorc.dat_clean$Rarity)

##Willpower
favstats(lorc.dat_clean$Willpower)

##Strength
favstats(lorc.dat_clean$Strength)

##Move Cost
favstats(lorc.dat_clean$Move_Cost)

#Cost to Lore Comparison
costlore.lm <- lm(Cost ~ Lore, data = lorc.dat_clean)

plot(Cost ~ Lore, data = lorc.dat_clean, pch = 16)
abline(costlore.lm, col = "skyblue")

##Creating a new variable to get proportions
lorc.dat_clean$CostLore_Prop <- lorc.dat_clean$Lore/lorc.dat_clean$Cost
favstats(lorc.dat_clean$CostLore_Prop)
good_cards <- lorc.dat_clean$Name[lorc.dat_clean$CostLore_Prop>1]
