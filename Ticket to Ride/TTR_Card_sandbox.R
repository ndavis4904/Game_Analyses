destinations <- read.csv("./Ticket to Ride/TTR_Card_Dest.csv")

library(mosaic)
library(pander)
library(ggplot2)

# Descriptive Stats
##Distinations

cities <- as.data.frame(cbind(c(destinations$From, destinations$To)))

table(cities$V1) %>% 
  pander()

Maj.Cit <- subset(cities, V1 == "Chicago" | V1 == "Dallas" | V1 == "Los Angeles"
                  | V1 == "Miami" | V1 == "New York" | V1 == "Seattle")
table(Maj.Cit$V1) %>% 
  pander()

Maj.dat <- data.frame("City" = c("Chicago", "Dallas", "Los Angeles", "Miami", "New York", "Seattle"),
                      "Points" = c(12, 10, 12, 8, 15, 8),
                      "Frequency" = c(11, 10, 10, 10, 11, 10))

####Major City Point Values

#Chicago: 12
#Dallas: 10
#Los Angeles: 12
#Miami: 8
#New York: 15
#Seattle: 8

## Colors
train.colors <- cbind(c(destinations$Color_1, destinations$Color_2, destinations$Color_3,
                        destinations$Color_4, destinations$Color_5), na.rm = TRUE)
table(train.colors) %>% 
  pander()



## Points to Number of Colors

dest.lm <- lm(Points ~ Number_Colors, data = destinations)
summary(dest.lm) %>% 
  pander()

plot(Points ~ Number_Colors, data = destinations, pch = 16)
abline(dest.lm, col = "skyblue")

### Destinations above the line
new.dat <- data.frame("Number_Colors" = destinations$Number_Colors)

destinations$Est.Points <- predict(dest.lm, newdata = new.dat)
destinations$Above.Line <- (destinations$Points - destinations$Est.Points)>0


ggplot(destinations, aes(x = Number_Colors, y = Points)) +
  geom_point() +
  geom_point(data = subset(destinations, Above.Line == TRUE), aes(x = Number_Colors, y = Points),
             colour = "red")

Cards.to.Watch <- subset(destinations, Above.Line == TRUE)

