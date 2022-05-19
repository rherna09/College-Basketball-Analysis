############################
#Rafael Hernandez
#Poster Project
#IST 719
##########################
#Loading data
#Data analysis
cbb <- read.csv("/Users/rafaelhernandez/Desktop/cbb.csv", header = TRUE, stringsAsFactors = FALSE)
str(cbb)
View(cbb)
library(dplyr)
#####################################
#CLEANING DATA
#changing column names
names(cbb) <- c('Team', 'Conf', 'G', 'W', 'ADJOE', 'ADJDE'
                , 'BARTHAG', 'EFG_O', 'EFG_D', 'TOR', 'TORD', 'ORB'
                , 'DRB', 'FTR', 'FTRD', '2P_O', '2P_D', '3P_O', '3P_D'
                , 'ADJ_T', 'WAB', 'POSTSEASON', 'SEED', 'YEAR')

View(cbb)

cbbdf <- as.data.frame(cbb)
#Removing first row
cbbdf = cbbdf[-1,]
View(cbbdf)

#Changing char to numeric
str(cbbdf)

cbbdf <- transform(cbbdf, G = as.numeric(G))
str(cbbdf)

cbbdf <- transform(cbbdf, W = as.numeric(W),
                   ADJOE = as.numeric(ADJOE),
                   ADJDE = as.numeric(ADJDE),
                   BARTHAG = as.numeric(BARTHAG),
                   EFG_O = as.numeric(EFG_O),
                   EFG_D = as.numeric(EFG_D),
                   TOR = as.numeric(TOR),
                   TORD = as.numeric(TORD),
                   ORB = as.numeric(ORB),
                   DRB = as.numeric(DRB),
                   FTR = as.numeric(FTR),
                   FTRD = as.numeric(FTRD),
                   X2P_O = as.numeric(X2P_O),
                   X2P_D = as.numeric(X2P_D),
                   X3P_O = as.numeric(X3P_O),
                   X3P_D = as.numeric(X3P_D),
                   ADJ_T = as.integer(ADJ_T))
str(cbbdf)

#Transforming YEAR colunn from char to integer


str(cbbdf)

#Transforming SEED column from char to factor

cbbdf$SEED <- as.factor(cbbdf$SEED)

class(cbbdf$SEED)
#Looking for missing values or na's in dataframe
any(is.na(cbbdf))


####################################################
#Exploring the data

library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

op <- par(mar = c(10, 4, 4, 2) + 0.1)
pairs(cbbdf[3:19], lower.panel = NULL)
str(cbbdf)
view(cbbdf)
#Density Graphs
dw <- density(cbbdf$W)
dadjoe <- density(cbbdf$ADJOE)
dadjde <- density(cbbdf$ADJDE)
defg_o <- density(cbbdf$EFG_D)
defg_d <- density(cbbdf$EFG_D)
dtor <- density(cbbdf$TOR)
dtord <- density(cbbdf$TORD)
d2p_o <- density(cbbdf$X2P_O)
d2p_d <- density(cbbdf$X2P_D)
d3p_o <- density(cbbdf$X3P_O)
d3p_d <- density(cbbdf$X3P_D)
dadjt <- density(cbbdf$ADJ_T)

#multi-dimensional charts
plot(cbbdf$W, cbbdf$ADJ_T, xlab = "Wins", ylab = "Adjusted Tempo", 
     main = "Adjusted Tempo vs Wins")
abline(lm(cbbdf$ADJ_T~cbbdf$W), col = "red", lwd = 3)

#correlation between offensive efficiency and wins
plot(cbbdf$W, cbbdf$ADJOE, xlab = "Wins", ylab = "Offensive Efficiency",
     main = "Wins vs Offensive Effeciency",pch = 5, col = "black")
abline(lm(cbbdf$ADJOE~cbbdf$W), col = "blue", lwd = 3)

plot(cbbdf$W, cbbdf$ADJDE, xlab = "Wins", ylab = "Defensive Efficiency",
     main = "Wins vs Defensive Effeciency",pch = 5, col = "black")
abline(lm(cbbdf$ADJDE~cbbdf$W), col = "blue", lwd = 3)

plot(cbbdf$W, cbbdf$ORB, xlab = "Wins", ylab = "Offensive Rebounds",
     main = "Wins vs Offensive Rebounds",pch = 5, col = "black")
abline(lm(cbbdf$ORB~cbbdf$W), col = "blue", lwd = 3)

plot(cbbdf$W, cbbdf$TOR, xlab = "Wins", ylab = "Turnover Rate",
     main = "Wins vs Turnover rate",pch = 5, col = "black")
abline(lm(cbbdf$TOR~cbbdf$W), col = "blue", lwd = 3)

ggplot(cbbdf) + aes(y = ADJDE, x = ADJOE, colour = W) + geom_point() + geom_smooth()

#offense density
par(mfrow = c(4,3))
plot(dadjoe, main = "Density of Adjusted Offensive Effeciency")
polygon(dadjoe, col = "lightblue", border = "red")
plot(defg_o, main = "Density of Effective Field Goal Percentage")
polygon(defg_o, col = "lightblue", border = "red")
plot(dtor, main = "Density of Turnover Rate")
polygon(dtor, col = "lightblue", border = "red")
polot(d2p_o, main = "Density of Two point Shooting Percentage")
polygon(d2p_o, col = "lightblue", border = "red")
plot(d3p_o, main = "Density of Three point Shooting Percentage")
polygon(d3p_o, col = "lightblue", border = "red")
plot(dadjt, main = "Density of Adjusted Tempo")
polygon(dadjt, col = "lightblue", border = "red")
hist(cbbdf$ADJOE, xlab = "Offensive Effeciency", ylab = "Frequency"
     , main="Offensive Effeciency Histogram", col = "lightblue")
hist(cbbdf$EFG_O, xlab = "Effective Field Goal Percentage", ylab = "Frequency"
     , main="Effective Field Goal Percentage Histogram", col = "lightblue")
hist(cbbdf$TOR, xlab = "Turnover Rate", ylab = "Frequency"
     , main="Turnover Rate Histogram", col = "lightblue")
hist(cbbdf$ADJOE, xlab = "Offensive Effeciency", ylab = "Frequency"
     , main="Offensive Effeciency Histogram", col = "lightblue")
hist(cbbdf$X2P_O, xlab = "2point shooting percentage", ylab = "Frequency"
     , main="2point shooting percentage Histogram", col = "lightblue")
hist(cbbdf$X3P_O, xlab = "3point shooting percentage", ylab = "Frequency"
     , main="3point shooting percentageHistogram", col = "lightblue")
hist(cbbdf$ORB, xlab = "Offensive Rebound", ylab = "Frequency"
     , main="Offensive Rebound", col = "lightblue")


#Isolating first few density graphs. 

par(mfrow = c(3,2))
plot(defg_o, main = "Density of Effective Field Goal Percentage")
polygon(defg_o, col = "lightblue", border = "red")
plot(dtor, main = "Density of Turnover Rate")
polygon(dtor, col = "lightblue", border = "red")
polot(d2p_o, main = "Density of Two point Shooting Percentage")
polygon(d2p_o, col = "lightblue", border = "red")
plot(d3p_o, main = "Density of Three point Shooting Percentage")
polygon(d3p_o, col = "lightblue", border = "red")
plot(dadjt, main = "Density of Adjusted Tempo")
polygon(dadjt, col = "lightblue", border = "red")
plot(dadjoe, main = "Density of Adjusted Offensive Effeciency")
polygon(dadjoe, col = "lightblue", border = "red")

cbbdf[cbbdf$Team=="Kansas",]
cbbdf[cbbdf$Team=="Kentucky",]
cbbdf[cbbdf$Team=="North Carolina",]
cbbdf[cbbdf$Team=="UCLA",]
cbbdf[cbbdf$Team=="Duke",]
cbbdf[cbbdf$Team=="Gonzaga",]
cbbdf[cbbdf$Team=="Villanova",]
cbbdf[cbbdf$POSTSEASON=="Champions",]

kansasdf <- cbbdf[c(23,23,47,2067,2070,2117,2449), ]
view(kansasdf)
kentuckydf <- cbbdf[c(6, 31, 32, 50, 1880, 2107, 2434), ]
view(kentuckydf)
northcarolinadf <- cbbdf[c(1, 10, 2047, 2122, 2141, 2400, 2407), ]
view(northcarolinadf)
ucladf <- cbbdf[c(1039, 1064, 2346, 2386, 2428, 2431, 2444),]
view(ucladf)
dukedf <- cbbdf[c(8, 19, 21, 40, 2042, 2308, 2402), ]
view(dukedf)
gonzagadf <- cbbdf[c(5, 33, 34, 2125, 2132, 2438, 2439),]
view(gonzagadf)
villanovadf <- cbbdf[c(10, 11, 2073, 2077, 2081, 2117, 2349), ]
view(villanovadf)
championsdf <- cbbdf[c(7, 8, 9, 10, 11, 12, 13), ]
view(championsdf)

bluebloods <- do.call("rbind", list(kansasdf, kentuckydf, northcarolinadf, ucladf, dukedf, gonzagadf, villanovadf))
view(bluebloods)

view(bluebloods)

#Comparing Traditional Blueblood Programs

b1 <- tapply(bluebloods$ADJOE, list(bluebloods$Team), mean)
barplot(b1, main = "Blueblood Offense Efficiency Mean")

b2 <- tapply(bluebloods$ADJDE, list(bluebloods$Team), mean)
barplot(b2, main = "Blublood Defense Effeciency Mean")

b3 <- tapply(bluebloods$W, list(bluebloods$Team), mean)
barplot(b3, main = "Average Wins Per Blue Blood")



championsyeardf <- championsdf[, c(1,5)]
View(championsyeardf)

barplot(championsyeardf)


#histograms
ggplot(cbbdf) + aes(y = Team, x = SEED) + geom_histogram()

col.vec <- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(cbbdf))
col.vec[cbbdf$ADJDE < 90] <- rgb(255,64,64, maxColorValue = 255)
plot(cbbdf$W, cbbdf$ADJDE, pch = 16, cex =1, col = col.vec)




#Championship comparisons
par(mfrow = c(2,2))


gplot <- ggplot(cbbdf) + aes(y = ADJDE, x = ADJOE, color = W) + xlab("Offensive Efficiency") + ylab("Defensive Efficiency") +
  ggtitle("Offensive and Defensive Efficiency Measured by Wins") + theme(plot.title = element_text(hjust =  0.7)) +  
   geom_point() + geom_smooth()
p + theme_classic()
gplot
c1 <- tapply(championsdf$ADJOE, list(championsdf$Team), mean)
barplot(c1, main = "Offensive Effeciency for Championship Teams")
c2 <- tapply(championsdf$W, list(championsdf$Team), mean)
barplot(c2, main = "Championship Team Wins")
c3 <- tapply(championsdf$ADJDE, list(championsdf$Team), mean)
barplot(c3, main = "Defensive Efficiency for Championship Teams")


#are there trends in basketball over the past decade

t1 <- aggregate(cbbdf$ADJOE, list(cbbdf$YEAR), mean)
plot(t1, main = "Offensive Efficiency 2013-2019", xlab = "Years", ylab = "Adjusted Offensive Effeciency")
par(mfrow = c(2,2))
t3 <- aggregate(cbbdf$ADJ_T, list(cbbdf$YEAR), mean)
plot(t3, type = "s", main = "Tempo of Game Play 2013-2019", xlab = "Years", ylab = "Tempo")
t4 <- aggregate(cbbdf$X2P_O, list(cbbdf$YEAR), mean)
plot(t4, type = "s", main = "2pt Shooting Percentage Trends 2013-2019", xlab = "Year", ylab = "2pt Shooting Percentage")
t5 <- aggregate(cbbdf$X3P_O, list(cbbdf$YEAR), mean)
plot(t5, type = "s", main = "3pt Shooting Percentage Trends 2013-2019", xlab = "Year", ylab = "3pt Shooting Percantage")
t6 <- aggregate(cbbdf$ORB, list(cbbdf$YEAR), mean)
plot(t6, type = "s", main = "Offensive Rebounding Trends 2013-2019", xlab = "Years", ylab = "Offensive Rebound Avg")


#defensive trends
t2 <- aggregate(cbbdf$ADJDE, list(cbbdf$YEAR), mean)
plot(t2, type = "b", main = "Defensive Efficiency 2013-2019", xlab = "Years", ylab = "Defensive Effeciency")
par(mfrow = c(2,2))
t7 <- aggregate(cbbdf$TORD, list(cbbdf$YEAR), mean)
plot(t7, main = "Defensive Turnover Rate 2013-2019", xlab = "Years", ylab = "Turnover Rate")
t8 <- aggregate(cbbdf$X2P_D, list(cbbdf$YEAR), mean)
plot(t8, main = "2pt Defense", xlab = "Years", ylab = "2pt shooting average allowed")
t9 <- aggregate(cbbdf$DRB, list(cbbdf$YEAR), mean)
plot(t9, main = "Defensive Rebound", xlab = "Years", ylab = "Defensive Rebounds")


#conference examination
view(cbbdf)

co<- tapply(cbbdf$W, list(cbbdf$Conf), mean)
barplot(co, color = "dodgerblue", las = 1)

co1 <- tapply(cbbdf$ADJOE, list(cbbdf$Conf), mean)
plot(co1, type = "h", xlab = "Conferences", ylab = "Offensive Effeciency", ylim = c(0, 130))
points(co1, pch=19, col="black")

co2 <- tapply(cbbdf$ADJDE, list(cbbdf$Conf), mean)
plot(co2, type = "b", xlab = "conferences", ylab = "Defensive Effeciency")



barplot()

