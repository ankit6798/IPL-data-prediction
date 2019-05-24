setwd("/home/ankit/Desktop/Data_Analytics/ipldata")
matchdb <- read.csv("matches.csv")
head(matchdb)
tail(matchdb)

#Calculating which toss decision leads to better results

#How much of significance can a toss have in deciding the winner?
nrow(matchdb)

results <- matchdb[which(matchdb$winner != ""),]
#results
nrow(results)
#toss_deciders <- resultsdb[which(as.character(resultsdb$toss_winner) != as.character(resultsdb$winner)),]
#head(toss_deciders)
#tail(toss_deciders)
#levels(results$winner)
#check <- results[which(results$winner == ""),]
head(check)
#levels(results$toss_winner)
#check <- results[which(results$winner == ""),]
#head(check)
gt <- nrow(subset(results, as.character(toss_winner) == as.character(winner),select=id))
bt <- nrow(subset(results, as.character(toss_winner) != as.character(winner),select=id))
col1 <- as.data.frame(rbind(gt,bt))
col0 <- as.data.frame(c("Toss winning the match","Toss not winning the match"))
toss_an <- as.data.frame(cbind(col0,col1$V1))
names(toss_an) <- c("Toss affecting match","no of matches")
toss_an
pie(main = "Toss influencing win",labels = toss_an$`Toss affecting match`,toss_an$`no of matches`)
#only 51% of the toss cases have been called correctly

resultsdb <- as.character(resultsdb)

#levels(results$winner)
#library(plyr)
#count(results, toss_winner == winner)
?subset
#mydata[ which(mydata$gender=='F' 
              & mydata$age > 65), ]
# using subset function (part 2)
#newdata <- subset(mydata, sex=="m" & age > 25,
                  #select=weight:income)

#successfull chasing teams
library(plyr)
results <- as.data.frame(results)
count1 <- count(results,vars = c('winner'))

head(count1)
home_s <- results[which(resultsdb$team1 == resultsdb$winner),]
head(count1)
tail(count1)

?as.character
#successful chasing grounds
results$city <- gsub('Bengaluru','Bangalore',results$city)
updated <- results[which(results$season != 2009),]
View(updated)
nrow(updated)
first <- subset(updated, as.character(win_by_runs) != 0 ,select=c(id,city))
second <- subset(updated, as.character(win_by_wickets) != 0 ,select=c(id,city))
head(first)
#count based on city parameter
firstt <- count(first,vars = c('city'))
secondd <- count(second,vars = c('city'))
#Taking cities only where significant matches have been played
firstt <- firstt[which(firstt$freq >10),]
secondd <- secondd[which(secondd$freq>10),]

head(firstt)
head(secondd)
city_eval <- cbind(firstt$city,firstt$freq,secondd$freq)
chasing <- as.data.frame(secondd$freq*100/(secondd$freq+firstt$freq))
city_eval <- as.data.frame(cbind(city_eval,round(chasing,1)))
names(city_eval) <- c("City","Batting_winners","Chasing_winners","chasing_win")
#population[order(population$age),]
city_eval <- city_eval[order(city_eval$chasing_win,decreasing = TRUE),]
?order
head(city_eval)
library(ggplot2)
p <-ggplot(city_eval, aes(city_eval$City,(city_eval$chasing_win)))
?ggplot

p +geom_bar(stat = "identity")+ xlab("City") + ylab("Win_percentage") +ggtitle("Best cities to chase in ")
?geom_bar

data <- as.matrix(mtcars)

#Lineplot
sixteam <- matchdb[,c(1,2,11)]
head(sixteam)
countf <- count(sixteam,vars = c("winner","season") )
countf
cleaned <- countf[c(3:10,17:24,30:37,42:49,53:60,67:74,78:85),]
head(cleaned)
#cleaned$freq <- cumsum(cleaned$freq)
cleaned

?transform
head(cleaned)
library(ggplot2)
ggplot(data=cleaned, aes(x=cleaned$season, y=cleaned$freq, group =cleaned$winner, colour = as.factor(cleaned$winner)))+geom_line() 
#

#winmargins
head(matchdb)
close1 <- subset(updated, as.character(win_by_runs) != 0 ,select=c(id,win_by_runs))
head(close)
#close1 <- close[which(close$win_by_runs < 6),]
#close1
#close1 <- count(close1,vars = close1$id)
#close1
?cut
margins <- cut(close1$win_by_runs,breaks = c(0,10,20,30,40,60,180),labels = c("0-10","10-20","20-30","30-40","40-60","60+"))
head(margins)
head(close1$win_by_runs)
filtered <- as.data.frame(cbind(close1,margins),)
head(filtered)
magnitude <- count(filtered, vars = c("margins"))
head(magnitude)
#bar.plot(magnitude)
#?bar
barplot(magnitude$freq, main="Victory margins", xlab="Runs lost by",names.arg = magnitude$margins)
#?barplot
