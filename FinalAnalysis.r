
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(ggplot2)

#----------------------------Prepare Data------------------------------------

receiving <- read_csv("NFLReceivingALL.csv")
summary(receiving)

#Change Taysom Hill's Position to TE
receiving$Pos[receiving$Player == "Taysom Hill"] <- "TE"

#Remove QBs
receiving <- receiving %>%
  filter(Pos != 'QB')

#Filter for 20 <= targets
receiving <- receiving %>%
  filter(Tgt >= 20)

#Check for NA values
summary(receiving)

#Rename variables
receiving  <- receiving %>%
  rename("Team"="Tm") %>%
  rename("Position"="Pos") %>%
  rename("Games_played"="G") %>%
  rename("Games_started"="GS") %>%
  rename("Targets"="Tgt") %>%
  rename("Receptions"="Rec") %>%
  rename("CatchPerc"="Ctch%") %>%
  rename("YardsPerReception"="Y/R") %>%
  rename("First_downs"="1D") %>%
  rename("SuccessPerc"="Succ%") %>%
  rename("YardsPerTarget"="Y/Tgt") %>%
  rename("ReceptionsPerGame"="R/G") %>%
  rename("YardsPerGame"="Y/G")

#Convert year, position, & team to categorical variables
receiving$Year <- as.factor(receiving$Year)
receiving$Position <- as.factor(receiving$Position)
receiving$Team <- as.factor(receiving$Team)

#Create new DataFrame
receivingGrouped <- receiving

#Remove Unnecessary Columns
receivingGrouped <- receivingGrouped[-c(1, 4, 6:7)]

#Group by player and average stats from each year
receivingGrouped <- receivingGrouped %>%
  group_by(Player) %>%
  mutate(Targets = mean(Targets)) %>%
  mutate(Receptions = mean(Receptions)) %>%
  mutate(CatchPerc = mean(CatchPerc)) %>%
  mutate(Yds = mean(Yds)) %>%
  mutate(YardsPerReception = mean(YardsPerReception)) %>%
  mutate(TD = mean(TD)) %>%
  mutate(First_downs = mean(First_downs)) %>%
  mutate(SuccessPerc = mean(SuccessPerc)) %>%
  mutate(Lng = mean(Lng)) %>%
  mutate(YardsPerTarget = mean(YardsPerTarget)) %>%
  mutate(ReceptionsPerGame = mean(ReceptionsPerGame)) %>%
  mutate(YardsPerGame = mean(YardsPerGame)) %>%
  mutate(Fmb = mean(Fmb)) %>%
  distinct(Player, .keep_all = TRUE)

#Scale Data
receivingGrouped_stn <- receivingGrouped
receivingGrouped_stn[, 4:16] <- scale(receivingGrouped_stn[, 4:16])

#Prepare subsets of data
wr_stats <- receivingGrouped_stn %>%
  filter(Position == "WR") %>%
  select(-Position)

rb_stats <- receivingGrouped_stn %>%
  filter(Position == "RB") %>%
  select(-Position)

fb_stats <- receivingGrouped_stn %>%
  filter(Position == "FB") %>%
  select(-Position)

te_stats <- receivingGrouped_stn %>%
  filter(Position == "TE") %>%
  select(-Position)

#Create label dataframe
receiving_label <- receivingGrouped_stn$Position
receivingGrouped_stn <- receivingGrouped_stn %>%
  select(-Position)

#Move Columns
receivingGrouped_stn <- receivingGrouped_stn %>%
  relocate(YardsPerReception, .before = Lng) %>%
  relocate(Yds, .before = YardsPerReception) %>%
  relocate(Receptions, .before = Yds)

#-------------------Predicting Position Based on Receiving Stats (2014-2023)---------------------------
set.seed(1234)

receivingCluster <- kmeans(receivingGrouped_stn[, 8:14], center=4, nstart=25)
receivingCluster

table(receivingCluster$cluster, receiving_label)

fviz_nbclust(receivingGrouped_stn[, 8:14], kmeans, method="wss")
fviz_nbclust(receivingGrouped_stn[, 8:14], kmeans, method="silhouette")

gap_stat <- clusGap(receivingGrouped_stn[, 8:14], FUN = kmeans, nstart=25, K.max = 10, B=50)
fviz_gap_stat(gap_stat)

km.res <- kmeans(receivingGrouped_stn[8:14], 4, nstart = 25)
km.res
fviz_cluster(km.res, data=receivingGrouped_stn[8:14])

positionPrediction <- receivingGrouped_stn
positionPrediction$cluster <- as.factor(km.res$cluster)

summary(receivingGrouped_stn)

positionPrediction$actual_position <- receiving_label

positionPrediction <- positionPrediction %>%
  relocate(actual_position, .before = Player) %>%
  relocate(cluster, .after = actual_position)

cfm <- table(positionPrediction$cluster, receiving_label)
cfm
acc <- sum(diag(cfm)) / sum(rowSums(cfm)) * 100
paste("Accuracy: ", round(acc, 2))