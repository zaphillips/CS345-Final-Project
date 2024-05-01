library(tidyverse)
library(cluster)
library(factoextra)
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

#Move Columns
receivingGrouped_stn <- receivingGrouped_stn %>%
  relocate(YardsPerReception, .before = Lng) %>%
  relocate(Yds, .before = YardsPerReception) %>%
  relocate(Receptions, .before = Yds)

#Prepare subsets of data (for each position)
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

#-------------------Predicting Position Based on Receiving Stats (2014-2023)---------------------------
#Seed
set.seed(1234)

#kmeans
receivingCluster <- kmeans(receivingGrouped_stn[, 8:14], center=4, nstart=25)
receivingCluster

#Compare with label
table(receivingCluster$cluster, receiving_label)

#Determine number of clusters
fviz_nbclust(receivingGrouped_stn[, 8:14], kmeans, method="wss")
fviz_nbclust(receivingGrouped_stn[, 8:14], kmeans, method="silhouette")

gap_stat <- clusGap(receivingGrouped_stn[, 8:14], FUN = kmeans, nstart=25, K.max = 10, B=50)
fviz_gap_stat(gap_stat)

#Final kmeans
km.res <- kmeans(receivingGrouped_stn[8:14], 4, nstart = 25)
km.res
fviz_cluster(km.res, data=receivingGrouped_stn[8:14])

#Create dataframe including predictions from model
positionPrediction <- receivingGrouped_stn
positionPrediction$cluster <- as.factor(km.res$cluster)

summary(receivingGrouped_stn)

positionPrediction$actual_position <- receiving_label

#Relocate columns
positionPrediction <- positionPrediction %>%
  relocate(actual_position, .before = Player) %>%
  relocate(cluster, .after = actual_position)

#Create confusion matrix and calculate accuracy of model
cfm <- table(positionPrediction$cluster, receiving_label)
cfm
acc <- sum(diag(cfm)) / sum(rowSums(cfm)) * 100
paste("Accuracy: ", round(acc, 2))

#-------------------Identifying Additional Clusters Based on Receiving Stats (2014-2023)---------------------------

# Expected Sub Positions:
# RB: Receiving, Hybrid, Power
# WR: Boundary, Move, Slot
# TE: Inline, Move
# FB: Receiving

#Create kmeans based on expected number of clusters (based on expected sub-positions)
receivingCluster2 <- kmeans(receivingGrouped_stn[, 8:14], center=9, nstart=25)
receivingCluster2

#Final kmeans
km.res2 <- kmeans(receivingGrouped_stn[8:14], 9, nstart = 25)
km.res2
fviz_cluster(km.res2, data=receivingGrouped_stn[8:14])

#Compare similar players (based on model)
positionPrediction2 <- receivingGrouped_stn
positionPrediction2$cluster <- as.factor(km.res2$cluster)

#Move cluster column and arrange data by cluster
positionPrediction2 <- positionPrediction2 %>%
  relocate(cluster, .after = Player) %>%
  arrange(cluster)

View(positionPrediction2)

#-------------------Identifying Sub-Positions Based on Receiving Stats (2014-2023)---------------------------

# Expected Sub Positions:
# RB: Receiving, Hybrid, Power
# WR: Boundary, Move, Slot
# TE: Inline, Move
# FB: Receiving

#RBs:
rbCluster <- kmeans(rb_stats[, 8:14], center=3, nstart=25)
rbCluster

fviz_nbclust(rb_stats[, 8:14], kmeans, method="wss")
fviz_nbclust(rb_stats[, 8:14], kmeans, method="silhouette")

gap_stat_rb <- clusGap(rb_stats[, 8:14], FUN = kmeans, nstart=25, K.max = 10, B=50)
fviz_gap_stat(gap_stat_rb)

km.res_rb <- kmeans(rb_stats[8:14], 3, nstart = 25)
km.res_rb
fviz_cluster(km.res_rb, data=rb_stats[8:14])

#Compare similar players (based on model)
positionPrediction_rb <- rb_stats
positionPrediction_rb$cluster <- as.factor(km.res_rb$cluster)

#Move cluster column and arrange data by cluster
positionPrediction_rb <- positionPrediction_rb %>%
  relocate(cluster, .after = Player) %>%
  arrange(cluster)

View(positionPrediction_rb)

#WRs:
wrCluster <- kmeans(wr_stats[, 8:14], center=3, nstart=25)
wrCluster

fviz_nbclust(wr_stats[, 8:14], kmeans, method="wss")
fviz_nbclust(wr_stats[, 8:14], kmeans, method="silhouette")

gap_stat_wr <- clusGap(wr_stats[, 8:14], FUN = kmeans, nstart=25, K.max = 10, B=50)
fviz_gap_stat(gap_stat_wr)

km.res_wr <- kmeans(wr_stats[8:14], 3, nstart = 25)
km.res_wr
fviz_cluster(km.res_wr, data=wr_stats[8:14])

#Compare similar players (based on model)
positionPrediction_wr <- wr_stats
positionPrediction_wr$cluster <- as.factor(km.res_wr$cluster)

#Move cluster column and arrange data by cluster
positionPrediction_wr <- positionPrediction_wr %>%
  relocate(cluster, .after = Player) %>%
  arrange(cluster)

View(positionPrediction_wr)

#TEs:
teCluster <- kmeans(te_stats[, 8:14], center=2, nstart=25)
teCluster

fviz_nbclust(te_stats[, 8:14], kmeans, method="wss")
fviz_nbclust(te_stats[, 8:14], kmeans, method="silhouette")

gap_stat_te <- clusGap(te_stats[, 8:14], FUN = kmeans, nstart=25, K.max = 10, B=50)
fviz_gap_stat(gap_stat_te)

km.res_te <- kmeans(te_stats[8:14], 2, nstart = 25)
km.res_te
fviz_cluster(km.res_te, data=te_stats[8:14])

#Compare similar players (based on model)
positionPrediction_te <- te_stats
positionPrediction_te$cluster <- as.factor(km.res_te$cluster)

#Move cluster column and arrange data by cluster
positionPrediction_te <- positionPrediction_te %>%
  relocate(cluster, .after = Player) %>%
  arrange(cluster)

View(positionPrediction_te)

#FBs:

#NOT ENOUGH DATA TO RUN ANALYSIS (Commented Out Code to Prevent Errors)

# fbCluster <- kmeans(fb_stats[, 8:14], center=2, nstart=25)
# fbCluster

# fviz_nbclust(fb_stats[, 8:14], kmeans, method="wss")
# fviz_nbclust(fb_stats[, 8:14], kmeans, method="silhouette")

# gap_stat_fb <- clusGap(fb_stats[, 8:14], FUN = kmeans, nstart=25, K.max = 10, B=50)
# fviz_gap_stat(gap_stat_fb)

# km.res_fb <- kmeans(fb_stats[8:14], 2, nstart = 25)
# km.res_fb
# fviz_cluster(km.res_fb, data=fb_stats[8:14])