library("tidyverse")
library("ggplot2")

receiving <- read_csv("NFLReceivingALL.csv")
summary(receiving)


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
  rename("Catch%"="Ctch%") %>%
  rename("YardsPerReception"="Y/R") %>%
  rename("First_downs"="1D") %>%
  rename("Success%"="Succ%") %>%
  rename("YardsPerTarget"="Y/Tgt") %>%
  rename("ReceptionsPerGame"="R/G") %>%
  rename("YardsPerGame"="Y/G")

#Convert year, position, & team to categorical variables
receiving$Year <- as.factor(receiving$Year)
receiving$Position <- as.factor(receiving$Position)
receiving$Team <- as.factor(receiving$Team)

#Create standardized dataframe
receiving_stn <- receiving
receiving_stn[6:20] <- as.data.frame(scale(receiving_stn[6:20]))

#Create visualizations
ggplot(receiving, aes(x=Targets, y=Yds, shape=Position, color=Position)) +
  geom_point() +
  scale_color_manual(name = "Position",
                     values = c("FB" = "Red", "WR" = "Purple", "RB" = "Dark Blue", "TE" = "Orange", "QB" = "Green"))


ggplot(receiving, aes(x=Targets, fill=Position)) +
  geom_histogram(position="dodge") +
  scale_fill_manual(name = "Position",
                     values = c("FB" = "Red", "WR" = "Purple", "RB" = "Dark Blue", "TE" = "Orange", "QB" = "Green"))

by_position <- receiving

by_position <- by_position %>%
  group_by(Position) %>%
  summarise(avgypr=mean(YardsPerReception))
