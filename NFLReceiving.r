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

#Create standardized dataframe
receiving_stn <- receiving
receiving_stn[6:20] <- as.data.frame(scale(receiving_stn[6:20]))

#Create visualizations
#Univariate Exploration (RAW Data):


#Number of Players at Each Position
ggplot(receiving, aes(x = Position, fill = Position)) +
  geom_bar() +
  labs(x = "Number of Players at Each Position (2014-2023)")

#Number of Targets
ggplot(receiving, aes(x = Targets, fill=Position)) +
  geom_histogram(position="dodge")

ggplot(receiving, aes(y = Targets)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Number of Receptions
ggplot(receiving, aes(x = Receptions, fill=Position)) +
  geom_histogram(position="dodge") +
  xlab("")

ggplot(receiving, aes(y = Receptions)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Number of Yards
ggplot(receiving, aes(x = Yds, fill=Position)) +
  geom_histogram(position="dodge")

ggplot(receiving, aes(y = Yds)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Yards Per Reception
ggplot(receiving, aes(x = YardsPerReception, fill=Position)) +
  geom_histogram(position="dodge")

ggplot(receiving, aes(y = YardsPerReception)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Yards Per Target
ggplot(receiving, aes(x = YardsPerTarget, fill=Position)) +
  geom_histogram(position="dodge")

ggplot(receiving, aes(y = YardsPerTarget)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Receptions Per Game
ggplot(receiving, aes(x = ReceptionsPerGame, fill=Position)) +
  geom_histogram(position="dodge")

ggplot(receiving, aes(y = ReceptionsPerGame)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Bivariate Exploration (RAW Data)

#Targets vs. Yards
ggplot(receiving, aes(x=Targets, y=Yds, color=Position)) +
  geom_point()

cov(receiving$Targets, receiving$Yds)
cor(receiving$Targets, receiving$Yds)

#Targets vs. Receptions
ggplot(receiving, aes(x=Targets, y=Receptions, color=Position)) +
  geom_point()

cov(receiving$Targets, receiving$Receptions)
cor(receiving$Targets, receiving$Receptions)

#Receptions vs. Yards
ggplot(receiving, aes(x=Receptions, y=Yds, color=Position)) +
  geom_point()

cov(receiving$Receptions, receiving$Yds)
cor(receiving$Receptions, receiving$Yds)

#Number Players for Each Position vs. Year
ggplot(receiving, aes(x = Position, fill = Year)) +
  geom_bar(position="dodge2")
  


# by_position <- receiving

# by_position <- by_position %>%
#   group_by(Position) %>%
#   summarise(avgypr=mean(YardsPerReception))
