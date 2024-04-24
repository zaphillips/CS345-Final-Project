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


#Total Number of Players at Each Position (2014-2023)
ggplot(receiving, aes(x = Position)) +
  geom_bar() +
  labs(x = "Total Number of Players at Each Position (2014-2023)")

#Number of Targets Per Player Per Season
ggplot(receiving, aes(x = Targets)) +
  geom_histogram(position="dodge") + 
  labs(x = "Number of Targets Per Player Per Season")

ggplot(receiving, aes(x = Targets)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(x = "Number of Targets Per Player Per Season")

#Number of Receptions Per Player Per Season
ggplot(receiving, aes(x = Receptions)) +
  geom_histogram(position="dodge") + 
  labs(x = "Number of Receptions Per Player Per Season")

ggplot(receiving, aes(x = Receptions)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(x = "Number of Receptions Per Player Per Season")

#Number of Yards Per Player Per Season
ggplot(receiving, aes(x = Yds)) +
  geom_histogram(position="dodge") + 
  labs(x = "Number of Yards Per Player Per Season")

ggplot(receiving, aes(x = Yds)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(x = "Number of Yards Per Player Per Season")

#Average Yards Per Target Per Player
ggplot(receiving, aes(x = YardsPerTarget)) +
  geom_histogram(position="dodge") + 
  labs(x = "Average Yards Per Target Per Player")

ggplot(receiving, aes(x = YardsPerTarget)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(x = "Average Yards Per Target Per Player")

#Average Yards Per Reception Per Player
ggplot(receiving, aes(x = YardsPerReception)) +
  geom_histogram(position="dodge") + 
  labs(x = "Average Yards Per Reception Per Player")

ggplot(receiving, aes(x = YardsPerReception)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(x = "Average Yards Per Reception Per Player")

#Average Number of Receptions Per Player Per Game
ggplot(receiving, aes(x = ReceptionsPerGame)) +
  geom_histogram(position="dodge") + 
  labs(x = "Average Number of Receptions Per Player Per Game")

ggplot(receiving, aes(x = ReceptionsPerGame)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(x = "Average Number of Receptions Per Player Per Game")

#Bivariate Exploration (RAW Data)

#Targets vs. Yards
ggplot(receiving, aes(x=Targets, y=Yds)) +
  geom_point()

cov(receiving$Targets, receiving$Yds)
cor(receiving$Targets, receiving$Yds)

#Targets vs. Receptions
ggplot(receiving, aes(x=Targets, y=Receptions)) +
  geom_point()

cov(receiving$Targets, receiving$Receptions)
cor(receiving$Targets, receiving$Receptions)

#Receptions vs. Yards
ggplot(receiving, aes(x=Receptions, y=Yds)) +
  geom_point()

cov(receiving$Receptions, receiving$Yds)
cor(receiving$Receptions, receiving$Yds)

#Receptions vs. Touchdowns
ggplot(receiving, aes(x=Receptions, y=TD)) +
  geom_point()

cov(receiving$Receptions, receiving$TD)
cor(receiving$Receptions, receiving$TD)

#Number of Players for Each Position for Each Year
ggplot(receiving, aes(x = Position, fill = Year)) +
  geom_bar(position="dodge2")

#Multivariate Exploration (RAW Data)

#Targets per Position vs. Yards
ggplot(receiving, aes(x=Targets, y=Yds, color=Position)) +
  geom_point()

#Targets per Position vs. Receptions
ggplot(receiving, aes(x=Targets, y=Receptions, color=Position)) +
  geom_point()

#Receptions per Position vs. Yards
ggplot(receiving, aes(x=Receptions, y=Yds, color=Position)) +
  geom_point()

#Receptions per Position vs. Touchdowns
ggplot(receiving, aes(x=Receptions, y=TD, color=Position)) +
  geom_point()

#Univariate Exploration (Standardized Data):


#Number of Players at Each Position
ggplot(receiving_stn, aes(x = Position)) +
  geom_bar() +
  labs(x = "Total Number of Players at Each Position (2014-2023)")

#Number of Targets
ggplot(receiving_stn, aes(x = Targets)) +
  geom_histogram(position="dodge")

ggplot(receiving_stn, aes(x = Targets)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Number of Receptions
ggplot(receiving_stn, aes(x = Receptions)) +
  geom_histogram(position="dodge") +
  xlab("")

ggplot(receiving_stn, aes(x = Receptions)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Number of Yards
ggplot(receiving_stn, aes(x = Yds)) +
  geom_histogram(position="dodge")

ggplot(receiving_stn, aes(x = Yds)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Yards Per Reception
ggplot(receiving_stn, aes(x = YardsPerReception)) +
  geom_histogram(position="dodge")

ggplot(receiving_stn, aes(x = YardsPerReception)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Yards Per Target
ggplot(receiving_stn, aes(x = YardsPerTarget)) +
  geom_histogram(position="dodge")

ggplot(receiving_stn, aes(x = YardsPerTarget)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Receptions Per Game
ggplot(receiving_stn, aes(x = ReceptionsPerGame)) +
  geom_histogram(position="dodge")

ggplot(receiving_stn, aes(x = ReceptionsPerGame)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#Bivariate Exploration (Standardized Data)

#Targets vs. Yards
ggplot(receiving_stn, aes(x=Targets, y=Yds)) +
  geom_point()

cov(receiving_stn$Targets, receiving_stn$Yds)
cor(receiving_stn$Targets, receiving_stn$Yds)

#Targets vs. Receptions
ggplot(receiving_stn, aes(x=Targets, y=Receptions)) +
  geom_point()

cov(receiving_stn$Targets, receiving_stn$Receptions)
cor(receiving_stn$Targets, receiving_stn$Receptions)

#Receptions vs. Yards
ggplot(receiving_stn, aes(x=Receptions, y=Yds)) +
  geom_point()

cov(receiving_stn$Receptions, receiving_stn$Yds)
cor(receiving_stn$Receptions, receiving_stn$Yds)

#Receptions vs. Touchdowns
ggplot(receiving_stn, aes(x=Receptions, y=TD)) +
  geom_point()

cov(receiving_stn$Receptions, receiving_stn$TD)
cor(receiving_stn$Receptions, receiving_stn$TD)

#Number of Players for Each Position for Each Year
ggplot(receiving_stn, aes(x = Position, fill = Year)) +
  geom_bar(position="dodge2")

#Multivariate Exploration (Standardized Data)

#Targets per Position vs. Yards
ggplot(receiving_stn, aes(x=Targets, y=Yds, color=Position)) +
  geom_point()

#Targets per Position vs. Receptions
ggplot(receiving_stn, aes(x=Targets, y=Receptions, color=Position)) +
  geom_point()

#Receptions per Position vs. Yards
ggplot(receiving_stn, aes(x=Receptions, y=Yds, color=Position)) +
  geom_point()

#Receptions per Position vs. Touchdowns
ggplot(receiving_stn, aes(x=Receptions, y=TD, color=Position)) +
  geom_point()