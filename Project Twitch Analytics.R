library(lubridate)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

Twitch_dataset <- read_csv("Twitch_game_data.csv")
Game2 <- read.csv(file = "Game2.csv", sep = ";")


#Dropping NA value
sum(is.na(Twitch_dataset))
is.na(Twitch_dataset)

TwitchdatasetwithoutNA <- Twitch_dataset %>% na.omit()
sum(is.na(TwitchdatasetwithoutNA))
print(TwitchdatasetwithoutNA)

#
#Making the dates in Date format
Month <- as.numeric(TwitchdatasetwithoutNA$Month)
print(Month)
Years <- as.numeric(TwitchdatasetwithoutNA$Year)
print(Years)
Dates <- make_date(year = Years, month = Month)
print(Dates)
#

filter(TwitchDataBruh, Year == 2022)

# Tidy 2022 and 2023 Datas
######################################################################
################ code gaet ###########################################
######################################################################


#TwitchDataBruh$Hours_watched
#tail(TwitchDataBruh$Hours_watched)

#a = gsub("\\(.*","",TwitchDataBruh$Hours_watched)

#tail(a)

#filter(a, Year == "2022")

##############################################################

TwitchDataBruh <- TwitchdatasetwithoutNA %>%
  mutate_at("Hours_Streamed", str_replace," hours","")

BruhHoursWatched <- gsub("\\(.*","",TwitchdatasetwithoutNA$Hours_watched)
BruhHoursStreamed <- gsub("\\(.*","",TwitchDataBruh$Hours_Streamed)
BruhPeakViewers <- gsub("\\(.*","",TwitchdatasetwithoutNA$Peak_viewers)
BruhPeakChannels <- gsub("\\(.*","",TwitchdatasetwithoutNA$Peak_channels)
BruhStreamers <- gsub("\\(.*","",TwitchdatasetwithoutNA$Streamers)
BruhAvgViewers <- gsub("\\(.*","",TwitchdatasetwithoutNA$Avg_viewers)
BruhAvgChannels <- gsub("\\(.*","",TwitchdatasetwithoutNA$Avg_channels)
BruhAvgViewerRatio <- gsub("\\(.*","",TwitchdatasetwithoutNA$Avg_viewer_ratio)

BruhHoursStreamed

# Transforming as numrerical
TidyHoursWatched <-  as.numeric(BruhHoursWatched)
TidyHoursWatched <- replace_na(TidyHoursWatched, 0)

TidyHoursStreamed <- as.numeric(BruhHoursStreamed)
TidyHoursStreamed <- replace_na(TidyHoursStreamed, 0)

TidyPeakViewers <-  as.numeric(BruhPeakViewers)
TidyPeakViewers <- replace_na(TidyPeakViewers, 0)

TidyPeakChannels <-  as.numeric(BruhPeakChannels)
TidyPeakChannels <- replace_na(TidyPeakChannels, 0)

TidyStreamers <-  as.numeric(BruhStreamers)
TidyStreamers <- replace_na(TidyStreamers, 0)

TidyAvgViewers <-  as.numeric(BruhAvgViewers)
TidyAvgViewers <- replace_na(TidyAvgViewers, 0)

TidyAvgChannels <-  as.numeric(BruhAvgChannels)
TidyAvgChannels <- replace_na(TidyAvgChannels, 0)

TidyAvgViewerRatio <-  as.numeric(BruhAvgViewerRatio)
TidyAvgViewerRatio <- replace_na(TidyAvgViewerRatio, 0)

TidyRank <- as.numeric(TwitchDataBruh$Rank)

#Creating the Tidy Dataset

TidyTwitchAnalDataset <- tibble(
  TidyRank,
  TwitchdatasetwithoutNA$Game,
  Dates,
  TidyHoursWatched,
  TidyHoursStreamed,
  TidyPeakViewers,
  TidyPeakChannels,
  TidyStreamers,
  TidyAvgViewers,
  TidyAvgChannels,
  TidyAvgViewerRatio
  
)
print(TidyTwitchAnalDataset)

#Renaming the Columns

colnames(TidyTwitchAnalDataset)

colnames(TidyTwitchAnalDataset) <- c(
  "Rank",
  "Games",
  "Date",
  "Hours_Watched",
  "Hours_Streamed",
  "Peak_Viewers",
  "Peak_Channels",
  "Streamers_Number",
  "Average_Viewers",
  "Average_Channels",
  "Average_Viewer_Ratio"
)

TidyTwitchAnalDataset

TidyDatanal <- merge(TidyTwitchAnalDataset, Game2, by = 'Games')
TidyDataAnalFinal <- TidyDatanal[order(TidyDatanal$Date, TidyDatanal$Rank),]
TidyDataAnalFinal

TidyDataAnalFinal <- TidyDataAnalFinal %>%
  filter(!grepl("REMOVE",Category)) %>%
  filter(!grepl("REMOOVE",Category)) %>%
  mutate(Type = if_else(Type == "multiplayer", "Multiplayer", Type)) %>%
  mutate(Type = if_else(Type == "Both", "Co-op", Type)) %>%
  mutate(Games = if_else(Games == "Escape From Tarkov", "Escape from Tarkov", Games)) %>%
  mutate(Category = if_else(Category == "Battle Royale", "Battle-Royale", Category)) %>%
  mutate(Category = if_else(Category == "FUN", "Fun", Category)) %>%
  mutate(Category = if_else(Category == "Shoot'em up", "Shoot 'em up", Category)) %>%
  mutate(Category = if_else(Category == "Point&click", "Point&Click", Category)) %>%
  mutate(Category = if_else(Category == "Strate", "Strategy", Category)) %>%
  mutate(Category = if_else(Category == "Beat'em all", "Beat 'em all", Category))


save(TidyDataAnalFinal, file = "TidyTwitchAnalDataset.RData")

print(TidyDataAnalFinal)
tail(TidyDataAnalFinal)

