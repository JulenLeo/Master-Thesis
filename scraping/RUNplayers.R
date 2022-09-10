# Packages

pacman::p_load(rvest, stringr, dplyr, purrr, doParallel, foreach, xml2, tibble)


## 1. Players ----

source("../scraping/getplayers.R")

## LaLiga - Spain

cl <- makeCluster(7)
registerDoParallel(cl)
system.time({
  players_big_esp_df <- foreach(i = 1:nrow(club_esp_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
    getplayers(club_esp_df$club_name[[i]], club_esp_df$club_id[[i]], club_esp_df$year[[i]])
  }
})
stopCluster(cl)

players_esp_df <- players_big_esp_df[!duplicated(players_big_esp_df$player_id), c("player_name", "player_id")]


## PremierLeague - England

cl <- makeCluster(7)
registerDoParallel(cl)
players_big_premier_df <- foreach(i = 1:nrow(club_premier_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getplayers(club_premier_df$club_name[[i]], club_premier_df$club_id[[i]], club_premier_df$year[[i]])
}
stopCluster(cl)

players_premier_df <- players_big_premier_df[!duplicated(players_big_premier_df$player_id), c("player_name", "player_id")]

## Bundesliga - Germany

cl <- makeCluster(7)
registerDoParallel(cl)
players_big_bundesliga_df <- foreach(i = 1:nrow(club_bundesliga_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getplayers(club_bundesliga_df$club_name[[i]], club_bundesliga_df$club_id[[i]], club_bundesliga_df$year[[i]])
}
stopCluster(cl)

players_bundesliga_df <- players_big_bundesliga_df[!duplicated(players_big_bundesliga_df$player_id), c("player_name", "player_id")]


## SerieA - Italy

cl <- makeCluster(7)
registerDoParallel(cl)
players_big_seriesa_df <- foreach(i = 1:nrow(club_seriesa_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getplayers(club_seriesa_df$club_name[[i]], club_seriesa_df$club_id[[i]], club_seriesa_df$year[[i]])
}
stopCluster(cl)

players_seriesa_df <- players_big_seriesa_df[!duplicated(players_big_seriesa_df$player_id), c("player_name", "player_id")]


## Ligue1 - France

cl <- makeCluster(7)
registerDoParallel(cl)
players_big_ligue_df <- foreach(i = 1:nrow(club_ligue_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getplayers(club_ligue_df$club_name[[i]], club_ligue_df$club_id[[i]], club_ligue_df$year[[i]])
}
stopCluster(cl)

players_ligue_df <- players_big_ligue_df[!duplicated(players_big_ligue_df$player_id), c("player_name", "player_id")]


## 2. Players exposure ----

source("../scraping/getplayersexposure.R")


## LaLiga - Spain

cl <- makeCluster(7)
registerDoParallel(cl)
system.time({
  players_exposure_esp_df <- foreach(i=1:nrow(club_esp_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
    players_exposure(club_esp_df$club_name[[i]], club_esp_df$club_id[[i]], club_esp_df$year[[i]])
  }
})
stopCluster(cl)

players_big_esp_df <- left_join(players_big_esp_df, players_exposure_esp_df, by = c("player_name", "club_name", "year"))


## PremierLeague - England

cl <- makeCluster(7)
registerDoParallel(cl)
players_exposure_premier_df <- foreach(i=1:nrow(club_premier_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  players_exposure(club_premier_df$club_name[[i]], club_premier_df$club_id[[i]], club_premier_df$year[[i]])
}
stopCluster(cl)

players_big_premier_df <- left_join(players_big_premier_df, players_exposure_premier_df, by = c("player_name", "club_name", "year"))


## Bundesliga - Germany

cl <- makeCluster(7)
registerDoParallel(cl)
players_exposure_bundesliga_df <- foreach(i=1:nrow(club_bundesliga_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  players_exposure(club_bundesliga_df$club_name[[i]], club_bundesliga_df$club_id[[i]], club_bundesliga_df$year[[i]])
}
stopCluster(cl)

players_big_bundesliga_df <- left_join(players_big_bundesliga_df, players_exposure_bundesliga_df, by = c("player_name", "club_name", "year"))


## SerieA - Italy

cl <- makeCluster(7)
registerDoParallel(cl)
players_exposure_seriesa_df <- foreach(i=1:nrow(club_seriesa_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  players_exposure(club_seriesa_df$club_name[[i]], club_seriesa_df$club_id[[i]], club_seriesa_df$year[[i]])
}
stopCluster(cl)

players_big_seriesa_df <- left_join(players_big_seriesa_df, players_exposure_seriesa_df, by = c("player_name", "club_name", "year"))


## Ligue1 - France

cl <- makeCluster(7)
registerDoParallel(cl)
players_exposure_ligue_df <- foreach(i=1:nrow(club_ligue_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  players_exposure(club_ligue_df$club_name[[i]], club_ligue_df$club_id[[i]], club_ligue_df$year[[i]])
}
stopCluster(cl)

players_big_ligue_df <- left_join(players_big_ligue_df, players_exposure_ligue_df, by = c("player_name", "club_name", "year"))


save(players_big_bundesliga_df, players_bundesliga_df, players_exposure_bundesliga_df, 
     players_big_esp_df, players_esp_df, players_exposure_esp_df,
     players_big_premier_df, players_premier_df, players_exposure_premier_df,
     players_big_seriesa_df, players_seriesa_df, players_exposure_seriesa_df,
     players_big_ligue_df, players_ligue_df, players_exposure_ligue_df,
     file = "../data/players.RData")


## 3. More players info ----

source("../scraping/getplayersinfo.R")

## LaLiga - Spain

cl <- makeCluster(7)
registerDoParallel(cl)
system.time({
  info_esp_df <- foreach(i=1:nrow(players_esp_df), .combine = "rbind", .packages = c("dplyr", "rvest")) %dopar% {
    getplayersinfo(players_esp_df$player_name[[i]], players_esp_df$player_id[[i]])
  }
})
stopCluster(cl)

players_esp_df <- info_esp_df
players_esp_df$foot[which(players_esp_df$foot == "N/A")] <- NA
players_esp_df$foot <- as.factor(players_esp_df$foot)

players_esp_df$age <- as.numeric(players_esp_df$age)

players_esp_df$citizenship <- as.character(players_esp_df$citizenship)

players_esp_df$height[which(players_esp_df$height == "N/A")] <- NA
players_esp_df$height <- substr(players_esp_df$height, 1, 4) 
players_esp_df$height <- sub(pattern = ",", x = players_esp_df$height, ".")
players_esp_df$height <- as.numeric(players_esp_df$height)

players_esp_df$birth <- sub(pattern = "Happy Birthday", x = players_esp_df$birth, "")
players_esp_df$birth[which(players_esp_df$birth == "N/A")] <- NA

Sys.setlocale("LC_TIME", "English")
players_esp_df$birth <- as.Date(players_esp_df$birth, format = "%b %d, %Y")

## PremierLeague - England

cl <- makeCluster(7)
registerDoParallel(cl, cores = 7)
system.time({
  info_premier_df <- foreach(i=1:nrow(players_premier_df), .combine = "rbind", .packages = c("dplyr", "rvest")) %dopar% {
    getplayersinfo(players_premier_df$player_name[[i]], players_premier_df$player_id[[i]])
  }
})
stopCluster(cl)

players_premier_df <- info_premier_df
players_premier_df$foot[which(players_premier_df$foot == "N/A")] <- NA
players_premier_df$foot <- factor(players_premier_df$foot)

players_premier_df$age <- as.numeric(players_premier_df$age)

players_premier_df$citizenship <- as.character(players_premier_df$citizenship)

players_premier_df$height[which(players_premier_df$height == "N/A")] <- NA
players_premier_df$height <- substr(players_premier_df$height, 1, 4) 
players_premier_df$height <- sub(pattern = ",", x = players_premier_df$height, ".")
players_premier_df$height <- as.numeric(players_premier_df$height)

players_premier_df$birth <- sub(pattern = "Happy Birthday", x = players_premier_df$birth, "")
players_premier_df$birth[which(players_premier_df$birth == "N/A")] <- NA

Sys.setlocale("LC_TIME", "English")
players_premier_df$birth <- as.Date(players_premier_df$birth, format = "%b %d, %Y")

## Bundesliga - Germany

cl <- makeCluster(7)
registerDoParallel(cl, cores = 7)
system.time({
  info_bundesliga_df <- foreach(i=1:nrow(players_bundesliga_df), .combine = "rbind", .packages = c("dplyr", "rvest")) %dopar% {
    getplayersinfo(players_bundesliga_df$player_name[[i]], players_bundesliga_df$player_id[[i]])
  }
})
stopCluster(cl)

players_bundesliga_df <- info_bundesliga_df
players_bundesliga_df$foot[which(players_bundesliga_df$foot == "N/A")] <- NA
players_bundesliga_df$foot <- factor(players_bundesliga_df$foot)

players_bundesliga_df$age <- as.numeric(players_bundesliga_df$age)

players_bundesliga_df$citizenship <- as.character(players_bundesliga_df$citizenship)

players_bundesliga_df$height[which(players_bundesliga_df$height == "N/A")] <- NA
players_bundesliga_df$height <- substr(players_bundesliga_df$height, 1, 4) 
players_bundesliga_df$height <- sub(pattern = ",", x = players_bundesliga_df$height, ".")
players_bundesliga_df$height <- as.numeric(players_bundesliga_df$height)

players_bundesliga_df$birth <- sub(pattern = "Happy Birthday", x = players_bundesliga_df$birth, "")
players_bundesliga_df$birth[which(players_bundesliga_df$birth == "N/A")] <- NA

Sys.setlocale("LC_TIME", "English")
players_bundesliga_df$birth <- as.Date(players_bundesliga_df$birth, format = "%b %d, %Y")

## SerieA - Italy

cl <- makeCluster(7)
registerDoParallel(cl, cores = 7)
system.time({
  info_seriesa_df <- foreach(i=1:nrow(players_seriesa_df), .combine = "rbind", .packages = c("dplyr", "rvest")) %dopar% {
    getplayersinfo(players_seriesa_df$player_name[[i]], players_seriesa_df$player_id[[i]])
  }
})
stopCluster(cl)

players_seriesa_df <- info_seriesa_df
players_seriesa_df$foot[which(players_seriesa_df$foot == "N/A")] <- NA
players_seriesa_df$foot <- factor(players_seriesa_df$foot)

players_seriesa_df$age <- as.numeric(players_seriesa_df$age)

players_seriesa_df$citizenship <- as.character(players_seriesa_df$citizenship)

players_seriesa_df$height[which(players_seriesa_df$height == "N/A")] <- NA
players_seriesa_df$height <- substr(players_seriesa_df$height, 1, 4) 
players_seriesa_df$height <- sub(pattern = ",", x = players_seriesa_df$height, ".")
players_seriesa_df$height <- as.numeric(players_seriesa_df$height)

players_seriesa_df$birth <- sub(pattern = "Happy Birthday", x = players_seriesa_df$birth, "")
players_seriesa_df$birth[which(players_seriesa_df$birth == "N/A")] <- NA

Sys.setlocale("LC_TIME", "English")
players_seriesa_df$birth <- as.Date(players_seriesa_df$birth, format = "%b %d, %Y")


## ligueA - France

cl <- makeCluster(7)
registerDoParallel(cl, cores = 7)
system.time({
  info_ligue_df <- foreach(i=1:nrow(players_ligue_df), .combine = "rbind", .packages = c("dplyr", "rvest")) %dopar% {
    getplayersinfo(players_ligue_df$player_name[[i]], players_ligue_df$player_id[[i]])
  }
})
stopCluster(cl)

players_ligue_df <- info_ligue_df
players_ligue_df$foot[which(players_ligue_df$foot == "N/A")] <- NA
players_ligue_df$foot <- factor(players_ligue_df$foot)

players_ligue_df$citizenship <- as.character(players_ligue_df$citizenship)

players_ligue_df$height[which(players_ligue_df$height == "N/A")] <- NA
players_ligue_df$height <- substr(players_ligue_df$height, 1, 4) 
players_ligue_df$height <- sub(pattern = ",", x = players_ligue_df$height, ".")
players_ligue_df$height <- as.numeric(players_ligue_df$height)

players_ligue_df$birth <- sub(pattern = "Happy Birthday", x = players_ligue_df$birth, "")
players_ligue_df$birth[which(players_ligue_df$birth == "N/A")] <- NA

Sys.setlocale("LC_TIME", "English")
players_ligue_df$birth <- as.Date(players_ligue_df$birth, format = "%b %d, %Y")


## year to season

year2season <- function(year) {
  season <- recode(as.character(year), "2005" = "05/06", "2006" = "06/07",
                   "2007" = "07/08", "2008" = "08/09", "2009" = "09/10",
                   "2010" = "10/11", "2011" = "11/12", "2012" = "12/13",
                   "2013" = "13/14", "2014" = "14/15", "2015" = "15/16", 
                   "2016" = "16/17", "2017" = "17/18", "2018" = "18/19",
                   "2019" = "19/20", "2020" = "20/21", "2021" = "21/22")
  return(season)
}

players_big_esp_df$season <- year2season(players_big_esp_df$year)
players_big_premier_df$season <- year2season(players_big_premier_df$year)
players_big_bundesliga_df$season <- year2season(players_big_bundesliga_df$year)
players_big_seriesa_df$season <- year2season(players_big_seriesa_df$year)
players_big_ligue_df$season <- year2season(players_big_ligue_df$year)


## clean market

source("../scraping/cleanMarket.R")
options(scipen = 999)

## Market value

market_value<-players_big_esp_df$market_value
players_big_esp_df$market_value <- cleanMarket(market_value)
market_value<-players_big_premier_df$market_value
players_big_premier_df$market_value <- cleanMarket(market_value)
market_value<-players_big_bundesliga_df$market_value
players_big_bundesliga_df$market_value <- cleanMarket(market_value)
market_value<-players_big_seriesa_df$market_value
players_big_seriesa_df$market_value <- cleanMarket(market_value)
market_value<-players_big_ligue_df$market_value
players_big_ligue_df$market_value <- cleanMarket(market_value)

## add league column

players_big_esp_df$league <- "laliga"
players_big_premier_df$league <- "premier"
players_big_bundesliga_df$league <- "bundesliga"
players_big_seriesa_df$league <- "seriea"
players_big_ligue_df$league <- "ligue1"

players_esp_df$league <- "laliga"
players_premier_df$league <- "premier"
players_bundesliga_df$league <- "bundesliga"
players_seriesa_df$league <- "seriea"
players_ligue_df$league <- "ligue1"


# All in one ---- 

players_big_df <- rbind(players_big_esp_df, players_big_premier_df, players_big_bundesliga_df, players_big_seriesa_df, players_big_ligue_df)
players_big_df$league <- as.factor(players_big_df$league)

players_df <- rbind(players_esp_df, players_premier_df, players_bundesliga_df, players_seriesa_df, players_ligue_df)
players_df <- players_df %>% 
  select(-league) %>% 
  unique() 

players_big_df <- left_join(players_big_df, players_df, by = c("player_name", "player_id"))


## 4. Players Goals ----

source("../scraping/getplayersgoals.R")


cl <- makeCluster(7)
registerDoParallel(cl, cores = 7)
system.time({
  players_goals_df <- foreach(i = 1:nrow(players_df), .combine = "rbind", .packages = c("dplyr", "rvest", "stringr", "xml2")) %dopar% {
    getplayersgoals(players_df$player_name[[i]], players_df$player_id[[i]], players_df$position[[i]])
  }
})
stopCluster(cl)

players_goals_df$team <- NULL
seasons <- c("05/06", "06/07", "07/08", "08/09", "09/10", "10/11", "11/12", 
             "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19",
             "19/20", "20/21", "21/22")
players_goals_df <- subset(players_goals_df, season %in% seasons)

players_goals_df <- players_goals_df %>% 
  mutate(across(matches_played:clean_sheets, ~if_else(is.na(.), 0, .)))


# Merge observations from the same year

players_goals_df_short <- players_goals_df[FALSE,]
aux_old <- players_goals_df_short
for (i in 1:(nrow(players_goals_df))){
  id <- players_goals_df$player_id[i]
  seasoni <- players_goals_df$season[i]
  aux <- subset(players_goals_df, player_id==id & season==seasoni)
  if (identical(aux,aux_old)==F){
    aux_old <- aux
    n <- nrow(aux)
    if(n>1){
      for (j in 2:n){
        aux[1,5:13] <- aux[1,5:13] + aux[j,5:13]
      }
    }
    players_goals_df_short <- rbind(players_goals_df_short,aux[1,])
  }
}
players_goals_df <- players_goals_df_short
players_goals_df$liga <- NULL
players_goals_df$matches_played <- NULL
players_goals_df$minutes_played <- NULL


## 5. Clean and join all the data in players_big_df ----

players_big_df <- left_join(players_big_df, players_goals_df,
                            by = c("player_name", "player_id", "season"))

players_big_df <- players_big_df %>% 
  mutate(across(goals:clean_sheets, ~if_else(is.na(.), 0, .)))

players_big_df <- players_big_df[, c(1,2,3,4,5,9,10,6,7,8,11,12,13,14,15,16,17)]

players_big_df$season <- as.factor(players_big_df$season)

players_df$age <- as.numeric(players_df$age)
players_df$position <- as.factor(players_df$position)

position2 <- recode(players_df$position,
                              "attack" = "Forward",
                              "attack - Centre-Forward" = "Forward",
                              "attack - Left Winger" = "Forward",
                              "attack - Right Winger" = "Forward",
                              "attack - Second Striker" = "Forward",
                              "Defender" = "Defender",
                              "Defender - Centre-Back" = "Defender",
                              "Defender - Left-Back" = "Defender",
                              "Defender - Right-Back" = "Defender",
                              "Defender - Sweeper" = "Defender",
                              "Goalkeeper" = "Goalkeeper",
                              "midfield" = "Midfielder",
                              "midfield - Attacking Midfield" = "Midfielder",
                              "midfield - Central Midfield" = "Midfielder",
                              "midfield - Defensive Midfield" = "Midfielder",
                              "midfield - Left Midfield" = "Midfielder",
                              "midfield - Right Midfield" = "Midfielder")
players_df <- add_column(players_df, position2, .after = "position")

players_df$position <- recode(players_df$position,
                              "attack" = "NA",
                              "attack - Centre-Forward" = "Forward - Centre-Forward",
                              "attack - Left Winger" = "Forward - Left Winger",
                              "attack - Right Winger" = "Forward - Right Winger",
                              "attack - Second Striker" = "Forward - Second Striker",
                              "Defender" = "NA",
                              "Defender - Centre-Back" = "Defender - Centre-Back",
                              "Defender - Left-Back" = "Defender - Left-Back",
                              "Defender - Right-Back" = "Defender - Right-Back",
                              "Defender - Sweeper" = "Defender - Centre-Back",
                              "Goalkeeper" = "Goalkeeper",
                              "midfield" = "NA",
                              "midfield - Attacking Midfield" = "Midfielder - Attacking Midfielder",
                              "midfield - Central Midfield" = "Midfielder - Central Midfielder",
                              "midfield - Defensive Midfield" = "Midfielder - Defensive Midfielder",
                              "midfield - Left Midfield" = "Midfielder - Left Midfielder",
                              "midfield - Right Midfield" = "Midfielder - Right Midfielder")
players_df$position[players_df$position=="NA"] <- NA
players_df$position <- droplevels(players_df$position)
table(players_df$position)

players_all_df <- left_join(players_big_df, players_df,
                       by = c("player_name", "player_id"))
players_all_df$age <- players_all_df$year - as.numeric(format(players_all_df$birth,"%Y"))

## 6. SAVE players ----

save(players_big_bundesliga_df, players_bundesliga_df, players_exposure_bundesliga_df, 
     players_big_esp_df, players_esp_df, players_exposure_esp_df,
     players_big_premier_df, players_premier_df, players_exposure_premier_df,
     players_big_seriesa_df, players_seriesa_df, players_exposure_seriesa_df,
     players_big_ligue_df, players_ligue_df, players_exposure_ligue_df,
     players_big_df, players_df, players_all_df,
     file = "../data\players.RData")

saveRDS(players_all_df, "../data/players_all_df.RDs", compress = TRUE)