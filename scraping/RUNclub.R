# Packages

pacman::p_load(rvest, stringr, dplyr, purrr, doParallel, foreach, xml2)

# 1. Clubs ----

source("../scraping/getclub.R")


## LaLiga - Spain

cl <- makeCluster(4)
registerDoParallel(cl)
system.time({
  club_esp_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
    getclub("laliga", "ES1", year)
  }
})
stopCluster(cl)


## PremierLeague - England

cl <- makeCluster(4)
registerDoParallel(cl)
club_premier_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclub("premier-league", "GB1", year)
}
stopCluster(cl)


## Bundesliga - Germany

cl <- makeCluster(4)
registerDoParallel(cl)
club_bundesliga_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclub("1-bundesliga", "L1", year)
}
stopCluster(cl)


## SerieA - Italy

cl <- makeCluster(4)
registerDoParallel(cl)
club_seriesa_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclub("series-a", "IT1", year)
}
stopCluster(cl)


## Ligue1 - France

cl <- makeCluster(4)
registerDoParallel(cl)
club_ligue_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclub("ligue-1", "FR1", year)
}
stopCluster(cl)


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

club_esp_df$season <- year2season(club_esp_df$year)
club_premier_df$season <- year2season(club_premier_df$year)
club_bundesliga_df$season <- year2season(club_bundesliga_df$year)
club_seriesa_df$season <- year2season(club_seriesa_df$year)
club_ligue_df$season <- year2season(club_ligue_df$year)


## alphabetic order

club_esp_df <- arrange(club_esp_df, club_name, year)
club_premier_df <- arrange(club_premier_df, club_name, year)
club_bundesliga_df <- arrange(club_bundesliga_df, club_name, year)
club_seriesa_df <- arrange(club_seriesa_df, club_name, year)
club_ligue_df <- arrange(club_ligue_df, club_name, year)


source("../scraping/getclubPosition.R")


## LaLiga - Spain

cl <- makeCluster(4)
registerDoParallel(cl)
system.time({
  club_pos_esp_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
    getclubPosition("laliga", "ES1", year)
  }
})
stopCluster(cl)


## PremierLeague - England

cl <- makeCluster(4)
registerDoParallel(cl)
club_pos_premier_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclubPosition("premier-league", "GB1", year)
}
stopCluster(cl)


## Bundesliga - Germany
cl <- makeCluster(4)
registerDoParallel(cl)
club_pos_bundesliga_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclubPosition("1-bundesliga", "L1", year)
}
stopCluster(cl)


## SerieA - Italy

cl <- makeCluster(4)
registerDoParallel(cl)
club_pos_seriesa_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclubPosition("series-a", "IT1", year)
}
stopCluster(cl)


## Ligue1 - France

cl <- makeCluster(4)
registerDoParallel(cl)
club_pos_ligue_df <- foreach(year=2005:2021, .combine = "rbind", .packages = c("dplyr", "rvest", "stringr")) %dopar% {
  getclubPosition("ligue-1", "FR1", year)
}
stopCluster(cl)


## merge

club_esp_df <- left_join(club_esp_df, club_pos_esp_df, by = c("club_name", "club_id", "year"))
club_premier_df <- left_join(club_premier_df, club_pos_premier_df, by = c("club_name", "club_id", "year"))
club_bundesliga_df <- left_join(club_bundesliga_df, club_pos_bundesliga_df, by = c("club_name", "club_id", "year"))
club_seriesa_df <- left_join(club_seriesa_df, club_pos_seriesa_df, by = c("club_name", "club_id", "year"))
club_ligue_df <- left_join(club_ligue_df, club_pos_ligue_df, by = c("club_name", "club_id", "year"))


## clean market values

source("../scraping/cleanMarket.R")

## Total market value

total_market_value<-club_esp_df$total_market_value
club_esp_df$total_market_value <- cleanMarket(total_market_value)
total_market_value<-club_premier_df$total_market_value
club_premier_df$total_market_value <- cleanMarket(total_market_value)
total_market_value<-club_bundesliga_df$total_market_value
club_bundesliga_df$total_market_value <- cleanMarket(total_market_value)
total_market_value<-club_seriesa_df$total_market_value
club_seriesa_df$total_market_value <- cleanMarket(total_market_value)
total_market_value<-club_ligue_df$total_market_value
club_ligue_df$total_market_value <- cleanMarket(total_market_value)


## Avg market value

avg_market_value<-club_esp_df$avg_market_value
club_esp_df$avg_market_value <- cleanMarket(avg_market_value)
avg_market_value<-club_premier_df$avg_market_value
club_premier_df$avg_market_value <- cleanMarket(avg_market_value)
avg_market_value<-club_bundesliga_df$avg_market_value
club_bundesliga_df$avg_market_value <- cleanMarket(avg_market_value)
avg_market_value<-club_seriesa_df$avg_market_value
club_seriesa_df$avg_market_value <- cleanMarket(avg_market_value)
avg_market_value<-club_ligue_df$avg_market_value
club_ligue_df$avg_market_value <- cleanMarket(avg_market_value)


## add league column

club_esp_df$league <- "laliga"
club_premier_df$league <- "premier"
club_bundesliga_df$league <- "bundesliga"
club_seriesa_df$league <- "seriea"
club_ligue_df$league <- "ligue1"


## all in one

clubs_df <- rbind(club_esp_df, club_premier_df, club_bundesliga_df, club_seriesa_df, club_ligue_df)


## 2. SAVE clubs ----
save(club_esp_df, club_premier_df, club_bundesliga_df, club_seriesa_df, club_ligue_df, clubs_df,
     file = "../data/clubs.RData")

saveRDS(clubs_df, "../data/clubs_df.RDs", compress = TRUE)
