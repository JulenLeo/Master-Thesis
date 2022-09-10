# Packages

pacman::p_load(rvest, stringr, dplyr, purrr, doParallel, foreach, xml2, tibble)


## 1. Injuries ----

load("data/players.RData")
source("scraping/getinjury.R")

cl <- makeCluster(7)
registerDoParallel(cl, cores = 7)
system.time({
  injury_df <- foreach(i = 1:nrow(players_df), .combine = "rbind", .packages = c("dplyr", "rvest", "xml2")) %dopar% {
    getinjury(players_df$player_name[[i]], players_df$player_id[[i]])
  }
})
stopCluster(cl)


Sys.setlocale("LC_TIME", "English")

injury_df$from <- as.Date(injury_df$from, format = "%b %d, %Y") %>% format
injury_df$from <- as.Date(injury_df$from)
injury_df$until <- as.Date(injury_df$until, format = "%b %d, %Y") %>% format
injury_df$until <- as.Date(injury_df$until)
injury_df <- rename(injury_df, season = Season)

seasons <- c("05/06", "06/07", "07/08", "08/09", "09/10", "10/11", "11/12", 
             "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19",
             "19/20", "20/21", "21/22")
injury_df <- subset(injury_df, season %in% seasons)
injury_df$Team <- NULL
injury_df <- rename(injury_df, injury = Injury)

injury_df$`Games missed`[injury_df$`Games missed`=="-"] <- NA
injury_df$`Games missed`[injury_df$`Games missed`=="?"] <- NA
injury_df$`Games missed` <- as.numeric(injury_df$`Games missed`)
injury_df <- rename(injury_df, games_missed = `Games missed`)

injury_df$Days <- sub(" days", "", injury_df$Days) %>% as.numeric
injury_df <- rename(injury_df, days_missed = Days)


range(injury_df$from, na.rm = T)
range(injury_df$until, na.rm = T)

sort(injury_df$until, decreasing = T)
injury_df <- injury_df[-which(injury_df$from > "2022-06-30"),]

injury_df$season <- as.factor(injury_df$season)

year2season <- function(year) {
  season <- recode(as.character(year), "2005" = "05/06", "2006" = "06/07",
                   "2007" = "07/08", "2008" = "08/09", "2009" = "09/10",
                   "2010" = "10/11", "2011" = "11/12", "2012" = "12/13",
                   "2013" = "13/14", "2014" = "14/15", "2015" = "15/16", 
                   "2016" = "16/17", "2017" = "17/18", "2018" = "18/19",
                   "2019" = "19/20", "2020" = "20/21", "2021" = "21/22")
  return(season)
}

injury_df <- injury_df %>% mutate(year = as.numeric(ifelse(as.numeric(format(from,"%m"))>=7, as.numeric(format(from,"%Y")), as.numeric(format(from,"%Y"))-1)))
injury_df$season <- year2season(injury_df$year)
injury_df$year <- NULL


## 2. SAVE injuries ----
save(injury_df, file = "data\injuries.RData")

saveRDS(injury_df, "data\injury_df.RDs", compress = TRUE)