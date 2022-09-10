################################################################################
##
## getplayersgoals function
##  - Arguments: player_name, player_id, year, position
##  - Output: dataframe with the statistics of the jugador: goals,
##     assists and cards.
##  - Example: getplayersgoals("iker-muniain", 54235, "Attacking Midfield")
##
################################################################################


getplayersgoals <- function(player_name, player_id, position) {
  url_name <- paste0("https://www.transfermarkt.com/", player_name, "/leistungsdatendetails/spieler/", player_id)
  
  players_goals_df <- read_html(url(url_name, method = "libcurl")) %>%
    # httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
    # read_html() %>% 
    # html_nodes("table") %>% 
    xml_find_all(xpath='/html/body/div[2]/main/div[3]/div[1]/div/div[4]/div/table') %>%
    html_table(fill = TRUE) %>% 
    # .[[2]] %>% 
    data.frame(check.names = TRUE) %>% 
    slice(-1) %>% 
    rename_all(~paste0("X",1:length(.)))
  
  if (!length(players_goals_df)) {
    players_goals_df <- read_html(url(url_name, method = "libcurl")) %>%
      # httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
      # read_html() %>%
      # html_nodes("table") %>% 
      xml_find_all(xpath='/html/body/div[3]/div[5]/div[1]/div/div[4]/div/table') %>%
      html_table(fill = TRUE) %>% 
      # .[[2]] %>% 
      data.frame(check.names = TRUE) %>% 
      slice(-1) %>% 
      rename_all(~paste0("X",1:length(.)))
  }
  
  if (!length(players_goals_df)) {
    players_goals_df <- NULL
  } else {
    ## si es portero
    if (position=="Goalkeeper" & position!="") {
      players_goals_df <- players_goals_df %>% 
        rename("season" = "X1",
               "liga" = "X3", 
               "team" = "X4",
               "matches_played" = "X5",
               "goals" = "X6",
               "cards" = "X7",
               "goals_conceded" = "X8",
               "clean_sheets" = "X9",
               "minutes_played" = "X10") %>% 
        select(-X2) %>% 
        mutate(assists = NA)
    } else { ## si no
      players_goals_df <- players_goals_df %>% 
        rename("season" = "X1",
               "liga" = "X3", 
               "team" = "X4",
               "matches_played" = "X5",
               "goals" = "X6",
               "assists" = "X7",
               "cards" = "X8",
               "minutes_played" = "X9") %>% 
        select(-X2) %>% 
        mutate(goals_conceded = NA,
               clean_sheets = NA)
    }
    
    
    ## scrap and add club_name
    players_goals_df$team <- sapply(1:nrow(players_goals_df), function(i) {
      res <- url_name %>%
        read_html() %>%
        html_node(xpath = paste0('/html/body/div[3]/div[6]/div[1]/div/div[4]/div/table/tbody/tr[', i,']/td[4]/a')) %>%
        html_attr(., "title")
      return(res)
    })
    
    ## Borrar las observaciones que no son de liga
    ligas5europa <- c("LaLiga", "Premier League", "Bundesliga", "Serie A", "Ligue 1",
                      "Champions League", "Champions League Qu.", "Champions League Qualifying",
                      "Europa League", "Europa League Qualifying",
                      "UEFA Cup", "UEFA-Cup Qualifikation",
                      "Conference League", "Conference League Qu.", "Conference League Qualifying", 
                      "Copa del Rey", "Supercopa",
                      "EFL Cup", "FA Cup", "Community Shield",
                      "DFB-Pokal", "DFL-Supercup",
                      "Copa Italia", "Supercoppa Italiana",
                      "Coupe de France", "Coupe de la Ligue", "Trophée des Champions",
                      "Club World Cup", "UEFA Super Cup")
    players_goals_df <- players_goals_df %>% 
      filter(liga %in% ligas5europa)
    
    ## clean dataframe
    cards_df <- str_split(players_goals_df$cards, "/", simplify = T) %>% 
      str_trim() %>% 
      matrix(ncol = 3, byrow = FALSE) %>% 
      as.data.frame() %>% 
      rename("yellows" = "V1", "second_yellow" = "V2", "reds" = "V3") %>% 
      mutate_all(~as.numeric(ifelse(. == "-", NA, .)))
    
    players_goals_df <- players_goals_df %>% 
      mutate(goals = as.numeric(ifelse(goals == "-", NA, goals)),
             assists = as.numeric(ifelse(assists == "-", NA, assists)),
             yellows = cards_df$yellows,
             second_yellow = cards_df$second_yellow,
             reds = cards_df$reds,
             matches_played = as.numeric(ifelse(matches_played == "-", NA, matches_played)),
             minutes_played = ifelse(minutes_played == "-", NA, minutes_played),
             minutes_played = as.numeric(
               str_remove_all(players_goals_df$minutes_played, "'|\\.") ## convertir en valor numerico (quitar las "'" y los ".")
             ), 
             goals_conceded = as.numeric(ifelse(goals_conceded == "-", NA, goals_conceded)),
             clean_sheets = as.numeric(ifelse(clean_sheets == "-", NA, clean_sheets))
      ) %>% 
      select(-cards)
    
    players_goals_df <- players_goals_df %>% 
      mutate(player_name = player_name, player_id = player_id) %>% 
      select(player_name, player_id, season, liga, team,
             matches_played, minutes_played, 
             goals, assists, yellows, second_yellow, reds,
             goals_conceded, clean_sheets) %>% 
      arrange(season)
  }
  
  return(players_goals_df)
}