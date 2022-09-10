################################################################################
##
## getplayersexposure function
##  - Arguments: club, club_id, year
##  - Output: dataframe with the name of the player in the season and the club
##            specified. Also the matches and minutes played in all the 
##            competitions the club has played
## 
##  - Example: getplayers("athletic-bilbao", 621, 2020)
##
################################################################################


players_exposure <- function(club_name, club_id, year) {
  url_profile <- paste0("https://www.transfermarkt.com/", club_name, "/leistungsdaten/verein/", club_id, "/plus/0?reldata=%26", year)
  
  tabla <- url(url_profile) %>% 
    read_html() %>% 
    html_nodes(xpath = "/html/body/div[3]/div[6]/div[1]/div/div[4]/div/table")
  
  if (length(tabla)) {
    tabla_values <- tabla %>% 
      html_nodes("td") %>% 
      html_text()
    
    matches_played <- tabla_values[seq(9, length(tabla_values), by = 11)]
    minutes_played <- tabla_values[seq(11, length(tabla_values), by = 11)]
    
    matches_played[which(matches_played == "Not in squad during this season")] <- 0
    matches_played[which(matches_played == "Not used during this season")] <- 0
    minutes_played[which(minutes_played == "-")] <- 0
    
    matches_played <- as.numeric(matches_played)
    minutes_played <- sub("'", "", minutes_played) %>% 
      sub(".", "", ., fixed = TRUE) %>% 
      as.numeric
  } else {
    matches_played <- NA
    minutes_played <- NA
  }
  
  hrefs <- tabla %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  hrefs <- hrefs[str_detect(hrefs, "/profil/")] %>% unique()
  hrefs <- str_split(hrefs, "/") %>% unlist() %>% .[sort(c(seq(2, length(.), by=5), seq(5, length(.), by=5)))]  # split by "/"
  
  n <- length(hrefs)
  players_exposure_df <- data.frame(player_name = hrefs[seq(1, n, by=2)],
                                    club_name = club_name,
                                    year = year,
                                    matches_played = matches_played,
                                    minutes_played = minutes_played)
  
  return(players_exposure_df)
}