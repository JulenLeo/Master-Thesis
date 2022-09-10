################################################################################
##
## getclub function
##  - Arguments: club, club_id, year
##  - Output: dataframe with the name of the player in the season and the club
##            specified. Also his id, season, and the market value at the end
##            of that season.
## 
##  - Example: getplayers("athletic-bilbao", 621, 2020)
##
################################################################################


getplayers <- function(club, club_id, year) {
  url <- paste0("https://www.transfermarkt.com/", club, "/startseite/verein/", club_id, "/saison_id/", year)
  
  tabla <- url %>% 
    read_html() %>% 
    html_nodes(xpath = "/html/body/div[3]/div[6]/div[1]/div/div[4]/div/table")
  
  hrefs <- tabla[[1]] %>% 
    html_nodes("a") %>% 
    html_attr('href')
  
  hrefs <- hrefs[str_detect(hrefs, "/profil/")] %>% unique()
  hrefs <- str_split(hrefs, "/") %>% unlist() %>% .[sort(c(seq(2, length(.), by=5), seq(5, length(.), by=5)))]  # split by "/"
  
  n <- length(hrefs)
  player_name = hrefs[seq(1, n, by=2)]
  player_id = hrefs[seq(2, n, by=2)]
  
  market_value <- tabla %>% 
    html_nodes("td") %>% html_text()
  market_value <- market_value[seq(from = 9, to = length(market_value), by = 9)]
  
  if (club=="espanyol-barcelona" && club_id=="714" && year==2019) {
    market_value <- market_value[-31]
  }
  if (club=="fc-elche" && club_id=="1531" && year==2020) {
    market_value <- market_value[-36]
  }
  if (club=="manchester-united" && club_id=="985" && year==2016) {
    market_value <- market_value[-21]
  }
  
  hrefs_df <- data.frame(player_name = player_name,
                         player_id = player_id,
                         club_name = club,
                         club_id = club_id,
                         year = year,
                         market_value = market_value)
  return(hrefs_df)
}