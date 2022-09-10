################################################################################
##
## getclubPosition function
##  - Arguments: league, league_id, year
##  - Output: dataframe with the names of the clubs in transfermarkt, season,
##                ranking/classification in that season and points obtained.
## 
##  - Example: getclubPosition("laliga", "ES1", 2020)
##
##  - Possible inputs: league <- c("laliga", "premier-league", "1-bundesliga", 
##                                 "series-a", "ligue-1")
##                     league_id <- c("ES1", "GB1", "L1", "IT1", "FR1")
##
################################################################################


getclubPosition <- function(league, league_id, year) {
  url_club <- paste0("https://www.transfermarkt.com/", league, "/startseite/wettbewerb/", league_id, "/plus/?saison_id=", year)
  
  clubs <- url(url_club) %>% 
    read_html() %>% 
    html_nodes(xpath = "/html/body/div[3]/div[6]/div[2]/div[3]/div/div[2]/table")

  if (!length(clubs)) {
    clubs <- url(url_club) %>% 
      read_html() %>% 
      html_nodes(xpath = "/html/body/div[3]/div[6]/div[2]/div[2]/div/div[2]/table")
  }
  
  clubs_refs <-  clubs %>% 
    html_nodes("a") %>% 
    html_attr("href")
  clubs_refs <- clubs_refs[str_detect(clubs_refs, "/spielplan/verein/")] %>% unique()
  clubs_refs <- str_split(clubs_refs, "/") %>% unlist() %>% .[sort(c(seq(2, length(.), by=7), seq(5, length(.), by=7)))]
  
  n <- length(clubs_refs)
  club_name <- clubs_refs[seq(from = 1, to = n, by=2)]
  club_id <- clubs_refs[seq(from = 2, to = n, by=2)]
  
  clubs_pos <- clubs %>%
    html_table(fill = TRUE) %>% .[[1]]
  clubs_pos <- clubs_pos[,c(-2)]
  ranking <- clubs_pos$`#`
  pts <- clubs_pos$Pts
  
  clubs_pos_df <- data.frame(club_name = club_name,
                             club_id = club_id,
                             year = year, 
                             ranking = ranking,
                             points = pts) 
  
  return(clubs_pos_df)
}