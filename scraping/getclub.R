################################################################################
##
## getclub function
##  - Arguments: league, league_id, year
##  - Output: dataframe with the names of the clubs, their ids, the year and
##              season. Also the total market value, average market value,
##              and the name of the club in transfermarkt.com.
## 
##  - Example: getclub("laliga", "ES1", 2020)
##
##  - Possible inputs: league <- c("laliga", "premier-league", "1-bundesliga", 
##                                 "series-a", "ligue-1")
##                     league_id <- c("ES1", "GB1", "L1", "IT1", "FR1")
##
################################################################################


getclub <- function(league, league_id, year) {
  url_club <- paste0("https://www.transfermarkt.com/", league, "/startseite/wettbewerb/", league_id, "/plus/?saison_id=", year)
  
  clubs <- url(url_club) %>%  
    read_html() %>%  
    html_nodes(xpath = "/html/body/div[3]/div[6]/div[1]/div[2]/div[2]/div/table")
  
  clubs_refs <-  clubs %>% 
    html_nodes("a") %>% 
    html_attr("href")
  clubs_refs <- clubs_refs[str_detect(clubs_refs, "/startseite/verein/")] %>% unique()
  clubs_refs <- str_split(clubs_refs, "/") %>% unlist() %>% .[sort(c(seq(2, length(.), by=7), seq(5, length(.), by=7)))]
  
  n <- length(clubs_refs)
  club_name <- clubs_refs[seq(from = 1, to = n, by=2)]
  club_id <- clubs_refs[seq(from = 2, to = n, by=2)]
  
  clubs_market <- clubs %>%
    html_table(fill = TRUE) %>% .[[1]]
  name_cols <- names(clubs_market)[c(-1, -3)]
  clubs_market <- clubs_market[-c(1),-c(1, ncol(clubs_market))]
  names(clubs_market) <- name_cols
  avg_market_value <- clubs_market$`ø market value`
  total_market_value <- clubs_market$`Total market value`
  club_name_transfermarkt <- clubs_market$club
  squad <- clubs_market$Squad
  foreigners <- clubs_market$Foreigners
  avg_age <- clubs_market$`? age`
  
  clubs_df <- data.frame(club_name = club_name,
                         club_name_transfermarkt = club_name_transfermarkt,
                         club_id = club_id,
                         year = year,
                         squad = squad,
                         foreigners = foreigners,
                         avg_age = avg_age,
                         avg_market_value = avg_market_value,
                         total_market_value = total_market_value)
  
  return(clubs_df)
}