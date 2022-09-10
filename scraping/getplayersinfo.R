################################################################################
##
## getplayersinfo function
##  - Arguments: player_name, player_id
##  - Output: dataframe with the characteristics of the jugador: name, id,
##                birthdate, age, birthplace, nationality,
##                height, position, foot.
## 
##  - Example: getplayersinfo("xabi-alonso", 7476)
##
################################################################################


getplayersinfo <- function(player_name, player_id) {
  url <- paste0("https://www.transfermarkt.com/", player_name, "/profil/spieler/", player_id)
  
  tabla <- url %>% 
    read_html() %>% 
    html_nodes(".info-table__content") %>%
    html_text()
  
  n <- length(tabla)
  colnames <- tabla[seq(1, n, by=2)]
  colnames <- sub(pattern = "\n", x = colnames, "")
  colnames <- gsub(pattern = "  ", x = colnames, "")
  colnames <- gsub(pattern = ":", x = colnames, "")
  info <- tabla[seq(2, n, by=2)]
  info <- sub(pattern = "\n", x = info, "")
  info <- gsub(pattern = "  ", x = info, "")
  player_info <- t(data.frame(info))
  colnames(player_info) <- colnames
  player_info <- as.data.frame(player_info)
  
  info_df <- data.frame(player_name = player_name,
                        player_id = player_id,
                        birth = ifelse(is.null(player_info$`Date of birth`), NA,
                                       player_info$`Date of birth`) ,
                        age = ifelse(is.null(player_info$Age), NA, 
                                     player_info$Age),
                        place = ifelse(is.null(player_info$`Place of birth`), NA,
                                       player_info$`Place of birth`),
                        height = ifelse(is.null(player_info$Height), NA,
                                        player_info$Height),
                        citizenship = ifelse(is.null(player_info$Citizenship), NA,
                                             player_info$Citizenship),
                        position = ifelse(is.null(player_info$Position), NA, 
                                          player_info$Position),
                        foot = ifelse(is.null(player_info$Foot), NA, 
                                      player_info$Foot))
  return(info_df)
}