################################################################################
##
## getinjury function
##  - Arguments: player_name, player_id
##  - Output: dataframe with the history of injuries of a player: season,
##                injury from (date), injury until (date), days out, games missed
## 
##  - Example: getinjury("gareth-bale", 39381)
##
################################################################################


getinjury <- function(player_name, player_id) {
  ## comprobar si tiene mas de una pagina
  url_injury <- paste0("https://www.transfermarkt.com/", player_name,"/verletzungen/spieler/", player_id)
  pages <- url_injury %>%
    read_html() %>% 
    xml_find_all("/html/body/div[2]/main/div[3]/div[1]/div/div[3]/div/div[2]") %>% 
    html_attr("class")
  pages <- ifelse(length(pages) != 0, pages, "no_injured_player")
  
  if (pages == "pager") { ## si tiene mas de una pagina for
    ## comprobar cuantas paginas tiene:
    page_num <- url_injury %>% 
      read_html() %>% 
      xml_find_all("/html/body/div[2]/main/div[3]/div[1]/div/div[3]/div/div[2]/ul") %>% 
      html_children() %>% 
      length()
    page_num <- page_num - 2
    
    injury_df <- NULL
    for (i in seq_len(page_num)) {
      url_injury <- paste0("https://www.transfermarkt.com/", player_name,"/verletzungen/spieler/", player_id, "/page/", i)
      injury_df_aux <- scrape_injury_table(url_injury, player_name, player_id)
      injury_df <- rbind(injury_df, injury_df_aux)
    } 
  } else { ## no tiene mas de una pagina (url sin /page/)
    injury_df <- scrape_injury_table(url_injury, player_name, player_id)
  }
  return(injury_df)
}

scrape_injury_table <- function(url_injury, player_name, player_id) {
  #url_injury <- paste0("https://www.transfermarkt.com/", player_name,"/verletzungen/spieler/", player_id,)
  
  injury_df_aux <- url_injury %>%
    read_html() %>% 
    xml_find_all(xpath='/html/body/div[2]/main/div[3]/div[1]/div/div[3]/div/table') %>%   # /html/body/div[4]/div[10]/div[1]/div/div[2]/div/table
    html_table() 
  
  if (length(injury_df_aux)) { # si tiene lesiones (tabla no nula)
    
    injury_df_aux <- as.data.frame(injury_df_aux[[1]]) 
    
    team <- url_injury %>% # extract team (a veces no hay imagen del escudo del equipo. Cuando games missed = "-", no hay img)
      read_html() %>% 
      html_nodes("table") %>% 
      .[[1]] %>% 
      html_nodes("img") %>% 
      html_attr("alt")
    
    injury_df_aux$Team <- NA
    
    j <- 1
    for(i in 1:dim(injury_df_aux)[1]) {
      if(injury_df_aux$`Games missed`[i]!="-") {
        injury_df_aux$Team[i] <- team[j]
        j <- j + 1
      }
      i <- i + 1
    }
    
    injury_df <- injury_df_aux %>%   # pegar el nombre y el id
      mutate(player_name = player_name,
             player_id = player_id) %>% 
      select(player_name, player_id, Season, everything())
    
  } else {
    injury_df <- NULL
  }
  return(injury_df)
}