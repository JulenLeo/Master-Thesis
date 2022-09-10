#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # Home --------------------------------------------------------------------
  
  # Data Base --------------------------------------------------------------------
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    players_df <- players_df[,c(1,6,3,8,7,23,24,25,19,18,21,22,20,9:17,28:32)]
    players_df$inj_burden <- round(players_df$inj_burden, 2)
    players_df$inj_incidence <- round(players_df$inj_incidence, 2)
    
    ## filters
    
    ### market_value
    players_df <- subset(players_df, (players_df$market_value >= (input$market_value_filter[1] * 1000000) & 
                                        players_df$market_value <= (input$market_value_filter[2] * 1000000)))
    
    ### season
    idx <- match(c(input$season_filter[1],input$season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    ### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$minute_filter[1] &
                                        players_df$minutes_played <= input$minute_filter[2]))
    ### league
    if (input$league_filter != "All"){
      players_df <- subset(players_df, league %in% input$league_filter)
    }
    
    ### age
    players_df <- subset(players_df, (players_df$age >= input$age_filter[1] &
                                        players_df$age <= input$age_filter[2]))
    
    ### height
    players_df <- subset(players_df, (players_df$height >= input$height_filter[1] &
                                        players_df$height <= input$height_filter[2]))
    
    ### position
    if (input$position_filter != "All"){
      players_df <- subset(players_df, position %in% input$position_filter)
    }
    
    ### position2
    if (input$position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$position2_filter)
    }
    
    ### foot
    if (input$foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$foot_filter)
    }
    
    ### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$month_filter[1] &
                                        players_df$`birth month` <= input$month_filter[2]))
    
    
    attach(players_df)
    players_df <- players_df[order(-market_value), ]
    detach(players_df)
    
    players_df
    
  }, extensions = "FixedColumns",
  options = list(pageLength = 10, scrollX = TRUE,
                 fixedColumns = list(leftColumns = 2))))
  
  
  # EDA ---------------------------------------------------------------------
  
  # |- Injury data ----------------------------------------------------------
  
  # ## Reactive data
  # df_exp_EDA1_aux <- callModule(
  #   module = selectizeGroupServer,
  #   id = "filters_EDA1",
  #   data = players_df,  
  #   vars = c("liga", "club_name", "player_name2")
  # )
  # 
  # 
  # df_exp_EDA1 <- eventReactive(input$go_EDA1, {
  #   if (input$tipoButton_EDA1 == 'team') {
  #     df_exp_EDA1 <- players_df %>% ######
  #     filter(club_name == input$team_EDA1 &
  #              input$seasons1_EDA1[[1]] <= year(year_starts) &
  #              input$seasons1_EDA1[[2]] >= year(year_stops) &
  #              input$min_timeExp1_EDA1 <= minutes_played) %>%
  #       droplevels()
  #   }
  #   if (input$tipoButton_EDA1 == 'player') {
  #     df_exp_EDA1 <- df_exp_EDA1_aux() %>%
  #       filter(
  #         input$seasons2_EDA1[[1]] <= year_starts & input$seasons2_EDA1[[2]] >= year_stops &
  #           input$min_timeExp2_EDA1 <= minutes_played
  #       ) %>%
  #       droplevels()
  #   }
  #   return(df_exp_EDA1)
  # }
  # )
  # 
  # df_inj_EDA1 <- eventReactive(input$go_EDA1, { ## this should be calculated after df_exp_EDA1()!
  #   df_inj_EDA1 <- injuries_df %>%
  #     filter(player_name2 %in% levels(df_exp_EDA1()$player_name2) & season %in% levels(df_exp_EDA1()$season)) %>%
  #     droplevels()
  #   return(df_inj_EDA1)
  # }
  # )
  # 
  # ## injd_EDA1
  # injd_EDA1 <- reactive({
  #   ## check if it is possible to make the plot.. check if there is data
  #   validate(need(nrow(df_exp_EDA1()) != 0, "There are no matches in the dataset. Try removing or relaxing one or more filters.")) ## source: https://stackoverflow.com/questions/52330926/if-rows-0-after-filtering-on-inputs-have-shiny-app-display-custom-message-in
  #   df_injuries <- prepare_inj(df_injuries0 = df_inj_EDA1(),
  #                              player         = "player_name",
  #                              date_injured   = "from",
  #                              date_recovered = "until")
  #   
  #   df_exposures <- prepare_exp(df_exposures0 = df_exp_EDA1(),
  #                               player    = "player_name",
  #                               date      = "year",
  #                               time_expo = "minutes_played")
  #   injd <- prepare_all(df_exposures, df_injuries, exp_unit = "matches_minutes")
  #   return(injd)
  # })
  # 
  # ## Start MainPanel
  # ## Ikus: https://statsomat.shinyapps.io/Confirmatory-factor-analysis/
  # ##       https://github.com/aaronjfisher/interAdapt/tree/master/r_package/inst/interAdapt
  # ##       https://beta.rstudioconnect.com/content/2671/Combining-Shiny-R-Markdown.html
  # ##       https://shiny.rstudio.com/articles/generating-reports.html
  # # output$gg_injuryphoto <- renderPlotly(ggplotly(
  # #   gg_injphoto(injd_EDA1(), by_date = "1 year")
  # # ))
  # 
  # output$knitr_EDA1 <- renderUI({
  #   # Set up parameters to pass to Rmd document
  #   params <- list(injd = injd_EDA1())
  #   tempReport <- file.path(tempdir(), "knitr_Injury_EDA_s.Rmd")
  #   file.copy("www/knitr_Injury_EDA_s.Rmd", tempReport, overwrite = TRUE)
  #   # HTML(markdown::markdownToHTML(knit('knitr_Injury_EDA_s.Rmd', quiet = TRUE)))
  #   includeHTML(
  #     rmarkdown::render(tempReport, output_file = "knitr_Injury_EDA_s.html",
  #                       params = params,
  #                       envir = new.env(parent = globalenv()))
  #   )
  # })
  # 
  # #generate a knitr report
  # output$download_EDA1 <- downloadHandler( ## source: https://shiny.rstudio.com/articles/generating-reports.html
  #   filename =  'Injury_EDA.pdf',
  #   contentType =  'application/pdf',
  #   content = function(filename) {
  #     tempReport <- file.path(tempdir(), "knitr_Injury_EDA.Rmd")
  #     file.copy("www/knitr_Injury_EDA.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(injd = injd_EDA1())
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = filename,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
  
  
  # |- Market value -------------------------------------------
  
  ## Barplot
  
  output$plot_mv_v1.1 <- renderPlotly({
    
    ### filters
    
    #### season
    idx <- match(c(input$eda_season_filter[1],input$eda_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played>=input$eda_minute_filter[1] & 
                                        players_df$minutes_played<=input$eda_minute_filter[2]) |
                           is.na(players_df$minutes_played)==TRUE)
    
    #### birth month
    players_df$`birth month` <- as.factor(format(players_df$birth,"%m"))
    
    #### plot
    if(input$variable1_selected %in% c("season", "age", "height", "birth month")){
      df <- data.frame(variable = levels(as.factor(players_df[,input$variable1_selected])),
                       mean_market_value = tapply(players_df$market_value,
                                                  players_df[,input$variable1_selected],
                                                  mean, na.rm=T))
      df$variable <- as.factor(df$variable)
      colnames(df) <- c(input$variable1_selected, "mean_market_value")
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable1_selected))) +
                      geom_bar(stat="identity") +
                      labs(x=input$variable1_selected, y="mean market value") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "league"){
      df <- data.frame(variable = levels(as.factor(players_df[,input$variable1_selected])),
                       mean_market_value = tapply(players_df$market_value,
                                                  players_df[,input$variable1_selected],
                                                  mean, na.rm=T))
      df$variable <- as.factor(df$variable)
      colnames(df) <- c(input$variable1_selected, "mean_market_value")
      g <- ggplotly(ggplot(df, aes(x=reorder(!!as.name(input$variable1_selected),
                                             -mean_market_value),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable1_selected))) +
                      geom_bar(stat="identity") +
                      labs(x=input$variable1_selected, y="mean market value") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "position"){
      df <- data.frame(variable = levels(as.factor(players_df[,input$variable1_selected])),
                       mean_market_value = tapply(players_df$market_value,
                                                  players_df[,input$variable1_selected],
                                                  mean, na.rm=T))
      df$variable <- as.factor(df$variable)
      colnames(df) <- c(input$variable1_selected, "mean_market_value")
      df$position <- factor(df$position,
                            levels = c("Goalkeeper",
                                       "Defender - Centre-Back",
                                       "Defender - Left-Back",
                                       "Defender - Right-Back",
                                       "Midfielder - Defensive Midfielder",
                                       "Midfielder - Central Midfielder",
                                       "Midfielder - Left Midfielder",
                                       "Midfielder - Right Midfielder",
                                       "Midfielder - Attacking Midfielder",
                                       "Forward - Second Striker",
                                       "Forward - Left Winger",
                                       "Forward - Right Winger",
                                       "Forward - Centre-Forward"))
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable1_selected))) +
                      geom_bar(stat="identity") +
                      labs(x=input$variable1_selected, y="mean market value") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "position2"){
      df <- data.frame(variable = levels(as.factor(players_df[,input$variable1_selected])),
                       mean_market_value = tapply(players_df$market_value,
                                                  players_df[,input$variable1_selected],
                                                  mean, na.rm=T))
      df$variable <- as.factor(df$variable)
      colnames(df) <- c(input$variable1_selected, "mean_market_value")
      df$position2 <- factor(df$position2,
                             levels = c("Goalkeeper", "Defender",
                                        "Midfielder", "Forward"))
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable1_selected))) +
                      geom_bar(stat="identity") +
                      labs(x=input$variable1_selected, y="mean market value") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "foot"){
      df <- data.frame(variable = levels(as.factor(players_df[,input$variable1_selected])),
                       mean_market_value = tapply(players_df$market_value,
                                                  players_df[,input$variable1_selected],
                                                  mean, na.rm=T))
      df$variable <- as.factor(df$variable)
      colnames(df) <- c(input$variable1_selected, "mean_market_value")
      df$foot <- factor(df$foot, levels = c("left", "both", "right"))
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable1_selected))) +
                      geom_bar(stat="identity") +
                      labs(x=input$variable1_selected, y="mean market value") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45)))
    }
    
  })
  
  
  ## boxplots
  
  output$plot_mv_v1.2 <- renderPlotly({
    
    ### filters
    
    #### season
    idx <- match(c(input$eda_season_filter[1],input$eda_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played>=input$eda_minute_filter[1] & 
                                        players_df$minutes_played<=input$eda_minute_filter[2]) |
                           is.na(players_df$minutes_played)==TRUE)
    
    #### birth month
    players_df$`birth month` <- as.factor(format(players_df$birth,"%m"))
    
    #### plot
    if(input$variable1_selected!="Please select the first variable..."){
      if(input$variable1_selected == "league"){
        g <- ggplotly(ggplot(players_df, aes(x=reorder(league, -market_value,
                                                       na.rm = TRUE),
                                             y=log(market_value),
                                             fill=as.factor(!!as.name(input$variable1_selected)))) +
                        labs(x=input$variable1_selected, y="log market value",
                             fill=input$variable1_selected) +
                        geom_boxplot() +
                        theme(axis.text.x = element_text(angle = 45)))
      } else {
        g <- ggplotly(ggplot(players_df, aes(x=as.factor(!!as.name(input$variable1_selected)),
                                             y=log(market_value),
                                             fill=as.factor(!!as.name(input$variable1_selected)))) +
                        labs(x=input$variable1_selected, y="log market value",
                             fill=input$variable1_selected) +
                        geom_boxplot() +
                        theme(axis.text.x = element_text(angle = 45)))
      }
    }
  })
  
  
  ## Lineplots
  
  output$plot_mv_v2.1 <- renderPlotly({
    
    ### filters
    
    #### season
    idx <- match(c(input$eda_season_filter[1],input$eda_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played>=input$eda_minute_filter[1] & 
                                        players_df$minutes_played<=input$eda_minute_filter[2]) |
                           is.na(players_df$minutes_played)==TRUE)
    
    #### birth month
    players_df$`birth month` <- as.factor(format(players_df$birth,"%m"))
    
    #### plot
    df<-data.frame()
    for(elem in levels(as.factor(players_df[,input$variable1_selected]))){
      df1<-subset(players_df, players_df[,input$variable1_selected]==elem)
      means<-tapply(df1$market_value,
                    df1[,input$variable2_selected], mean, na.rm=T)
      aux<-data.frame(cbind(rep(elem,length(levels(as.factor(players_df[,input$variable2_selected])))),
                            levels(as.factor(players_df[,input$variable2_selected])),means))
      df<-rbind(df,aux)
    }
    rownames(df)<-1:nrow(df)
    colnames(df)<-c(input$variable1_selected,input$variable2_selected,"mean_market_value")
    df$mean_market_value<-as.numeric(df$mean_market_value)
    g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                 y=mean_market_value,
                                 group=!!as.name(input$variable2_selected))) +
                    geom_line(aes(color=!!as.name(input$variable2_selected)))+
                    geom_point(aes(color=!!as.name(input$variable2_selected)))+
                    labs(x=input$variable1_selected, y="mean market value")+
                    theme(axis.text.x = element_text(angle = 45)))
  })
  
  output$plot_mv_v2.2 <- renderPlotly({
    
    ## filters
    
    ### season
    idx <- match(c(input$eda_season_filter[1],input$eda_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    ### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played>=input$eda_minute_filter[1] & 
                                        players_df$minutes_played<=input$eda_minute_filter[2]) |
                           is.na(players_df$minutes_played)==TRUE)
    
    ### birth month
    players_df$`birth month` <- as.factor(format(players_df$birth,"%m"))
    
    ### plot
    if(input$variable1_selected == "league"){
      df<-data.frame()
      for(elem in levels(as.factor(players_df[,input$variable2_selected_II]))){
        df1<-subset(players_df, players_df[,input$variable2_selected_II]==elem)
        means<-tapply(df1$market_value,
                      df1[,input$variable1_selected], mean, na.rm=T)
        aux<-data.frame(cbind(rep(elem,length(levels(as.factor(players_df[,input$variable1_selected])))),
                              levels(as.factor(players_df[,input$variable1_selected])),means))
        df<-rbind(df,aux)
      }
      rownames(df)<-1:nrow(df)
      colnames(df)<-c(input$variable2_selected_II,input$variable1_selected,"mean_market_value")
      df$mean_market_value<-as.numeric(df$mean_market_value)
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable2_selected_II))) +
                      geom_bar(stat="identity", position=position_dodge())+
                      labs(x=input$variable1_selected, y="mean market value")+
                      theme_minimal()+
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "position") {
      df<-data.frame()
      for(elem in levels(as.factor(players_df[,input$variable2_selected_III]))){
        df1<-subset(players_df, players_df[,input$variable2_selected_III]==elem)
        means<-tapply(df1$market_value,
                      df1[,input$variable1_selected], mean, na.rm=T)
        aux<-data.frame(cbind(rep(elem,length(levels(as.factor(players_df[,input$variable1_selected])))),
                              levels(as.factor(players_df[,input$variable1_selected])),means))
        df<-rbind(df,aux)
      }
      rownames(df)<-1:nrow(df)
      colnames(df)<-c(input$variable2_selected_III,input$variable1_selected,"mean_market_value")
      df$mean_market_value<-as.numeric(df$mean_market_value)
      df$position <- factor(df$position,
                            levels = c("Goalkeeper",
                                       "Defender - Centre-Back",
                                       "Defender - Left-Back",
                                       "Defender - Right-Back",
                                       "Midfielder - Defensive Midfielder",
                                       "Midfielder - Central Midfielder",
                                       "Midfielder - Left Midfielder",
                                       "Midfielder - Right Midfielder",
                                       "Midfielder - Attacking Midfielder",
                                       "Forward - Second Striker",
                                       "Forward - Left Winger",
                                       "Forward - Right Winger",
                                       "Forward - Centre-Forward"))
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable2_selected_III))) +
                      geom_bar(stat="identity", position=position_dodge())+
                      labs(x=input$variable1_selected, y="mean market value")+
                      theme_minimal()+
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "position2") {
      df<-data.frame()
      for(elem in levels(as.factor(players_df[,input$variable2_selected_IV]))){
        df1<-subset(players_df, players_df[,input$variable2_selected_IV]==elem)
        means<-tapply(df1$market_value,
                      df1[,input$variable1_selected], mean, na.rm=T)
        aux<-data.frame(cbind(rep(elem,length(levels(as.factor(players_df[,input$variable1_selected])))),
                              levels(as.factor(players_df[,input$variable1_selected])),means))
        df<-rbind(df,aux)
      }
      rownames(df)<-1:nrow(df)
      colnames(df)<-c(input$variable2_selected_IV,input$variable1_selected,"mean_market_value")
      df$mean_market_value<-as.numeric(df$mean_market_value)
      df$position2 <- factor(df$position2,
                             levels = c("Goalkeeper", "Defender",
                                        "Midfielder", "Forward"))
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable2_selected_IV))) +
                      geom_bar(stat="identity", position=position_dodge())+
                      labs(x=input$variable1_selected, y="mean market value")+
                      theme_minimal()+
                      theme(axis.text.x = element_text(angle = 45)))
    } else if (input$variable1_selected == "foot") {
      df<-data.frame()
      for(elem in levels(as.factor(players_df[,input$variable2_selected_V]))){
        df1<-subset(players_df, players_df[,input$variable2_selected_V]==elem)
        means<-tapply(df1$market_value,
                      df1[,input$variable1_selected], mean, na.rm=T)
        aux<-data.frame(cbind(rep(elem,length(levels(as.factor(players_df[,input$variable1_selected])))),
                              levels(as.factor(players_df[,input$variable1_selected])),means))
        df<-rbind(df,aux)
      }
      rownames(df)<-1:nrow(df)
      colnames(df)<-c(input$variable2_selected_V,input$variable1_selected,"mean_market_value")
      df$mean_market_value<-as.numeric(df$mean_market_value)
      df$foot <- factor(df$foot, levels = c("left", "both", "right"))
      g <- ggplotly(ggplot(df, aes(x=!!as.name(input$variable1_selected),
                                   y=mean_market_value,
                                   fill=!!as.name(input$variable2_selected_V))) +
                      geom_bar(stat="identity", position=position_dodge())+
                      labs(x=input$variable1_selected, y="mean market value")+
                      theme_minimal()+
                      theme(axis.text.x = element_text(angle = 45)))
    }
  })
  
  # |- Market value & injury data -------------------------------------------
  
  # ## Reactive data
  # df_exp_EDA3_aux <- callModule(
  #   module = selectizeGroupServer,
  #   id = "filters_EDA3",
  #   data = players_df, 
  #   vars = c("liga", "club_name", "player_name2")
  # )
  # 
  # 
  # df_exp_EDA3 <- eventReactive(input$go_EDA3, {
  #   if (input$tipoButton_EDA3 == 'team') {
  #     df_exp_EDA3 <- players_df %>% #######
  #     filter(club_name == input$team_EDA3 & 
  #              input$seasons1_EDA3[[1]] <= year_starts &
  #              input$seasons1_EDA3[[2]] >= year_stops &
  #              input$min_timeExp1_EDA3 <= minutes_played) %>%
  #       droplevels()
  #   }
  #   if (input$tipoButton_EDA3 == 'player') {
  #     df_exp_EDA3 <- df_exp_EDA3_aux() %>%
  #       filter(
  #         input$seasons2_EDA3[[1]] <= year_starts & input$seasons2_EDA3[[2]] >= year_stops &
  #           input$min_timeExp2_EDA3 <= minutes_played
  #       ) %>%
  #       droplevels()
  #   }
  #   return(df_exp_EDA3)
  # }
  # )
  # 
  # df_inj_EDA3 <- eventReactive(input$go_EDA3, { ## this should be calculated after df_exp_EDA3()!
  #   df_inj_EDA3 <- injuries_df %>%
  #     filter(player_name2 %in% levels(df_exp_EDA3()$player_name2) & season %in% levels(df_exp_EDA3()$season)) %>%
  #     droplevels()
  #   return(df_inj_EDA3)
  # }
  # 
  # )
  # 
  # 
  # ## Plot gg_marketinjury
  # plot_EDA3 <- reactive({
  #   ## check if it is possible to make the plot.. check if there is data
  #   validate(need(nrow(df_exp_EDA3()) != 0, "There are no matches in the dataset. Try removing or relaxing one or more filters.")) ## source: https://stackoverflow.com/questions/52330926/if-rows-0-after-filtering-on-inputs-have-shiny-app-display-custom-message-in
  #   plot_EDA3 <- gg_marketinjury(df_exp_EDA3(), df_inj_EDA3())
  #   return(plot_EDA3)
  # })
  # height_EDA3 <- reactive({gg_facet_nrow(plot_EDA3())})
  # #he <- reactive({if(!is.null(plot_EDA3())) gg_facet_nrow(plot_EDA3())})
  # output$gg_marketinjury <- renderPlot({
  #   plot_EDA3()
  # },
  # # height = function() {if(!is.null(plot_EDA3())) he()*300}
  # # height = function() height_EDA3()*200-((height_EDA3()-1)*10)
  # height = function() height_EDA3()*200              #how can i have fixed panel sizes.... check: egg::set_panel_size https://cran.r-project.org/web/packages/egg/egg.pdf
  # )
  
  
  
  # Inference ---------------------------------------------------------------
  
  ## PARAMETRIC
  
  ## ONE MEAN
  
  output$results_onemean1 <- renderUI({
    
    ### data filters
    
    #### season
    idx <- match(c(input$inf_season_filter[1],input$inf_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$inf_minute_filter[1] & 
                                        players_df$minutes_played <= input$inf_minute_filter[2]))
    #### league
    if (input$inf_league_filter != "All"){
      players_df <- subset(players_df, league %in% input$inf_league_filter)
    }
    
    #### age
    players_df <- subset(players_df, (players_df$age >= input$inf_age_filter[1] & 
                                        players_df$age <= input$inf_age_filter[2]))
    
    #### height
    players_df <- subset(players_df, (players_df$height >= input$inf_height_filter[1] & 
                                        players_df$height <= input$inf_height_filter[2]))
    
    #### position
    if (input$inf_position_filter != "All"){
      players_df <- subset(players_df, position %in% input$inf_position_filter)
    }
    
    #### position2
    if (input$inf_position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$inf_position2_filter)
    }
    
    #### foot
    if (input$inf_foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$inf_foot_filter)
    }
    
    #### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$inf_month_filter[1] & 
                                        players_df$`birth month` <= input$inf_month_filter[2]))
    
    #### output
    data <- players_df$market_value
    if (length(data) < 2){
      "There are not enough obervations with the selected filters. Please try
      again with another combination of them."
    } else {
      if (length(data) < 30){
        withMathJax(
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample size,  \\(n =\\) ", length(data)),
          br(),
          br(),
          paste0("The parametric t-test may not be valid with number of obervations with 
          the selected filters (<30) due to the lackness of nomrality.  We suggest 
          to try the non-parametric test or try again with another combination of
          the filters."),
          br(),
          br(),
          paste0("Sample mean,  \\(\\bar{x} =\\) ", round(mean(data, na.rm = TRUE), 2)),
          br(),
          paste0("Sample standard deviation,  \\(s =\\) ", round(sd(data, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      } else {
        withMathJax(
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample size,  \\(n =\\) ", length(data)),
          br(),
          paste0("Sample mean,  \\(\\bar{x} =\\) ", round(mean(data, na.rm = TRUE), 2)),
          br(),
          paste0("Sample standard deviation,  \\(s =\\) ", round(sd(data, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      }
      
    }
  })
  
  
  ## TWO MEANS IND
  
  output$results_twomeans_ind1 <- renderUI({
    
    ### data filters 1
    
    #### season
    idx <- match(c(input$inf_season_filter_ind1[1],input$inf_season_filter_ind1[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data1 <- subset(players_df, season %in% seasons[idx])
    data1$season <- droplevels(data1$season)
    
    #### minutes_played
    data1 <- subset(data1, (data1$minutes_played >= input$inf_minute_filter_ind1[1] & 
                              data1$minutes_played <= input$inf_minute_filter_ind1[2]))
    #### league
    if (input$inf_league_filter_ind1 != "All"){
      data1 <- subset(data1, league %in% input$inf_league_filter_ind1)
    }
    
    #### age
    data1 <- subset(data1, (data1$age >= input$inf_age_filter_ind1[1] & 
                              data1$age <= input$inf_age_filter_ind1[2]))
    
    #### height
    data1 <- subset(data1, (data1$height >= input$inf_height_filter_ind1[1] & 
                              data1$height <= input$inf_height_filter_ind1[2]))
    
    #### position
    if (input$inf_position_filter_ind1 != "All"){
      data1 <- subset(data1, position %in% input$inf_position_filter_ind1)
    }
    
    #### position2
    if (input$inf_position2_filter_ind1 != "All"){
      data1 <- subset(data1, position2 %in% input$inf_position2_filter_ind1)
    }
    
    #### foot
    if (input$inf_foot_filter_ind1 != "All"){
      data1 <- subset(data1, foot %in% input$inf_foot_filter_ind1)
    }
    
    #### birth month
    data1$`birth month` <- as.numeric(format(data1$birth,"%m"))
    data1 <- subset(data1, (data1$`birth month` >= input$inf_month_filter_ind1[1] & 
                              data1$`birth month` <= input$inf_month_filter_ind1[2]))
    
    
    ### data filters 2
    
    #### season
    idx <- match(c(input$inf_season_filter_ind2[1],input$inf_season_filter_ind2[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data2 <- subset(players_df, season %in% seasons[idx])
    data2$season <- droplevels(data2$season)
    
    #### minutes_played
    data2 <- subset(data2, (data2$minutes_played >= input$inf_minute_filter_ind2[1] & 
                              data2$minutes_played <= input$inf_minute_filter_ind2[2]))
    #### league
    if (input$inf_league_filter_ind2 != "All"){
      data2 <- subset(data2, league %in% input$inf_league_filter_ind2)
    }
    
    #### age
    data2 <- subset(data2, (data2$age >= input$inf_age_filter_ind2[1] & 
                              data2$age <= input$inf_age_filter_ind2[2]))
    
    #### height
    data2 <- subset(data2, (data2$height >= input$inf_height_filter_ind2[1] & 
                              data2$height <= input$inf_height_filter_ind2[2]))
    
    #### position
    if (input$inf_position_filter_ind2 != "All"){
      data2 <- subset(data2, position %in% input$inf_position_filter_ind2)
    }
    
    #### position2
    if (input$inf_position2_filter_ind2 != "All"){
      data2 <- subset(data2, position2 %in% input$inf_position2_filter_ind2)
    }
    
    #### foot
    if (input$inf_foot_filter_ind2 != "All"){
      data2 <- subset(data2, foot %in% input$inf_foot_filter_ind2)
    }
    
    #### birth month
    data2$`birth month` <- as.numeric(format(data2$birth,"%m"))
    data2 <- subset(data2, (data2$`birth month` >= input$inf_month_filter_ind2[1] & 
                              data2$`birth month` <= input$inf_month_filter_ind2[2]))
    
    
    #### output
    data1 <- data1$market_value
    data2 <- data2$market_value
    if (length(data1) < 2 | length(data2) < 2){
      "There are not enough obervations with the selected filters in at least one
      of the samples. Please try again with another combination of them."
    } else {
      if (length(data1) < 30 | length(data2) < 30){
        withMathJax(
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample 1 size,  \\(n_1 =\\) ", length(data1)),
          br(),
          paste0("Sample 2 size,  \\(n_2 =\\) ", length(data2)),
          br(),
          br(),
          paste0("The parametric t-test may not be valid with number of obervations 
          (<30) with the selected filters due to the lackness of nomrality. We suggest 
          to try the nonparametric test or try again with another combination of
          the filters."),
          br(),
          br(),
          paste0("Sample 1 mean,  \\(\\bar{x_1} =\\) ", round(mean(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 mean,  \\(\\bar{x_2} =\\) ", round(mean(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          paste0("Sample 1 standard deviation,  \\(s_1 =\\) ", round(sd(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 standard deviation,  \\(s_2 =\\) ", round(sd(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      } else {
        withMathJax(
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample 1 size,  \\(n_1 =\\) ", length(data1)),
          br(),
          paste0("Sample 2 size,  \\(n_2 =\\) ", length(data2)),
          br(),
          br(),
          paste0("Sample 1 mean,  \\(\\bar{x_1} =\\) ", round(mean(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 mean,  \\(\\bar{x_2} =\\) ", round(mean(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          paste0("Sample 1 standard deviation,  \\(s_1 =\\) ", round(sd(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 standard deviation,  \\(s_2 =\\) ", round(sd(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      }
      
    }
  })
  
  
  ## ONE MEAN
  
  output$plot_market_value_distr_onemean <- renderPlot({
    
    ### data filters
    
    #### season
    idx <- match(c(input$inf_season_filter[1],input$inf_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$inf_minute_filter[1] & 
                                        players_df$minutes_played <= input$inf_minute_filter[2]))
    #### league
    if (input$inf_league_filter != "All"){
      players_df <- subset(players_df, league %in% input$inf_league_filter)
    }
    
    #### age
    players_df <- subset(players_df, (players_df$age >= input$inf_age_filter[1] & 
                                        players_df$age <= input$inf_age_filter[2]))
    
    #### height
    players_df <- subset(players_df, (players_df$height >= input$inf_height_filter[1] & 
                                        players_df$height <= input$inf_height_filter[2]))
    
    #### position
    if (input$inf_position_filter != "All"){
      players_df <- subset(players_df, position %in% input$inf_position_filter)
    }
    
    #### position2
    if (input$inf_position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$inf_position2_filter)
    }
    
    #### foot
    if (input$inf_foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$inf_foot_filter)
    }
    
    #### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$inf_month_filter[1] & 
                                        players_df$`birth month` <= input$inf_month_filter[2]))
    
    #### output
    data <- players_df$market_value
    if (length(data) > 2){
      g1 <- ggplot(players_df, aes(x=market_value)) +
        geom_histogram(aes(y = ..density..), color="darkblue", fill="lightblue") +
        geom_density(colour="darkblue", fill="lightblue", alpha=0.25, adjust=5) +
        geom_vline(aes(xintercept = mean(market_value, na.rm = TRUE)),
                   color="darkblue", size=0.8) + 
        geom_vline(aes(xintercept = input$null_h0), color="red", 
                   linetype = "dashed", size = 0.8)
      g2 <- ggplot(players_df, aes(sample = market_value)) +
        stat_qq(color="darkblue") +
        stat_qq_line(color="darkblue") +
        xlab("Normal theoretical quantiles") + ylab("Sample data quantiles")
      ggarrange(g1, g2,
                ncol = 2, nrow = 1)
    }
  })
  
  
  ## TWO MEANS IND
  
  output$plot_market_value_distr_ind <- renderPlot({
    
    ### data filters 1
    
    #### season
    idx <- match(c(input$inf_season_filter_ind1[1],input$inf_season_filter_ind1[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data1 <- subset(players_df, season %in% seasons[idx])
    data1$season <- droplevels(data1$season)
    
    #### minutes_played
    data1 <- subset(data1, (data1$minutes_played >= input$inf_minute_filter_ind1[1] & 
                              data1$minutes_played <= input$inf_minute_filter_ind1[2]))
    #### league
    if (input$inf_league_filter_ind1 != "All"){
      data1 <- subset(data1, league %in% input$inf_league_filter_ind1)
    }
    
    #### age
    data1 <- subset(data1, (data1$age >= input$inf_age_filter_ind1[1] & 
                              data1$age <= input$inf_age_filter_ind1[2]))
    
    #### height
    data1 <- subset(data1, (data1$height >= input$inf_height_filter_ind1[1] & 
                              data1$height <= input$inf_height_filter_ind1[2]))
    
    #### position
    if (input$inf_position_filter_ind1 != "All"){
      data1 <- subset(data1, position %in% input$inf_position_filter_ind1)
    }
    
    #### position2
    if (input$inf_position2_filter_ind1 != "All"){
      data1 <- subset(data1, position2 %in% input$inf_position2_filter_ind1)
    }
    
    #### foot
    if (input$inf_foot_filter_ind1 != "All"){
      data1 <- subset(data1, foot %in% input$inf_foot_filter_ind1)
    }
    
    #### birth month
    data1$`birth month` <- as.numeric(format(data1$birth,"%m"))
    data1 <- subset(data1, (data1$`birth month` >= input$inf_month_filter_ind1[1] & 
                              data1$`birth month` <= input$inf_month_filter_ind1[2]))
    
    
    ### data filters 2
    
    #### season
    idx <- match(c(input$inf_season_filter_ind2[1],input$inf_season_filter_ind2[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data2 <- subset(players_df, season %in% seasons[idx])
    data2$season <- droplevels(data2$season)
    
    #### minutes_played
    data2 <- subset(data2, (data2$minutes_played >= input$inf_minute_filter_ind2[1] & 
                              data2$minutes_played <= input$inf_minute_filter_ind2[2]))
    #### league
    if (input$inf_league_filter_ind2 != "All"){
      data2 <- subset(data2, league %in% input$inf_league_filter_ind2)
    }
    
    #### age
    data2 <- subset(data2, (data2$age >= input$inf_age_filter_ind2[1] & 
                              data2$age <= input$inf_age_filter_ind2[2]))
    
    #### height
    data2 <- subset(data2, (data2$height >= input$inf_height_filter_ind2[1] & 
                              data2$height <= input$inf_height_filter_ind2[2]))
    
    #### position
    if (input$inf_position_filter_ind2 != "All"){
      data2 <- subset(data2, position %in% input$inf_position_filter_ind2)
    }
    
    #### position2
    if (input$inf_position2_filter_ind2 != "All"){
      data2 <- subset(data2, position2 %in% input$inf_position2_filter_ind2)
    }
    
    #### foot
    if (input$inf_foot_filter_ind2 != "All"){
      data2 <- subset(data2, foot %in% input$inf_foot_filter_ind2)
    }
    
    #### birth month
    data2$`birth month` <- as.numeric(format(data2$birth,"%m"))
    data2 <- subset(data2, (data2$`birth month` >= input$inf_month_filter_ind2[1] & 
                              data2$`birth month` <= input$inf_month_filter_ind2[2]))
    
    
    #### output
    data1_mv <- data1$market_value
    data2_mv <- data2$market_value
    if (length(data1_mv) > 2 & length(data2_mv) > 2){
      g11 <- ggplot(data1, aes(x=market_value)) +
        geom_histogram(aes(y = ..density..), color="darkblue", fill="lightblue") +
        geom_density(colour="darkblue", fill="lightblue", alpha=0.25, adjust=5) +
        geom_vline(aes(xintercept = mean(market_value, na.rm = TRUE)),
                   color="darkblue", size=0.8) +
        ylab("Sample 1 market value")
      g12 <- ggplot(data1, aes(sample = market_value)) +
        stat_qq(color="darkblue") +
        stat_qq_line(color="darkblue") +
        xlab("Normal theoretical quantiles") + ylab("Sample 1 data quantiles")
      g21 <- ggplot(data2, aes(x=market_value)) +
        geom_histogram(aes(y = ..density..), color="darkgreen", fill="lightgreen") +
        geom_density(colour="darkgreen", fill="lightgreen", alpha=0.25, adjust=5) +
        geom_vline(aes(xintercept = mean(market_value, na.rm = TRUE)),
                   color="darkgreen", size=0.8) +
        ylab("Sample 2 market value")
      g22 <- ggplot(data2, aes(sample = market_value)) +
        stat_qq(color="darkgreen") +
        stat_qq_line(color="darkgreen") +
        xlab("Normal theoretical quantiles") + ylab("Sample 2 data quantiles")
      ggarrange(g11, g12, g21, g22,
                ncol = 2, nrow = 2)
    }
  })
  
  ## ONE MEAN
  
  output$results_onemean2 <- renderUI({
    
    ### data filters
    
    #### season
    idx <- match(c(input$inf_season_filter[1],input$inf_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$inf_minute_filter[1] & 
                                        players_df$minutes_played <= input$inf_minute_filter[2]))
    #### league
    if (input$inf_league_filter != "All"){
      players_df <- subset(players_df, league %in% input$inf_league_filter)
    }
    
    #### age
    players_df <- subset(players_df, (players_df$age >= input$inf_age_filter[1] & 
                                        players_df$age <= input$inf_age_filter[2]))
    
    #### height
    players_df <- subset(players_df, (players_df$height >= input$inf_height_filter[1] & 
                                        players_df$height <= input$inf_height_filter[2]))
    
    #### position
    if (input$inf_position_filter != "All"){
      players_df <- subset(players_df, position %in% input$inf_position_filter)
    }
    
    #### position2
    if (input$inf_position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$inf_position2_filter)
    }
    
    #### foot
    if (input$inf_foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$inf_foot_filter)
    }
    
    #### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$inf_month_filter[1] & 
                                        players_df$`birth month` <= input$inf_month_filter[2]))
    
    data <- players_df$market_value
    
    #### output
    if (length(data) > 2){
      
      t_test <- t.test(x = data, mu = input$null_h0, alternative = input$alt_h1,
                       conf.level = 1 - input$alpha)
      
      withMathJax(
        paste0("Althought the data is/was not normally distributed, the t-test 
             that will be performed it does not assume that the population is
             normally distributed. In fact, it assumes that the means of different
             samples of a particular size that can be taken from a population are
             normally distributed. So, as long as the sample size is at least 30
             the t-test is relatively robust for moderate violations of the normality
             assumtion. And in large enough sample sizes, the test is robust even
             to large deviations from normality."),
        br(),
        br(),
        tags$b("Hypothesis test:"),
        br(),
        br(),
        paste0("\\(H_0 : \\mu = \\)", input$null_h0),
        br(),
        paste0("\\(H_1 : \\mu \\)", ifelse(input$alt_h1 == "two.sided", "\\( \\neq \\) ",
                                           ifelse(input$alt_h1 == "greater", "\\( > \\) ",
                                                  "\\( < \\) ")), input$null_h0),
        br(),
        br(),
        tags$b("Result of the t-test:"),
        br(),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(t_test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), t_test$null.value, " \\((p\\)-value ", ifelse(t_test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t_test$p.value, 3))), ")", ".")
        
        
      )
    }
  })
  
  ## TWO MEANS IND
  
  output$results_twomeans_ind2 <- renderUI({
    
    ### data filters 1
    
    #### season
    idx <- match(c(input$inf_season_filter_ind1[1],input$inf_season_filter_ind1[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data1 <- subset(players_df, season %in% seasons[idx])
    data1$season <- droplevels(data1$season)
    
    #### minutes_played
    data1 <- subset(data1, (data1$minutes_played >= input$inf_minute_filter_ind1[1] & 
                              data1$minutes_played <= input$inf_minute_filter_ind1[2]))
    #### league
    if (input$inf_league_filter_ind1 != "All"){
      data1 <- subset(data1, league %in% input$inf_league_filter_ind1)
    }
    
    #### age
    data1 <- subset(data1, (data1$age >= input$inf_age_filter_ind1[1] & 
                              data1$age <= input$inf_age_filter_ind1[2]))
    
    #### height
    data1 <- subset(data1, (data1$height >= input$inf_height_filter_ind1[1] & 
                              data1$height <= input$inf_height_filter_ind1[2]))
    
    #### position
    if (input$inf_position_filter_ind1 != "All"){
      data1 <- subset(data1, position %in% input$inf_position_filter_ind1)
    }
    
    #### position2
    if (input$inf_position2_filter_ind1 != "All"){
      data1 <- subset(data1, position2 %in% input$inf_position2_filter_ind1)
    }
    
    #### foot
    if (input$inf_foot_filter_ind1 != "All"){
      data1 <- subset(data1, foot %in% input$inf_foot_filter_ind1)
    }
    
    #### birth month
    data1$`birth month` <- as.numeric(format(data1$birth,"%m"))
    data1 <- subset(data1, (data1$`birth month` >= input$inf_month_filter_ind1[1] & 
                              data1$`birth month` <= input$inf_month_filter_ind1[2]))
    
    
    ### data filters 2
    
    #### season
    idx <- match(c(input$inf_season_filter_ind2[1],input$inf_season_filter_ind2[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data2 <- subset(players_df, season %in% seasons[idx])
    data2$season <- droplevels(data2$season)
    
    #### minutes_played
    data2 <- subset(data2, (data2$minutes_played >= input$inf_minute_filter_ind2[1] & 
                              data2$minutes_played <= input$inf_minute_filter_ind2[2]))
    #### league
    if (input$inf_league_filter_ind2 != "All"){
      data2 <- subset(data2, league %in% input$inf_league_filter_ind2)
    }
    
    #### age
    data2 <- subset(data2, (data2$age >= input$inf_age_filter_ind2[1] & 
                              data2$age <= input$inf_age_filter_ind2[2]))
    
    #### height
    data2 <- subset(data2, (data2$height >= input$inf_height_filter_ind2[1] & 
                              data2$height <= input$inf_height_filter_ind2[2]))
    
    #### position
    if (input$inf_position_filter_ind2 != "All"){
      data2 <- subset(data2, position %in% input$inf_position_filter_ind2)
    }
    
    #### position2
    if (input$inf_position2_filter_ind2 != "All"){
      data2 <- subset(data2, position2 %in% input$inf_position2_filter_ind2)
    }
    
    #### foot
    if (input$inf_foot_filter_ind2 != "All"){
      data2 <- subset(data2, foot %in% input$inf_foot_filter_ind2)
    }
    
    #### birth month
    data2$`birth month` <- as.numeric(format(data2$birth,"%m"))
    data2 <- subset(data2, (data2$`birth month` >= input$inf_month_filter_ind2[1] & 
                              data2$`birth month` <= input$inf_month_filter_ind2[2]))
    
    
    #### output
    data1 <- data1$market_value
    data2 <- data2$market_value
    
    if (length(data1) > 2 & length(data2) > 2){
      
      t_test <- t.test(x = data1, y = data2, mu = input$null_h0_ind, alternative = input$alt_h1_ind,
                       conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      
      withMathJax(
        paste0("Althought the data is/was not normally distributed, the t-test 
             that will be performed it does not assume that the population is
             normally distributed. In fact, it assumes that the means of different
             samples of a particular size that can be taken from a population are
             normally distributed. So, as long as the sample size is at least 30
             the t-test is relatively robust for moderate violations of the normality
             assumtion and in large enough sample sizes, the test is robust even to large
             deviations from normality."),
        br(),
        br(),
        tags$b("Hypothesis test:"),
        br(),
        br(),
        paste0("\\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$null_h0_ind),
        br(),
        paste0("\\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alt_h1_ind == "two.sided", "\\( \\neq \\) ",
                                                       ifelse(input$alt_h1_ind == "greater", "\\( > \\) ",
                                                              "\\( < \\) ")), input$null_h0_ind),
        br(),
        br(),
        tags$b("Result of the t-test:"),
        br(),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(t_test$p.value < input$alpha, "we reject the null hypothesis that the difference between the two means is equal ", "we do not reject the null hypothesis that the difference between the two means is equal "), t_test$null.value, " \\((p\\)-value ", ifelse(t_test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t_test$p.value, 3))), ")", ".")
        
        
      )
    }
  })
  
  ## NONPARAMETRIC
  
  ## ONE SAMPLE
  
  output$results_onesample1 <- renderUI({
    
    ### data filters
    
    #### season
    idx <- match(c(input$inf_season_filter[1],input$inf_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$inf_minute_filter[1] & 
                                        players_df$minutes_played <= input$inf_minute_filter[2]))
    #### league
    if (input$inf_league_filter != "All"){
      players_df <- subset(players_df, league %in% input$inf_league_filter)
    }
    
    #### age
    players_df <- subset(players_df, (players_df$age >= input$inf_age_filter[1] & 
                                        players_df$age <= input$inf_age_filter[2]))
    
    #### height
    players_df <- subset(players_df, (players_df$height >= input$inf_height_filter[1] & 
                                        players_df$height <= input$inf_height_filter[2]))
    
    #### position
    if (input$inf_position_filter != "All"){
      players_df <- subset(players_df, position %in% input$inf_position_filter)
    }
    
    #### position2
    if (input$inf_position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$inf_position2_filter)
    }
    
    #### foot
    if (input$inf_foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$inf_foot_filter)
    }
    
    #### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$inf_month_filter[1] & 
                                        players_df$`birth month` <= input$inf_month_filter[2]))
    
    #### output
    data <- players_df$market_value
    if (length(data) < 2){
      "There are not enough obervations with the selected filters. Please try
      again with another combination of them."
    } else {
      if (length(data) > 30){
        withMathJax(
          br(),
          br(),
          tags$b("Important information about the nonparametric Wilcoxon test:"),
          br(),
          br(),
          paste0("The nonparametric Wilcoxon/Wilcoxon-Mann-Withney test is frequently 
          used as an alternative to the parametric t-test when the assumtions 
          are not satisfied (more or less our case). However, it is important to 
          know that the hypotheses of this test are not the same as the t-test ones. 
          In fact, for this particular case, the Wilcoxon test is used to compare 
          the sample observations to a given default value. In terms of null and 
          alternative hypotheses (for the two-tailed test) we have that:"),
          br(),
          br(),
          paste0("\\(H_0 : \\)", " the location of the data is equal to the chosen value"),
          br(),
          paste0("\\(H_1 : \\)", " the location of the data is different to the chosen value"),
          br(),
          br(),
          paste0("And in the particular case that the data are symmetric, the test 
          can be reformulated as:"),
          br(),
          br(),
          paste0("\\(H_0 : \\)", " the median of the data is equal to the chosen value"),
          br(),
          paste0("\\(H_1 : \\)", " the median of the data is different to the chosen value"),
          br(),
          br(),
          paste0("Having understood this, the test can be done and interpreted in a correct way."),
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample size,  \\(n =\\) ", length(data)),
          br(),
          br(),
          paste0("The parametric t-test may be more powerful with the number of obervations with 
          the selected filters (>30) as the parametric assumptions are met. So, we suggest 
          the parametric test if the goal is to compare the mean(s). On the contrary, 
          if the aim is to compare the population(s) in a global way, or try to compare 
          the median(s), the nonparametric test may be appropriate."),
          br(),
          br(),
          paste0("Sample median,  \\(med_x =\\) ", round(median(data, na.rm = TRUE), 2)),
          br(),
          paste0("Sample standard deviation,  \\(s =\\) ", round(sd(data, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      } else {
        withMathJax(
          br(),
          br(),
          tags$b("Important information about the nonparametric Wilcoxon test:"),
          br(),
          br(),
          paste0("The nonparametric Wilcoxon/Wilcoxon-Mann-Withney test is frequently 
          used as an alternative to the parametric t-test when the assumtions 
          are not satisfied (more or less our case). However, it is important to 
          know that the hypotheses of this test are not the same as the t-test ones. 
          In fact, for this particular case, the Wilcoxon test is used to compare 
          the sample observations to a given default value. In terms of null and 
          alternative hypotheses (for the two-tailed test) we have that:"),
          br(),
          br(),
          paste0("\\(H_0 : \\)", " the location of the data is equal to the chosen value"),
          br(),
          paste0("\\(H_1 : \\)", " the location of the data is different to the chosen value"),
          br(),
          br(),
          paste0("And in the particular case that the data are symmetric, the test 
          can be reformulated as:"),
          br(),
          br(),
          paste0("\\(H_0 : \\)", " the median of the data is equal to the chosen value"),
          br(),
          paste0("\\(H_1 : \\)", " the median of the data is different to the chosen value"),
          br(),
          br(),
          paste0("Having understood this, the test can be done and interpreted in a correct way."),
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample size,  \\(n =\\) ", length(data)),
          br(),
          paste0("Sample median,  \\(med_x =\\) ", round(median(data, na.rm = TRUE), 2)),
          br(),
          paste0("Sample standard deviation,  \\(s =\\) ", round(sd(data, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      }
      
    }
  })
  
  ## TWO SAMPLES IND
  
  output$results_twosamples_ind1 <- renderUI({
    
    ### data filters 1
    
    #### season
    idx <- match(c(input$inf_season_filter_ind1[1],input$inf_season_filter_ind1[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data1 <- subset(players_df, season %in% seasons[idx])
    data1$season <- droplevels(data1$season)
    
    #### minutes_played
    data1 <- subset(data1, (data1$minutes_played >= input$inf_minute_filter_ind1[1] & 
                              data1$minutes_played <= input$inf_minute_filter_ind1[2]))
    #### league
    if (input$inf_league_filter_ind1 != "All"){
      data1 <- subset(data1, league %in% input$inf_league_filter_ind1)
    }
    
    #### age
    data1 <- subset(data1, (data1$age >= input$inf_age_filter_ind1[1] & 
                              data1$age <= input$inf_age_filter_ind1[2]))
    
    #### height
    data1 <- subset(data1, (data1$height >= input$inf_height_filter_ind1[1] & 
                              data1$height <= input$inf_height_filter_ind1[2]))
    
    #### position
    if (input$inf_position_filter_ind1 != "All"){
      data1 <- subset(data1, position %in% input$inf_position_filter_ind1)
    }
    
    #### position2
    if (input$inf_position2_filter_ind1 != "All"){
      data1 <- subset(data1, position2 %in% input$inf_position2_filter_ind1)
    }
    
    #### foot
    if (input$inf_foot_filter_ind1 != "All"){
      data1 <- subset(data1, foot %in% input$inf_foot_filter_ind1)
    }
    
    #### birth month
    data1$`birth month` <- as.numeric(format(data1$birth,"%m"))
    data1 <- subset(data1, (data1$`birth month` >= input$inf_month_filter_ind1[1] & 
                              data1$`birth month` <= input$inf_month_filter_ind1[2]))
    
    
    ### data filters 2
    
    #### season
    idx <- match(c(input$inf_season_filter_ind2[1],input$inf_season_filter_ind2[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data2 <- subset(players_df, season %in% seasons[idx])
    data2$season <- droplevels(data2$season)
    
    #### minutes_played
    data2 <- subset(data2, (data2$minutes_played >= input$inf_minute_filter_ind2[1] & 
                              data2$minutes_played <= input$inf_minute_filter_ind2[2]))
    #### league
    if (input$inf_league_filter_ind2 != "All"){
      data2 <- subset(data2, league %in% input$inf_league_filter_ind2)
    }
    
    #### age
    data2 <- subset(data2, (data2$age >= input$inf_age_filter_ind2[1] & 
                              data2$age <= input$inf_age_filter_ind2[2]))
    
    #### height
    data2 <- subset(data2, (data2$height >= input$inf_height_filter_ind2[1] & 
                              data2$height <= input$inf_height_filter_ind2[2]))
    
    #### position
    if (input$inf_position_filter_ind2 != "All"){
      data2 <- subset(data2, position %in% input$inf_position_filter_ind2)
    }
    
    #### position2
    if (input$inf_position2_filter_ind2 != "All"){
      data2 <- subset(data2, position2 %in% input$inf_position2_filter_ind2)
    }
    
    #### foot
    if (input$inf_foot_filter_ind2 != "All"){
      data2 <- subset(data2, foot %in% input$inf_foot_filter_ind2)
    }
    
    #### birth month
    data2$`birth month` <- as.numeric(format(data2$birth,"%m"))
    data2 <- subset(data2, (data2$`birth month` >= input$inf_month_filter_ind2[1] & 
                              data2$`birth month` <= input$inf_month_filter_ind2[2]))
    
    
    #### output
    data1 <- data1$market_value
    data2 <- data2$market_value
    if (length(data1) < 2 | length(data2) < 2){
      "There are not enough obervations with the selected filters. Please try
      again with another combination of them."
    } else {
      if (length(data1) > 30 & length(data2) > 30){
        withMathJax(
          br(),
          br(),
          tags$b("Important information about the nonparametric Wilcoxon test:"),
          br(),
          br(),
          paste0("The nonparametric Wilcoxon/Wilcoxon-Mann-Withney test is frequently 
          used as an alternative to the parametric t-test when the assumtions 
          are not satisfied (more or less our case). However, it is important to 
          know that the hypotheses of this test are not the same as the t-test ones. 
          In fact, for this particular case, the Wilcoxon test is used to compare 
          the distributions of both populations. More precisely, the test detects 
          whether one of the populations is stochastically greater than the other.
          In terms of null and alternative hypotheses (for the two-tailed test) we have that:"),
          br(),
          br(),
          paste0("\\(H_0 : P(X_1>X_2)=P(X_2>X_1)\\)", "; or the distributions of both populations are identical"),
          br(),
          paste0("\\(H_1 : P(X_1>X_2) \\neq P(X_2>X_1)\\)", "; or the distributions of both populations are not identical"),
          br(),
          br(),
          paste0("And in the particular case that the both populations only differ in the 
          location (i. e. have the the same shape, disperssion, assymetry, variance...), the test 
          can be reformulated as:"),
          br(),
          br(),
          paste0("\\(H_0 : \\)", " the median of the two populations are equal"),
          br(),
          paste0("\\(H_1 : \\)", " the median of the two populations are not equal"),
          br(),
          br(),
          paste0("Having understood this, the test can be done and interpreted in a correct way."),
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample 1 size,  \\(n_1 =\\) ", length(data1)),
          br(),
          paste0("Sample 2 size,  \\(n_2 =\\) ", length(data2)),
          br(),
          br(),
          paste0("The parametric t-test may be more powerful with the number of obervations with 
          the selected filters (>30) as the parametric assumptions are met. So, we suggest 
          the parametric test if the goal is to compare the mean(s). On the contrary, 
          if the aim is to compare the population(s) in a global way, or try to compare 
          the median(s), the nonparametric test may be appropriate."),
          br(),
          br(),
          paste0("Sample 1 median,  \\(med_{X_1} =\\) ", round(median(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 median,  \\(med_{X_2} =\\) ", round(median(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          paste0("Sample 1 standard deviation,  \\(s_1 =\\) ", round(sd(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 standard deviation,  \\(s_2 =\\) ", round(sd(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      } else {
        withMathJax(
          br(),
          br(),
          tags$b("Important information about the nonparametric Wilcoxon test:"),
          br(),
          br(),
          paste0("The nonparametric Wilcoxon/Wilcoxon-Mann-Withney test is frequently 
          used as an alternative to the parametric t-test when the assumtions 
          are not satisfied (more or less our case). However, it is important to 
          know that the hypotheses of this test are not the same as the t-test ones. 
          In fact, for this particular case, the Wilcoxon test is used to compare 
          the distributions of both populations. More precisely, the test detects 
          whether one of the populations is stochastically greater than the other.
          In terms of null and alternative hypotheses (for the two-tailed test) we have that:"),
          br(),
          br(),
          paste0("\\(H_0 : P(X_1>X_2)=P(X_2>X_1)\\)", "; or the distributions of both populations are identical"),
          br(),
          paste0("\\(H_1 : P(X_1>X_2) \\neq P(X_2>X_1)\\)", "; or the distributions of both populations are not identical"),
          br(),
          br(),
          paste0("And in the particular case that the both populations only differ in the 
          location (i. e. have the the same shape, disperssion, assymetry, variance...), the test 
          can be reformulated as:"),
          br(),
          br(),
          paste0("\\(H_0 : \\)", " the median of the two populations are equal"),
          br(),
          paste0("\\(H_1 : \\)", " the median of the two populations are not equal"),
          br(),
          br(),
          paste0("Having understood this, the test can be done and interpreted in a correct way."),
          br(),
          br(),
          tags$b("About the filtered data:"),
          br(),
          br(),
          paste0("Sample 1 size,  \\(n_1 =\\) ", length(data1)),
          br(),
          paste0("Sample 2 size,  \\(n_2 =\\) ", length(data2)),
          br(),
          br(),
          paste0("Sample 1 median,  \\(med_{X_1} =\\) ", round(median(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 median,  \\(med_{X_2} =\\) ", round(median(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          paste0("Sample 1 standard deviation,  \\(s_1 =\\) ", round(sd(data1, na.rm = TRUE), 2)),
          br(),
          paste0("Sample 2 standard deviation,  \\(s_2 =\\) ", round(sd(data2, na.rm = TRUE), 2)),
          br(),
          br(),
          tags$b("The distribution of the filtered data:")
        )
      }
      
    }
  })
  
  ## ONE SAMPLE
  
  output$plot_market_value_distr_onesample <- renderPlot({
    
    ### data filters
    
    #### season
    idx <- match(c(input$inf_season_filter[1],input$inf_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$inf_minute_filter[1] & 
                                        players_df$minutes_played <= input$inf_minute_filter[2]))
    #### league
    if (input$inf_league_filter != "All"){
      players_df <- subset(players_df, league %in% input$inf_league_filter)
    }
    
    #### age
    players_df <- subset(players_df, (players_df$age >= input$inf_age_filter[1] & 
                                        players_df$age <= input$inf_age_filter[2]))
    
    #### height
    players_df <- subset(players_df, (players_df$height >= input$inf_height_filter[1] & 
                                        players_df$height <= input$inf_height_filter[2]))
    
    #### position
    if (input$inf_position_filter != "All"){
      players_df <- subset(players_df, position %in% input$inf_position_filter)
    }
    
    #### position2
    if (input$inf_position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$inf_position2_filter)
    }
    
    #### foot
    if (input$inf_foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$inf_foot_filter)
    }
    
    #### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$inf_month_filter[1] & 
                                        players_df$`birth month` <= input$inf_month_filter[2]))
    
    #### output
    data <- players_df$market_value
    if (length(data) > 2){
      g1 <- ggplot(players_df, aes(x=market_value)) +
        geom_histogram(aes(y = ..density..), color="darkblue", fill="lightblue") +
        geom_density(colour="darkblue", fill="lightblue", alpha=0.25, adjust=5) +
        geom_vline(aes(xintercept = median(market_value, na.rm = TRUE)),
                   color="darkblue", size=0.8) + 
        geom_vline(aes(xintercept = input$null_h0), color="red", 
                   linetype = "dashed", size = 0.8) +
        xlab("market value")
      g2 <- ggplot(players_df, aes(x = market_value)) +
        stat_ecdf(geom = "step", colour = "darkblue", pad = FALSE) +
        xlab("market value") + ylab("cumulative distribution") +
        geom_vline(aes(xintercept = median(market_value, na.rm = TRUE)),
                   color="darkblue", size=0.5) +
        geom_hline(aes(yintercept = 0.5), color="black", 
                   linetype = "dashed", size=0.5) + 
        geom_vline(aes(xintercept = input$null_h0), color="red", 
                   linetype = "dashed", size = 0.7)
      ggarrange(g1, g2,
                ncol = 2, nrow = 1)
    }
  })
  
  ## TWO SAMPLES IND
  
  output$plot_market_value_distr_twosamples <- renderPlot({
    
    ### data filters 1
    
    #### season
    idx <- match(c(input$inf_season_filter_ind1[1],input$inf_season_filter_ind1[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data1 <- subset(players_df, season %in% seasons[idx])
    data1$season <- droplevels(data1$season)
    
    #### minutes_played
    data1 <- subset(data1, (data1$minutes_played >= input$inf_minute_filter_ind1[1] & 
                              data1$minutes_played <= input$inf_minute_filter_ind1[2]))
    #### league
    if (input$inf_league_filter_ind1 != "All"){
      data1 <- subset(data1, league %in% input$inf_league_filter_ind1)
    }
    
    #### age
    data1 <- subset(data1, (data1$age >= input$inf_age_filter_ind1[1] & 
                              data1$age <= input$inf_age_filter_ind1[2]))
    
    #### height
    data1 <- subset(data1, (data1$height >= input$inf_height_filter_ind1[1] & 
                              data1$height <= input$inf_height_filter_ind1[2]))
    
    #### position
    if (input$inf_position_filter_ind1 != "All"){
      data1 <- subset(data1, position %in% input$inf_position_filter_ind1)
    }
    
    #### position2
    if (input$inf_position2_filter_ind1 != "All"){
      data1 <- subset(data1, position2 %in% input$inf_position2_filter_ind1)
    }
    
    #### foot
    if (input$inf_foot_filter_ind1 != "All"){
      data1 <- subset(data1, foot %in% input$inf_foot_filter_ind1)
    }
    
    #### birth month
    data1$`birth month` <- as.numeric(format(data1$birth,"%m"))
    data1 <- subset(data1, (data1$`birth month` >= input$inf_month_filter_ind1[1] & 
                              data1$`birth month` <= input$inf_month_filter_ind1[2]))
    
    
    ### data filters 2
    
    #### season
    idx <- match(c(input$inf_season_filter_ind2[1],input$inf_season_filter_ind2[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data2 <- subset(players_df, season %in% seasons[idx])
    data2$season <- droplevels(data2$season)
    
    #### minutes_played
    data2 <- subset(data2, (data2$minutes_played >= input$inf_minute_filter_ind2[1] & 
                              data2$minutes_played <= input$inf_minute_filter_ind2[2]))
    #### league
    if (input$inf_league_filter_ind2 != "All"){
      data2 <- subset(data2, league %in% input$inf_league_filter_ind2)
    }
    
    #### age
    data2 <- subset(data2, (data2$age >= input$inf_age_filter_ind2[1] & 
                              data2$age <= input$inf_age_filter_ind2[2]))
    
    #### height
    data2 <- subset(data2, (data2$height >= input$inf_height_filter_ind2[1] & 
                              data2$height <= input$inf_height_filter_ind2[2]))
    
    #### position
    if (input$inf_position_filter_ind2 != "All"){
      data2 <- subset(data2, position %in% input$inf_position_filter_ind2)
    }
    
    #### position2
    if (input$inf_position2_filter_ind2 != "All"){
      data2 <- subset(data2, position2 %in% input$inf_position2_filter_ind2)
    }
    
    #### foot
    if (input$inf_foot_filter_ind2 != "All"){
      data2 <- subset(data2, foot %in% input$inf_foot_filter_ind2)
    }
    
    #### birth month
    data2$`birth month` <- as.numeric(format(data2$birth,"%m"))
    data2 <- subset(data2, (data2$`birth month` >= input$inf_month_filter_ind2[1] & 
                              data2$`birth month` <= input$inf_month_filter_ind2[2]))
    
    
    #### output
    data1_mv <- data1$market_value
    data2_mv <- data2$market_value
    if (length(data1_mv) > 2 & length(data2_mv) > 2){
      g11 <- ggplot(data1, aes(x=market_value)) +
        geom_histogram(aes(y = ..density..), color="darkblue", fill="lightblue") +
        geom_density(colour="darkblue", fill="lightblue", alpha=0.25, adjust=5) +
        geom_vline(aes(xintercept = median(market_value, na.rm = TRUE)),
                   color="darkblue", size=0.8) +
        ylab("Sample 1 market value")
      g12 <- ggplot(data1, aes(x = market_value)) +
        stat_ecdf(geom = "step", colour = "darkblue", pad = FALSE) +
        xlab("market value") + ylab("Sample 1 cumulative distribution") +
        geom_vline(aes(xintercept = median(market_value, na.rm = TRUE)),
                   color="darkblue", size=0.5) +
        geom_hline(aes(yintercept = 0.5), color="black", 
                   linetype = "dashed", size=0.5)
      g21 <- ggplot(data2, aes(x=market_value)) +
        geom_histogram(aes(y = ..density..), color="darkgreen", fill="lightgreen") +
        geom_density(colour="darkgreen", fill="lightgreen", alpha=0.25, adjust=5) +
        geom_vline(aes(xintercept = median(market_value, na.rm = TRUE)),
                   color="darkgreen", size=0.8) +
        ylab("Sample 2 market value")
      g22 <- ggplot(data2, aes(x = market_value)) +
        stat_ecdf(geom = "step", colour = "darkgreen", pad = FALSE) +
        xlab("market value") + ylab("Sample 2 cumulative distribution") +
        geom_vline(aes(xintercept = median(market_value, na.rm = TRUE)),
                   color="darkgreen", size=0.5) +
        geom_hline(aes(yintercept = 0.5), color="black", 
                   linetype = "dashed", size=0.5)
      ggarrange(g11, g12, g21, g22,
                ncol = 2, nrow = 2)
    }
  })
  
  ## ONE SAMPLE
  
  output$results_onesample2 <- renderUI({
    
    ### data filters
    
    #### season
    idx <- match(c(input$inf_season_filter[1],input$inf_season_filter[2]),seasons)
    idx <- seq(idx[1], idx[2])
    players_df <- subset(players_df, season %in% seasons[idx])
    players_df$season <- droplevels(players_df$season)
    
    #### minutes_played
    players_df <- subset(players_df, (players_df$minutes_played >= input$inf_minute_filter[1] & 
                                        players_df$minutes_played <= input$inf_minute_filter[2]))
    #### league
    if (input$inf_league_filter != "All"){
      players_df <- subset(players_df, league %in% input$inf_league_filter)
    }
    
    #### age
    players_df <- subset(players_df, (players_df$age >= input$inf_age_filter[1] & 
                                        players_df$age <= input$inf_age_filter[2]))
    
    #### height
    players_df <- subset(players_df, (players_df$height >= input$inf_height_filter[1] & 
                                        players_df$height <= input$inf_height_filter[2]))
    
    #### position
    if (input$inf_position_filter != "All"){
      players_df <- subset(players_df, position %in% input$inf_position_filter)
    }
    
    #### position2
    if (input$inf_position2_filter != "All"){
      players_df <- subset(players_df, position2 %in% input$inf_position2_filter)
    }
    
    #### foot
    if (input$inf_foot_filter != "All"){
      players_df <- subset(players_df, foot %in% input$inf_foot_filter)
    }
    
    #### birth month
    players_df$`birth month` <- as.numeric(format(players_df$birth,"%m"))
    players_df <- subset(players_df, (players_df$`birth month` >= input$inf_month_filter[1] & 
                                        players_df$`birth month` <= input$inf_month_filter[2]))
    
    data <- players_df$market_value
    #### output
    if (length(data) > 2){
      
      U_test <- wilcox.test(x = data, mu = input$null_h0, alternative = input$alt_h1,
                            conf.level = 1 - input$alpha)
      
      withMathJax(
        paste0("Taking into acount the distribution showed in the graph above, the
        user should consider which one of the hypotheses is more appropriate or which
        one wants to use in this particular case."),
        br(),
        br(),
        tags$b("Hypothesis test:"),
        br(),
        br(),
        paste0("\\(H_0 : \\)", " the location of the data (or median) is equal to ", input$null_h0),
        br(),
        paste0("\\(H_1 : \\)", " the location of the data (or median) is ", ifelse(input$alt_h1 == "two.sided", "different to ",
                                                                                   ifelse(input$alt_h1 == "greater", "greater than ",
                                                                                          "less than ")), input$null_h0),
        br(),
        br(),
        tags$b("Result of the Wilcoxon test:"),
        br(),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(U_test$p.value < input$alpha, "we reject the null hypothesis ", "we do not reject the null hypothesis "), " \\((p\\)-value ", ifelse(U_test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(U_test$p.value, 3))), ")", "."),
        br(),
        br()
      )
    }
  })
  
  ## TWO SAMPLES (IND)
  
  output$results_twosamples_ind2 <- renderUI({
    
    ### data filters 1
    
    #### season
    idx <- match(c(input$inf_season_filter_ind1[1],input$inf_season_filter_ind1[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data1 <- subset(players_df, season %in% seasons[idx])
    data1$season <- droplevels(data1$season)
    
    #### minutes_played
    data1 <- subset(data1, (data1$minutes_played >= input$inf_minute_filter_ind1[1] & 
                              data1$minutes_played <= input$inf_minute_filter_ind1[2]))
    #### league
    if (input$inf_league_filter_ind1 != "All"){
      data1 <- subset(data1, league %in% input$inf_league_filter_ind1)
    }
    
    #### age
    data1 <- subset(data1, (data1$age >= input$inf_age_filter_ind1[1] & 
                              data1$age <= input$inf_age_filter_ind1[2]))
    
    #### height
    data1 <- subset(data1, (data1$height >= input$inf_height_filter_ind1[1] & 
                              data1$height <= input$inf_height_filter_ind1[2]))
    
    #### position
    if (input$inf_position_filter_ind1 != "All"){
      data1 <- subset(data1, position %in% input$inf_position_filter_ind1)
    }
    
    #### position2
    if (input$inf_position2_filter_ind1 != "All"){
      data1 <- subset(data1, position2 %in% input$inf_position2_filter_ind1)
    }
    
    #### foot
    if (input$inf_foot_filter_ind1 != "All"){
      data1 <- subset(data1, foot %in% input$inf_foot_filter_ind1)
    }
    
    #### birth month
    data1$`birth month` <- as.numeric(format(data1$birth,"%m"))
    data1 <- subset(data1, (data1$`birth month` >= input$inf_month_filter_ind1[1] & 
                              data1$`birth month` <= input$inf_month_filter_ind1[2]))
    
    
    ### data filters 2
    
    #### season
    idx <- match(c(input$inf_season_filter_ind2[1],input$inf_season_filter_ind2[2]),seasons)
    idx <- seq(idx[1], idx[2])
    data2 <- subset(players_df, season %in% seasons[idx])
    data2$season <- droplevels(data2$season)
    
    #### minutes_played
    data2 <- subset(data2, (data2$minutes_played >= input$inf_minute_filter_ind2[1] & 
                              data2$minutes_played <= input$inf_minute_filter_ind2[2]))
    #### league
    if (input$inf_league_filter_ind2 != "All"){
      data2 <- subset(data2, league %in% input$inf_league_filter_ind2)
    }
    
    #### age
    data2 <- subset(data2, (data2$age >= input$inf_age_filter_ind2[1] & 
                              data2$age <= input$inf_age_filter_ind2[2]))
    
    #### height
    data2 <- subset(data2, (data2$height >= input$inf_height_filter_ind2[1] & 
                              data2$height <= input$inf_height_filter_ind2[2]))
    
    #### position
    if (input$inf_position_filter_ind2 != "All"){
      data2 <- subset(data2, position %in% input$inf_position_filter_ind2)
    }
    
    #### position2
    if (input$inf_position2_filter_ind2 != "All"){
      data2 <- subset(data2, position2 %in% input$inf_position2_filter_ind2)
    }
    
    #### foot
    if (input$inf_foot_filter_ind2 != "All"){
      data2 <- subset(data2, foot %in% input$inf_foot_filter_ind2)
    }
    
    #### birth month
    data2$`birth month` <- as.numeric(format(data2$birth,"%m"))
    data2 <- subset(data2, (data2$`birth month` >= input$inf_month_filter_ind2[1] & 
                              data2$`birth month` <= input$inf_month_filter_ind2[2]))
    
    
    data1 <- data1$market_value
    data2 <- data2$market_value
    
    #### output
    if (length(data1) > 2 & length(data2) > 2){
      
      U_test <- wilcox.test(x = data1, y =data2, mu = input$null_h0_ind, alternative = input$alt_h1_ind,
                            conf.level = 1 - input$alpha)
      
      withMathJax(
        paste0("Taking into acount the distributions showed in the graph above, the
        user should consider which one of the hypotheses is more appropriate or which
        one wants to use in this particular case."),
        br(),
        br(),
        tags$b("Hypothesis test:"),
        br(),
        br(),
        paste0("\\(H_0 : \\)", " the distributions (or medians) of both populations are equal"),
        br(),
        paste0("\\(H_1 : \\)", " the distribution (or median) of the first population is ", ifelse(input$alt_h1_ind == "two.sided", "not equal to the second",
                                                                                                   ifelse(input$alt_h1_ind == "greater", "greater than the second",
                                                                                                          "not greater than the second"))),
        
        br(),
        br(),
        tags$b("Result of the Wilcoxon test:"),
        br(),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(U_test$p.value < input$alpha, "we reject the null hypothesis ", "we do not reject the null hypothesis "), " \\((p\\)-value ", ifelse(U_test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(U_test$p.value, 3))), ")", "."),
        br(),
        br()
      )
    }
  })
  
  
  # Statistical Modeling ---------------------------------------------------
  
  output$modeling1 <- renderUI({
    withMathJax(
      tags$b("Data:"),
      br(),
      br(),
      paste0("The statistical model presented in this section has been done to explain
             the market value estimated by Transfermarkt.com. For that purpose, the data
             is filtered such that we can obtain more clear results according to the
             available variables. The filterings done are the following:"),
      br(),
      br(),
      paste0("\\(\\bullet\\) ","Only players with at least 180 minutes played in each season have been included."),
      br(),
      paste0("\\(\\bullet\\) ","Only players with at least 8 consecutive observations have been included."),
      br(),
      paste0("\\(\\bullet\\) ","Goalkeepers have been excluded."),
      br(),
      br(),
      paste0("That way, the data is composed of 7254 observations from 649 different players."),
      br(),
      br(),
      tags$b("Train & Test:"),
      br(),
      br(),
      paste0("Althought the aim of the model is to explain the market value, we also want it to
             be capable of predicting future market values. For that purpose the data is split in
             two sets: the training test and the testing set. That way it is possible to evaluate
             the model for future observations. In this case, the training set consists of all the
             observations until the season 2020/2021 (6987 obs.), and the testing set, consists of
             the observations of the last 2021/2022 season (267 obs.). So, the model presented in
             this section is implemented with the training set."),
      br(),
      br(),
      tags$b("Response variable (market_value):"),
      br(),
      br(),
      paste0("The distribution of the original response variable is not normal as it can
             be seen in the first histogram and qq-plot below. Therefore, the logarithm of the market_value 
             that achieves somehow normality (second histogram and qq-plot below) is used to construct the model:"),
      br(),
      br()
    )
  })
  
  output$plot1 <- renderPlot({
    g11 <- ggplot(train_df, aes(x=market_value)) +
      geom_histogram(aes(y = ..density..), color="darkblue", fill="lightblue") +
      geom_density(colour="darkblue", fill="lightblue", alpha=0.25, adjust=5) +
      ylab("market_value")
    g12 <- ggplot(train_df, aes(sample = market_value)) +
      stat_qq(color="darkblue") +
      stat_qq_line(color="darkblue") +
      xlab("Normal theoretical quantiles") + ylab("market_value data quantiles")
    g21 <- ggplot(train_df, aes(x=log10(market_value))) +
      geom_histogram(aes(y = ..density..), color="darkgreen", fill="lightgreen") +
      geom_density(colour="darkgreen", fill="lightgreen", alpha=0.25, adjust=5) +
      ylab("log10(market_value)")
    g22 <- ggplot(train_df, aes(sample = log10(market_value))) +
      stat_qq(color="darkgreen") +
      stat_qq_line(color="darkgreen") +
      xlab("Normal theoretical quantiles") + ylab("log(market_value) data quantiles")
    ggarrange(g11, g12, g21, g22,
              ncol = 2, nrow = 2)
  })
  
  output$modeling2 <- renderUI({
    withMathJax(
      tags$b("Explanatory variables:"),
      br(),
      br(),
      paste0("The explanatory variables or predictors that are part of the model, explain 
             part of the variation of the market value. These variables are, to a greater 
             or lesser extent, related with the market value of the players and can explain it. 
             The following plots show that relation between the market value and the explanatory
             variables part of the model."),
      br(),
      br()
    )
  })
  
  output$plot_age <- renderPlotly({
    ggplotly(ggplot(train_df, aes(age, log10(market_value))) +
               geom_point() +
               stat_smooth(method='gam', formula = y~s(x)))
  })
  
  output$plot_goals <- renderPlotly({
    ggplotly(ggplot(train_df, aes(goals, log10(market_value))) +
               geom_point() +
               stat_smooth(method='lm'))
  })
  
  output$plot_assists <- renderPlotly({
    ggplotly(ggplot(train_df, aes(assists, log10(market_value))) +
               geom_point() +
               stat_smooth(method='lm'))
  })
  
  output$plot_minutes_played <- renderPlotly({
    ggplotly(ggplot(train_df, aes(minutes_played, log10(market_value))) +
               geom_point() +
               stat_smooth(method='lm'))
  })
  
  output$plot_ranking <- renderPlotly({
    ggplotly(ggplot(train_df, aes(ranking, log10(market_value))) +
               geom_point() +
               stat_smooth(method='lm'))
  })
  
  output$plot_position2 <- renderPlotly({
    ggplotly(ggplot(train_df, aes(x = position2 , y =  log10(market_value), fill = position2)) +
               geom_boxplot())
  })
  
  output$plot_league <- renderPlotly({
    ggplotly(ggplot(train_df, aes(x = league , y =  log10(market_value), fill = league)) +
               geom_boxplot())
  })
  
  output$plot_season <- renderPlotly({
    ggplotly(ggplot(train_df, aes(x = season , y =  log10(market_value), fill = season)) +
               geom_boxplot())
  })
  
  output$modeling3 <- renderUI({
    withMathJax(
      br(),
      br(),
      paste0("So, we can now understand how each of these variables affect the market value of
             a player. The player itself and the club he plays also play an import role in the
             estimation. A player might have a higher market value simply because he plays in a top
             club. Or he might have a higher market value simply fot beeing who he is. For example,
             Leo Messi will have a high market value even if he does not perform well in that season.
             That variability is inside the model by the called 'random effects'"),
      br(),
      br(),
      tags$b("The model:"),
      br(),
      br(),
      paste0("The model implemented to explain the market value of a player is a mixed model.
             So, ome variables are in the model ase 'random effects' and others like 'fixed effects'.
             Without entering in much statistical details, the implemented model is the following,
             where 'bs='re'' menas that that variable is used as random effect."),
      br(),
      br()
    )
  })
  
  output$model <- renderText({"
  Family: gaussian
  Link function: identity

  Formula:
    log10(market_value) ~ s(player_id, bs = 're') + s(age, player_id, bs = 're') + 
                          s(league, bs = 're') + s(club_name, bs = 're') +
                          s(season, bs = 're') + s(age, by = position2) + 
                          minutes_played + goals + assists + ranking + position2

  Parametric coefficients:
                          Estimate Std. Error t value Pr(>|t|)
    (Intercept)          6.409e+00  5.495e-02 116.628  < 2e-16 ***
    minutes_played       1.113e-04  3.208e-06  34.703  < 2e-16 ***
    goals                8.512e-03  7.298e-04  11.664  < 2e-16 ***
    assists              4.615e-03  9.545e-04   4.836 1.36e-06 ***
    ranking             -9.805e-03  6.968e-04 -14.072  < 2e-16 ***
    position2Midfielder  7.582e-02  2.026e-02   3.742 0.000184 ***
    position2Forward     1.352e-01  2.316e-02   5.838 5.55e-09 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
  Approximate significance of smooth terms:
                                  edf Ref.df       F  p-value
   s(player_id)               344.133    646   45.02  < 2e-16 ***
   s(age,player_id)           283.490    649   48.47  < 2e-16 ***
   s(league)                    3.677      4 9429.13  < 2e-16 ***
   s(club_name)               114.517    163  103.34 6.61e-06 ***
   s(season)                   14.896     15 1747.38  < 2e-16 ***
   s(age):position2Defender     7.303      9 8824.43  < 2e-16 ***
   s(age):position2Midfielder   6.677      9 4692.20  < 2e-16 ***
   s(age):position2Forward      7.033      9 4386.07  < 2e-16 ***
   ---
   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 R-sq.(adj) =  0.883   Deviance explained = 89.6%
 fREML = -862.33  Scale est. = 0.032697  n = 6987

    "
  })
  
  output$modeling4 <- renderUI({
    withMathJax(
      br(),
      br(),
      paste0("In the 'Model Validation' section, the validation for the model is done. 
           A minimum knowledge of statistics is required in order to understand it."),
      br(),
      br(),
      paste0("In the 'Model Predictions' section, the predicted market values for the players can
           be observed. The ones from the training set and the ones from the testing set
           all toghether."),
      br(),
      br()
    )
  })
  
  
  # Validation --------------------------------------------------------------
  
  output$validation1 <- renderUI({
    withMathJax(
      paste0("In this section a brief validation of the model is done. For that, the model
             assumtions will be analyzed, and the errors from the training set and the testing
             set will be compared"),
      br(),
      br(),
      tags$b("MODEL ASSUMTIONS:"),
      br(),
      br(),
      tags$b("Normality of the residual errors:"),
      br(),
      br()
    )
  })
  
  output$val_plot1 <- renderPlot({
    g11 <- ggplot(train_df, aes(x = errors)) +
      geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue") +
      geom_density(colour="darkblue", fill="lightblue", alpha=0.2, adjust=3)
    g12 <- ggplot(train_df, aes(sample = errors) ) +
      stat_qq(color = "darkblue") +
      stat_qq_line(color = "darkblue")+
      xlab("Normal theoretical quantiles") + ylab("log10(market_value) data quantiles")
    ggarrange(g11, g12,
              ncol = 2, nrow = 1)
  })
  
  output$validation2 <- renderUI({
    withMathJax(
      br(),
      br(),
      paste0("So, residual errors seem normally distributed. It is true that they have are
             a bit right skewed, but in general we accept that they are normally distributed."),
      br(),
      br(),
      tags$b("Homoscedasticity:"),
      br(),
      br()
    )
  })
  
  output$val_plot2 <- renderPlot({
    ggplot(train_df, aes(log10(market_value), errors)) +
      geom_point()+
      geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
      geom_hline(yintercept=-0.5, linetype="dashed", color = "red")+
      geom_hline(yintercept=0, color = "grey")+
      stat_smooth()
  })
  
  output$validation3 <- renderUI({
    withMathJax(
      br(),
      br(),
      paste0("The residual errors have also more or less constant variance. It is true
             that for the small values the errors seem to devaite from 0, but, that is bound
             to the previous histogram and qq-plot."),
      br(),
      br(),
      tags$b("Normality of the random effects:"),
      br(),
      br()
    )
  })
  
  output$validation4 <- renderUI({
    withMathJax(
      br(),
      br(),
      paste0("So, the random effects are normaly distributed for all the variables. Finally,
              the mean absolute errors (MAE) for both train and test sets are presented:"),
      br(),
      br(),
      tags$b("Mean Absolute Error (MAE):"),
      br(),
      br(),
      paste0("The MAE for the training set is of 3.259.081€. The MAE in the testing set is 
             5.597.657€. So, the results from the training set are quite good, and the ones
             from the testing set are worse, but still, acceptable. Therefore, we validate
             the model."),
      br(),
      br()
    )
  })
  
  
  # Predictions -------------------------------------------------------------
  
  output$plot_prediction <- renderPlot({
    aux <- train_and_test_df[train_and_test_df$player_name == input$player_prediction,]
    ggplot(aux, aes(x = age, y = predicted_mod13)) +
      geom_line() +
      geom_ribbon(aes(ymin = predicted_mod13.l, ymax = predicted_mod13.u), alpha = 0.1, linetype = 'dashed',size=0) +
      geom_point(aes(x = age, y = market_value, color = club_name, shape = league),size = 2.5) +
      theme_bw() +
      facet_wrap(~ player_name) +
      theme(legend.position = "right") + labs(y = "market_value")
  })
  
  output$table_prediction <- DT::renderDataTable(DT::datatable({
    
    aux <- train_and_test_df[train_and_test_df$player_name == input$player_prediction,]
    aux <- aux[,c(1,3,6,7,8,64,65,66)]
    colnames(aux)[which(names(aux) == "predicted_mod13")] <- "prediction"
    colnames(aux)[which(names(aux) == "predicted_mod13.l")] <- "Lower bound prediction"
    colnames(aux)[which(names(aux) == "predicted_mod13.u")] <- "Upper bound prediction"
    aux$error <- aux$market_value - aux$prediction
    aux
    
  }))
  
  
  # Help --------------------------------------------------------------------
  
  
  
}
