#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI
#Navbar structure for UI
navbarPage(title = "Market Value Shiny", 
           theme = shinytheme("cerulean"),
           
           
           ###### Here : insert shinydashboard dependencies ######
           header = tagList(
             useShinydashboard()
           ),
           #######################################################
           
           
           # Home --------------------------------------------------------------------
           tabPanel("Home", fluid = TRUE,
                    fluidRow(column(width = 12, align = "center", h1("Market Value Shiny"))),
                    br(),
                    fluidRow(column(width = 2), 
                             column(width = 8, p("")), 
                             column(width = 2)),
                    br(),
                    fluidRow(column(width = 2), 
                             column(width = 8, p("Developed by Julen Leo, Lore Zumeta-Olaskoaga and Dae-Jin Lee 2022")),
                             column(width = 2)),
                    fluidRow(column(width = 2),
                             column(width = 8, p("Feedback, questions you may have, bugs you may find, all are welcomed at jleo@bcamath.org or lzumeta@bcamath.org")),
                             column(width = 2)),
                    fluidRow(column(width = 2),
                             column(width = 8, a(strong("Applied Statistics group, BCAM - Basque Center for Applied Mathematics"), 
                                                 href = "http://www.bcamath.org/en/research/lines/AS",
                                                 target = "_blank")
                             ),
                             column(width = 2)),
                    fluidRow(column(width = 2),
                             column(width = 8, tags$img(src='bcam.jpg', align = "left", height='60', width='110')),
                             column(width = 2))
           ),
           
           # Data Base --------------------------------------------------------------------
           
           tabPanel("Data Base", fluid =TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        tags$b("Filters:"),
                        br(),
                        br(),
                        sliderInput(
                          inputId = "market_value_filter",
                          label = "Market value in Mill.",
                          min = 0, max = 200,
                          value = c(0,200),
                          step=1
                        ),
                        sliderTextInput(
                          inputId = "season_filter",
                          label = "Seasons",
                          choices = seasons,
                          selected = seasons[c(1,17)],
                          grid = TRUE
                        ),
                        sliderInput(
                          inputId = "minute_filter",
                          label = "Minutes played",
                          min = 0, max = 6000,
                          value = c(0,6000),
                          step=10
                        ),
                        selectizeInput(
                          inputId = "league_filter",
                          label = "League",
                          multiple = TRUE,
                          c("All", unique(as.character(players_df$league))),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "age_filter",
                          label = "Age",
                          min = 10, max = 50,
                          value = c(10,50),
                          step = 1
                        ),
                        sliderInput(
                          inputId = "height_filter",
                          label = "Height",
                          min = 1.50, max = 2.10,
                          value = c(1.5,2.1),
                          step = 0.01
                        ),
                        selectizeInput(
                          inputId = "position_filter",
                          label = "Position",
                          multiple = TRUE,
                          c("All", levels(players_df$position)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "position2_filter",
                          label = "Position2",
                          multiple = TRUE,
                          c("All", levels(players_df$position2)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "foot_filter",
                          label = "Foot",
                          multiple = TRUE,
                          c("All", levels(players_df$foot)),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "month_filter",
                          label = "Birth month",
                          min = 1, max = 12,
                          value = c(1,12),
                          step = 1
                        )
                      ),
                      mainPanel(
                        DT::dataTableOutput("table")
                      )
                    )
           ),
           
           
           # EDA ---------------------------------------------------------------------
           
           navbarMenu("Exploratory Data Analysis",
                      
                      # |- Injury data ----------------------------------------------------------
                      
                      tabPanel("Injury data", fluid = TRUE,
                               sidebarLayout(
                                 sidebarPanel(width = 3,
                                              wellPanel(
                                                id = "leftPanel1_EDA1",
                                                radioButtons("tipoButton_EDA1", "Type of selection:", choices = 
                                                               c("By team" = 'team', 
                                                                 "By players" = 'player'))
                                                
                                              ),
                                              wellPanel(
                                                id = "leftPanel2_EDA1",
                                                conditionalPanel(condition = ("input.tipoButton_EDA1 == 'team'"),
                                                                 selectInput(inputId = "team_EDA1", "Select a team:", 
                                                                             choices = teams),
                                                                 sliderInput(inputId = "seasons1_EDA1", label = "Select multiple seasons:", 
                                                                             min = min_Date,
                                                                             max = max_Date, 
                                                                             value = c(2015,2018),
                                                                             step = 1),
                                                                 sliderInput(inputId = "min_timeExp1_EDA1", label = "Set the minimum of minutes played:",
                                                                             min = 90,
                                                                             max = 3600,
                                                                             value = 90)
                                                ),
                                                conditionalPanel(condition = ("input.tipoButton_EDA1 == 'player'"),
                                                                 # selectizeInput(inputId = "player", "Select players:",  ## specify how many players at most 
                                                                 #             choices = players,
                                                                 #             multiple = TRUE),
                                                                 selectizeGroupUI(id = "filters_EDA1",
                                                                                  label = "Select players:",
                                                                                  params = list(
                                                                                    league = list(inputId = "liga_EDA1", title = "League:"),
                                                                                    model = list(inputId = "club_name_EDA1", title = "Team:"),
                                                                                    trans = list(inputId = "player_name2_EDA1", title = "Players:")
                                                                                  ),
                                                                                  btn_label = "Reset",
                                                                                  inline = TRUE),
                                                                 sliderInput(inputId = "seasons2_EDA1", label = "Select multiple seasons:", 
                                                                             min = min_Date,
                                                                             max = max_Date, 
                                                                             value = c(2015,2018),
                                                                             step = 1),
                                                                 sliderInput(inputId = "min_timeExp1_EDA1", label = "Set the minimum of minutes played:",
                                                                             min = 90,
                                                                             max = 3600,
                                                                             value = 90)
                                                ),
                                                
                                                
                                              ),
                                              actionButton('go_EDA1', 'OK', style='font-size:15px')
                                 ),
                                 mainPanel(
                                   # h1("Exploratory Analysis of Injury Data"),
                                   # plotlyOutput("gg_injuryphoto", width =  "100%"),
                                   uiOutput("knitr_EDA1"),
                                   downloadButton("download_EDA1", "Download")
                                 )
                               )),
                      
                      
                      # |- Market value ------------------------------------------
                      
                      tabPanel("Market value data", fluid = TRUE,
                               sidebarLayout(
                                 sidebarPanel(
                                   tags$b("Filters:"),
                                   br(),
                                   br(),
                                   sliderTextInput(
                                     inputId = "eda_season_filter",
                                     label = "Seasons",
                                     choices = seasons,
                                     selected = seasons[c(1,17)],
                                     grid = TRUE
                                   ),
                                   sliderInput(
                                     inputId = "eda_minute_filter",
                                     label = "Minutes played",
                                     min = 0, max = 6000,
                                     value = c(180,6000),
                                     step=10
                                   ),
                                   hr(),
                                   tags$b("Variables:"),
                                   br(),
                                   br(),
                                   selectizeInput(
                                     inputId = "variable1_selected",
                                     label = "Variable 1",
                                     choices = c("Please select the first variable...",
                                                 "season","league", "age",
                                                 "height", "position",
                                                 "position2", "foot",
                                                 "birth month"),
                                     selected = NULL,
                                     multiple = FALSE
                                   ),
                                   conditionalPanel(
                                     condition = "['season', 'age', 'height', 'birth month'].includes(input.variable1_selected)",
                                     selectizeInput(
                                       inputId = "variable2_selected",
                                       label = "Variable 2",
                                       choices = c("Please select the second (optional) variable...",
                                                   "league", "position", "position2"),
                                       selected = NULL,
                                       multiple = FALSE
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.variable1_selected == 'league'",
                                     selectizeInput(
                                       inputId = "variable2_selected_II",
                                       label = "Variable 2",
                                       choices = c("Please select the second (optional) variable...",
                                                   "position", "position2", "foot"),
                                       selected = NULL,
                                       multiple = FALSE
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.variable1_selected == 'position'",
                                     selectizeInput(
                                       inputId = "variable2_selected_III",
                                       label = "Variable 2",
                                       choices = c("Please select the second (optional) variable...",
                                                   "league", "foot"),
                                       selected = NULL,
                                       multiple = FALSE
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.variable1_selected == 'position2'",
                                     selectizeInput(
                                       inputId = "variable2_selected_IV",
                                       label = "Variable 2",
                                       choices = c("Please select the second (optional) variable...",
                                                   "league", "foot"),
                                       selected = NULL,
                                       multiple = FALSE
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.variable1_selected == 'foot'",
                                     selectizeInput(
                                       inputId = "variable2_selected_V",
                                       label = "Variable 2",
                                       choices = c("Please select the second (optional) variable...",
                                                   "league", "position", "position2"),
                                       selected = NULL,
                                       multiple = FALSE
                                     )
                                   )
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("1 Variable",
                                              titlePanel("Mean market value barplot:"),
                                              plotlyOutput('plot_mv_v1.1'),
                                              titlePanel("Log transformed market value boxplots:"),
                                              plotlyOutput('plot_mv_v1.2')
                                     ),
                                     tabPanel("2 Variables",
                                              conditionalPanel(
                                                titlePanel("Mean market value lineplots:"),
                                                condition = "['season', 'age', 'height', 'birth month'].includes(input.variable1_selected)",
                                                plotlyOutput("plot_mv_v2.1")
                                              ),
                                              conditionalPanel(
                                                titlePanel("Mean market value barplots:"),
                                                condition = "['league', 'position', 'position2', 'foot'].includes(input.variable1_selected)",
                                                plotlyOutput("plot_mv_v2.2")
                                              )
                                     )
                                   )
                                 )
                               )
                      ), 
                      
                      
                      
                      
                      # |- Market value & injury data -------------------------------------------
                      
                      
                      tabPanel("Market value & injury data", fluid = TRUE,
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   wellPanel(
                                     id = "leftPanel1_EDA3",
                                     radioButtons("tipoButton_EDA3", "Type of selection:", choices = 
                                                    c("By team" = 'team', 
                                                      "By players" = 'player'))
                                     
                                   ),
                                   wellPanel(
                                     id = "leftPanel2_EDA3",
                                     conditionalPanel(condition = ("input.tipoButton_EDA3 == 'team'"),
                                                      selectInput(inputId = "team_EDA3", "Select a team:", 
                                                                  choices = teams),
                                                      sliderInput(inputId = "seasons1_EDA3", label = "Select multiple seasons:", 
                                                                  min = min_Date,
                                                                  max = max_Date, 
                                                                  value = c(2015,2018),
                                                                  step = 1),
                                                      sliderInput(inputId = "min_timeExp1_EDA3", label = "Set the minimum of minutes played:",
                                                                  min = 90,
                                                                  max = 3600,
                                                                  value = 90)
                                     ),
                                     conditionalPanel(condition = ("input.tipoButton_EDA3 == 'player'"),
                                                      # selectizeInput(inputId = "player", "Select players:",  ## specify how many players at most 
                                                      #             choices = players,
                                                      #             multiple = TRUE),
                                                      selectizeGroupUI(id = "filters_EDA3",
                                                                       label = "Select players:",
                                                                       params = list(
                                                                         league = list(inputId = "liga_EDA3", title = "League:"),
                                                                         model = list(inputId = "club_name_EDA3", title = "Team:"),
                                                                         trans = list(inputId = "player_name2_EDA3", title = "Players:")
                                                                       ),
                                                                       btn_label = "Reset",
                                                                       inline = TRUE),
                                                      sliderInput(inputId = "seasons2_EDA3", label = "Select multiple seasons:", 
                                                                  min = min_Date,
                                                                  max = max_Date, 
                                                                  value = c(2015,2018),
                                                                  step = 1),
                                                      sliderInput(inputId = "min_timeExp2_EDA3", label = "Set the minimum of minutes played:",
                                                                  min = 90,
                                                                  max = 3600,
                                                                  value = 90)
                                     ),
                                   ),
                                   actionButton('go_EDA3', 'OK', style='font-size:15px')
                                 ),
                                 mainPanel(
                                   plotOutput("gg_marketinjury", width =  "100%")
                                 )
                               )
                      )),
           
           
           # Inference ---------------------------------------------------------------
           
           tabPanel("Statistical Inference", fluid = TRUE,
                    sidebarPanel(
                      selectInput(
                        inputId = "inference",
                        label = "Inference for:",
                        choices = c("one sample", "two samples (independent)"),
                        multiple = FALSE,
                        selected = "one sample"
                      ),
                      hr(),
                      conditionalPanel(
                        condition = "input.inference == 'one sample'",
                        sliderTextInput(
                          inputId = "inf_season_filter",
                          label = "Seasons:",
                          choices = seasons,
                          selected = seasons[c(1,17)],
                          grid = TRUE
                        ),
                        sliderInput(
                          inputId = "inf_minute_filter",
                          label = "Minutes played:",
                          min = 0, max = 6000,
                          value = c(180,6000),
                          step=10
                        ),
                        selectizeInput(
                          inputId = "inf_league_filter",
                          label = "League:",
                          multiple = TRUE,
                          c("All", unique(as.character(players_df$league))),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "inf_age_filter",
                          label = "Age:",
                          min = 10, max = 50,
                          value = c(10,50),
                          step = 1
                        ),
                        sliderInput(
                          inputId = "inf_height_filter",
                          label = "Height:",
                          min = 1.50, max = 2.10,
                          value = c(1.5,2.1),
                          step = 0.01
                        ),
                        selectizeInput(
                          inputId = "inf_position_filter",
                          label = "Position:",
                          multiple = TRUE,
                          c("All", levels(players_df$position)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "inf_position2_filter",
                          label = "Position2:",
                          multiple = TRUE,
                          c("All", levels(players_df$position2)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "inf_foot_filter",
                          label = "Foot:",
                          multiple = TRUE,
                          c("All", levels(players_df$foot)),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "inf_month_filter",
                          label = "Birth month:",
                          min = 1, max = 12,
                          value = c(1,12),
                          step = 1
                        ),
                        hr(),
                        tags$b("Hypothesis test for the market value:"),
                        numericInput(
                          inputId = "null_h0",
                          label = "\\( H_0 : \\mu = \\)",
                          value = 6000000
                        ),
                        radioButtons(
                          inputId = "alt_h1",
                          label = "\\( H_1 : \\mu \\)",
                          choices = c(
                            "\\( \\neq \\)" = "two.sided",
                            "\\( > \\)" = "greater",
                            "\\( < \\)" = "less"
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.inference == 'two samples (independent)'",
                        tags$b("Sample 1:"),
                        sliderTextInput(
                          inputId = "inf_season_filter_ind1",
                          label = "Seasons (Sample 1):",
                          choices = seasons,
                          selected = seasons[c(1,17)],
                          grid = TRUE
                        ),
                        sliderInput(
                          inputId = "inf_minute_filter_ind1",
                          label = "Minutes played (Sample 1):",
                          min = 0, max = 6000,
                          value = c(180,6000),
                          step=10
                        ),
                        selectizeInput(
                          inputId = "inf_league_filter_ind1",
                          label = "League (Sample 1):",
                          multiple = TRUE,
                          c("All", unique(as.character(players_df$league))),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "inf_age_filter_ind1",
                          label = "Age (Sample 1):",
                          min = 10, max = 50,
                          value = c(10,50),
                          step = 1
                        ),
                        sliderInput(
                          inputId = "inf_height_filter_ind1",
                          label = "Height (Sample 1):",
                          min = 1.50, max = 2.10,
                          value = c(1.5,2.1),
                          step = 0.01
                        ),
                        selectizeInput(
                          inputId = "inf_position_filter_ind1",
                          label = "Position (Sample 1):",
                          multiple = TRUE,
                          c("All", levels(players_df$position)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "inf_position2_filter_ind1",
                          label = "Position2 (Sample 1):",
                          multiple = TRUE,
                          c("All", levels(players_df$position2)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "inf_foot_filter_ind1",
                          label = "Foot (Sample 1):",
                          multiple = TRUE,
                          c("All", levels(players_df$foot)),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "inf_month_filter_ind1",
                          label = "Birth month (Sample 1):",
                          min = 1, max = 12,
                          value = c(1,12),
                          step = 1
                        ),
                        hr(),
                        tags$b("Sample 2:"),
                        sliderTextInput(
                          inputId = "inf_season_filter_ind2",
                          label = "Seasons (Sample 2):",
                          choices = seasons,
                          selected = seasons[c(1,17)],
                          grid = TRUE
                        ),
                        sliderInput(
                          inputId = "inf_minute_filter_ind2",
                          label = "Minutes played (Sample 2):",
                          min = 0, max = 6000,
                          value = c(180,6000),
                          step=10
                        ),
                        selectizeInput(
                          inputId = "inf_league_filter_ind2",
                          label = "League (Sample 2):",
                          multiple = TRUE,
                          c("All", unique(as.character(players_df$league))),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "inf_age_filter_ind2",
                          label = "Age (Sample 2):",
                          min = 10, max = 50,
                          value = c(10,50),
                          step = 1
                        ),
                        sliderInput(
                          inputId = "inf_height_filter_ind2",
                          label = "Height (Sample 2):",
                          min = 1.50, max = 2.10,
                          value = c(1.5,2.1),
                          step = 0.01
                        ),
                        selectizeInput(
                          inputId = "inf_position_filter_ind2",
                          label = "Position (Sample 2):",
                          multiple = TRUE,
                          c("All", levels(players_df$position)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "inf_position2_filter_ind2",
                          label = "Position2 (Sample 2):",
                          multiple = TRUE,
                          c("All", levels(players_df$position2)),
                          selected = c("All")
                        ),
                        selectizeInput(
                          inputId = "inf_foot_filter_ind2",
                          label = "Foot (Sample 2):",
                          multiple = TRUE,
                          c("All", levels(players_df$foot)),
                          selected = c("All")
                        ),
                        sliderInput(
                          inputId = "inf_month_filter_ind2",
                          label = "Birth month (Sample 2):",
                          min = 1, max = 12,
                          value = c(1,12),
                          step = 1
                        ),
                        hr(),
                        tags$b("Hypothesis test for the market value:"),
                        numericInput(
                          inputId = "null_h0_ind",
                          label = "\\( H_0 : \\mu_1 - \\mu_2 = \\)",
                          value = 0
                        ),
                        radioButtons(
                          inputId = "alt_h1_ind",
                          label = "\\( H_1 : \\mu_1 - \\mu_2 \\)",
                          choices = c(
                            "\\( \\neq \\)" = "two.sided",
                            "\\( > \\)" = "greater",
                            "\\( < \\)" = "less"
                          )
                        )
                      ),
                      hr(),
                      sliderInput("alpha",
                                  "Significance level \\(\\alpha = \\)",
                                  min = 0.01,
                                  max = 0.20,
                                  value = 0.05
                      )
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Parametric inference",
                                 conditionalPanel(
                                   condition = "input.inference == 'one sample'",
                                   uiOutput("results_onemean1")
                                 ),
                                 conditionalPanel(
                                   condition = "input.inference == 'two samples (independent)'",
                                   uiOutput("results_twomeans_ind1")
                                 ),
                                 br(),
                                 conditionalPanel(
                                   condition = "input.inference == 'one sample'",
                                   plotOutput("plot_market_value_distr_onemean")
                                 ),
                                 conditionalPanel(
                                   condition = "input.inference == 'two samples (independent)'",
                                   plotOutput("plot_market_value_distr_ind")
                                 ),
                                 br(),
                                 conditionalPanel(
                                   condition = "input.inference == 'one sample'",
                                   uiOutput("results_onemean2")
                                 ),
                                 conditionalPanel(
                                   condition = "input.inference == 'two samples (independent)'",
                                   uiOutput("results_twomeans_ind2")
                                 )
                        ),
                        tabPanel("Nonparametric inference",
                                 conditionalPanel(
                                   condition = "input.inference == 'one sample'",
                                   uiOutput("results_onesample1")
                                 ),
                                 conditionalPanel(
                                   condition = "input.inference == 'two samples (independent)'",
                                   uiOutput("results_twosamples_ind1")
                                 ),
                                 br(),
                                 conditionalPanel(
                                   condition = "input.inference == 'one sample'",
                                   plotOutput("plot_market_value_distr_onesample")
                                 ),
                                 conditionalPanel(
                                   condition = "input.inference == 'two samples (independent)'",
                                   plotOutput("plot_market_value_distr_twosamples")
                                 ),
                                 br(),
                                 conditionalPanel(
                                   condition = "input.inference == 'one sample'",
                                   uiOutput("results_onesample2")
                                 ),
                                 conditionalPanel(
                                   condition = "input.inference == 'two samples (independent)'",
                                   uiOutput("results_twosamples_ind2")
                                 )
                        )
                      )
                    )
           ),
           
           
           
           # Statistical Modelling ---------------------------------------------------
           tabPanel("Statistical Modeling", fluid = TRUE,
                    uiOutput("modeling1"),
                    plotOutput("plot1"),
                    uiOutput("modeling2"),
                    tabsetPanel(
                      tabPanel("Age",
                               plotlyOutput("plot_age")
                      ),
                      tabPanel("Goals",
                               plotlyOutput("plot_goals")
                      ),
                      tabPanel("Assists",
                               plotlyOutput("plot_assists")
                      ),
                      tabPanel("Minutes Played",
                               plotlyOutput("plot_minutes_played")
                      ),
                      tabPanel("Ranking",
                               plotlyOutput("plot_ranking")
                      ),
                      tabPanel("Position",
                               plotlyOutput("plot_position2")
                      ),
                      tabPanel("League",
                               plotlyOutput("plot_league")
                      ),
                      tabPanel("Season",
                               plotlyOutput("plot_season")
                      )
                    ),
                    uiOutput("modeling3"),
                    verbatimTextOutput("model"),
                    uiOutput("modeling4"),
                    
                    
           ),
           
           
           # Validation --------------------------------------------------------------
           tabPanel("Model Validation", fluid = TRUE,
                    uiOutput("validation1"),
                    plotOutput("val_plot1"),
                    uiOutput("validation2"),
                    plotOutput("val_plot2"),
                    uiOutput("validation3"),
                    tabsetPanel(
                      tabPanel("Players",
                               tags$img(src='players.png', width="100%")
                      ),
                      tabPanel("Clubs",
                               tags$img(src='clubs.png', width="100%")
                      ),
                      tabPanel("Leagues",
                               tags$img(src='leagues.png', width="100%")
                      ),
                      tabPanel("Seasons",
                               tags$img(src='seasons.png', width="100%")
                      )
                    ),
                    uiOutput("validation4")
           ),
           
           
           # Predictions -------------------------------------------------------------
           tabPanel("Model Predictions", fluid = TRUE,
                    sidebarPanel(
                      selectInput(
                        inputId = "player_prediction",
                        label = "Player:",
                        choices = unique(as.character(train_df$player_name)),
                        multiple = FALSE,
                        selected = "lionel-messi"
                      )),
                    mainPanel(
                      plotOutput("plot_prediction"),
                      DT::dataTableOutput("table_prediction")
                    )
           )
)

