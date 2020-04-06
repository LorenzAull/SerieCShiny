# load libraries and data
library(shiny)
library(tidyverse)
library(knitr)
load("matches.CGOLD2.Rda")

# data management
made = whole_data.df %>% distinct(data.success) %>% pull()
teams = whole_data.df %>% distinct(data.teamName) %>% pull()
area = whole_data.df %>% distinct(data.area) %>% pull()
whole_data.df$data.success <- ifelse(whole_data.df$data.success == 1, "made", "missed")

# Define UI for application
shinyUI(fluidPage(
   
  navbarPage("Navigation",
             tabPanel("Shots",
   
    # Application title
    titlePanel("Serie C Gold Shot Attempts"),
    
    
    br(),
    
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
  
            
            # # drop down menu for player
            selectInput("team_choice", label = h2("Select team"),
                        choices = teams, selected = "SANSEBASKET", selectize = FALSE) , # uncomment comma to add another widget
            # 
            # # drop down menu for season based on a certain player
            uiOutput("player_choice") ,
            
            checkboxInput("Player comparison", label = ("Player/Team Comparison On/Off")),
          
            radioButtons("shots_distance", label = h3("Choose distance:"), choices = list("all", "2pt short range", "2pt mid range", "3pt"),
                         selected = "all",inline = TRUE),
            
            checkboxGroupInput("shot_quarter", label = h3("Quarter"), choices = list(1,2,3,4), selected = c(1,2,3,4), inline = TRUE),
            
            dateRangeInput("date_selection", label = h3("Date range:"), start = min(whole_data.df$date),
                       end = max(whole_data.df$date), min = min(whole_data.df$date), max = max(whole_data.df$date),
                       format = "yyyy-mm-dd", separator = "-") ,

            #Select Court View
            radioButtons("court_view", label = h3("Select Court View"), choices = list("Half Court", "Full Court"))           
             ),

    
        # Show output based on user selections
        mainPanel(
            
            # spatial plot of shots made
            plotOutput("court_shots"),
            #
            verbatimTextOutput("mean_player"),
            verbatimTextOutput("mean_team")
            # box plot of shot distances
            #  plotlyOutput("shot_distances")
            
        )
     )
    ),
    ########## PAGE TWO ########## 
    tabPanel("Player Comparison",
             fluidRow(
               #PLAYER 1
               column(3,
                      selectInput("team_choice_comparison_1", label = h3("Select team 1"),
                                  choices = teams, selected = "SANSEBASKET", selectize = FALSE),
                      h5("Overall"),
                      verbatimTextOutput("team_stats_t1_accuracy"),
                      h5("2pts"),
                      verbatimTextOutput("team_stats_t1_2pt"),
                      
                      h5("3pts"),
                      verbatimTextOutput("team_stats_t1_3pt")
                      ),
               column(3,
                      uiOutput("player_choice_comparison_1"), 
                      h5("Overall"),
                      verbatimTextOutput("player_stats_p1_accuracy"),
                      h5("2pts"),
                      verbatimTextOutput("player_stats_p1_2pt"),
                      
                      h5("3pts"),
                      verbatimTextOutput("player_stats_p1_3pt")
                      ),
               #PLAYER 2
               column(3,
                      selectInput("team_choice_comparison_2", label = h3("Select team 2"),
                                  choices = teams, selected = "SANSEBASKET", selectize = FALSE),
                      h5("Overall"),
                      verbatimTextOutput("team_stats_t2_accuracy"),
                      h5("2pts"),
                      verbatimTextOutput("team_stats_t2_2pt"),
                      
                      h5("3pts"),
                      verbatimTextOutput("team_stats_t2_3pt")
               ),
               column(3,
                      uiOutput("player_choice_comparison_2"),
                      h5("Overall"),
                      verbatimTextOutput("player_stats_p2_accuracy"),
                      h5("2pts"),
                      verbatimTextOutput("player_stats_p2_2pt"),
                      
                      h5("3pts"),
                      verbatimTextOutput("player_stats_p2_3pt")
                      )
             )

    ),
    ########## PAGE THREE ########## 
    tabPanel("Documentation",
             
             column(width = 8, offset = 1,
             includeMarkdown("documentation.Rmd")
             )
    ) 
           
  )
))

