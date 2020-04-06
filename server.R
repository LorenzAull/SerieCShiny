# load libraries
library(shiny)
library(tidyverse)
library(rsconnect)
library(knitr)

# load data and source functions to make plot of basketball court
source("helpers.R")
load("matches.CGOLD2.Rda")

#rsconnect information
#rsconnect::setAccountInfo(name='removed', token='removed', secret='removed')

#Transformation of coordinates
whole_data.df$data.x3<-((((whole_data.df$data.y*0.5)-25)*1)*-1)
whole_data.df$data.y3<-(whole_data.df$data.x)
#These two work in isolation for the left court
#Now we need to transform separately for the right court
whole_data.df$data.x3 <- whole_data.df$data.x3 * ifelse(whole_data.df$data.x>50,-1,1)
whole_data.df$data.y3 <- whole_data.df$data.y3 + ifelse(whole_data.df$data.x>50,-100,0)
whole_data.df$data.y3 <- whole_data.df$data.y3 * ifelse(whole_data.df$data.x>50,-1,1)
#Coordinates transformation complete

#Definition of Success
whole_data.df$data.success2 <- ifelse(whole_data.df$data.success == 1, "made", "missed")


# define plot of court
gg_court_nba= make_court()
gg_court = make_italian_b_court()

################################################################################
# Define server logic 
shinyServer(function(input, output) {
    
    # set range of seasons based on team choice
    output$player_choice <- renderUI({
        players = whole_data.df %>% filter(data.teamName == input$team_choice) %>% 
            distinct(data.familyName) %>% pull()
        
        selectInput("player_choice", label = h3("Select player"), choices = players,
                    selected = "Trunic",
                    selectize = FALSE
        )
        
    })
    
    # 
    output$court_shots <- renderPlot({
        
        if (input$`Player comparison` == FALSE) {
        
        # subset data for NO comparison
            if (input$shots_distance == "all") {
                subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2])
                subset = subset(subset, subset$data.period %in% as.character(input$shot_quarter))}
            else {
                subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2], 
                                whole_data.df$data.shotType == input$shots_distance)
                subset = subset(subset, subset$data.period %in% as.character(input$shot_quarter))}
                            
        
        # create plot for NO comparison
        if (input$court_view=="Half Court")
        {gg_court_nba + geom_point(data = subset, alpha = 0.75, size = 2.5,
                                   aes(data.x3, data.y3 , color = data.success2 
                                   )) +
            scale_color_manual("", values = c(made = "blue", missed = "orange"))
        } else {
            gg_court + geom_point(data = subset, alpha = 0.75, size = 2.5,
                                  aes(data.x, data.y/2 , color = data.success2
                                  )) +
            scale_color_manual("", values = c(made = "blue", missed = "orange"))
        } }
        
        
        else { # subset data for YES comparison
          
            if (input$shots_distance == "all") {
                subsetteam = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2])
                subsetteam= subset(subsetteam, subsetteam$data.period %in% as.character(input$shot_quarter))
                } else {
                    subsetteam = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2], 
                                    whole_data.df$data.shotType == input$shots_distance)
                    subsetteam=subset(subsetteam, subsetteam$data.period %in% as.character(input$shot_quarter))
                    }
            
          if (input$shots_distance == "all") {
            subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2])
            subset = subset(subset, subset$data.period %in% as.character(input$shot_quarter))
            } else {
              subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2], 
                              whole_data.df$data.shotType == input$shots_distance)
              subset=subset(subset, subset$data.period %in% as.character(input$shot_quarter))
              }
                      
            
            # create plot for YES comparison
            if (input$court_view=="Half Court")
            {gg_court_nba + geom_point(data = subset, alpha = 1, size = 2.5,
                                       aes(data.x3, data.y3 , color = input$player_choice
                                       )) + geom_point(data = subsetteam, alpha = 0.15, size = 1.3,
                                                       aes(data.x3, data.y3 , color = input$team_choice
                                                       )) +
              scale_color_manual("", values = c("blue","red"))
            } else {
                gg_court + geom_point(data = subset, alpha = 1, size = 2.5,
                                      aes(data.x, data.y/2 , color = input$player_choice 
                                      )) + geom_point(data = subsetteam, alpha = 0.15, size = 1.3,
                                                       aes(data.x, data.y/2 , color = input$team_choice
                                                       )) +
                scale_color_manual("", values = c("blue","red"))
            } }
        
        
         })
      #Court Graph ends
    
    #Calculate Accuracy (should be the mean of the accuracy column?)
    output$mean_player<-renderText({
      if (input$shots_distance == "all") {
        subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2])
        subset = subset(subset, subset$data.period %in% as.character(input$shot_quarter))}
      else {
        subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2], 
                        whole_data.df$data.shotType == input$shots_distance)
        
        subset = subset(subset, subset$data.period %in% as.character(input$shot_quarter))}
      paste(" Player Accuracy:", round(((sum(subset$data.success, na.RM=TRUE)-1)/(nrow(subset)))*100,2),"%","
","Amount of selected throws:",(nrow(subset))
            )
    })
    
    output$mean_team<-renderText({
      if (input$shots_distance == "all") {
        subsetteam = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2])
        subsetteam= subset(subsetteam, subsetteam$data.period %in% as.character(input$shot_quarter))
      } else {
        subsetteam = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice, whole_data.df$date >= input$date_selection[1] & whole_data.df$date <= input$date_selection[2], 
                            whole_data.df$data.shotType == input$shots_distance)
        subsetteam=subset(subsetteam, subsetteam$data.period %in% as.character(input$shot_quarter))
      }
      paste(" Team Accuracy:",round(((sum(subsetteam$data.success, na.RM=TRUE)-1)/(nrow(subsetteam)))*100,2),"%","
","Amount of selected throws:",(nrow(subsetteam))
            )
    })
   
########## PAGE TWO ########## 
    
    ### PLAYER/TEAM COMPARISON ON PAGE 2
    output$player_choice_comparison_1 <- renderUI({
      players = whole_data.df %>% filter(data.teamName == input$team_choice_comparison_1) %>% 
        distinct(data.familyName) %>% pull()
      
      selectInput("player_choice_p1", label = h3("Select player 1"), choices = players,
                  selected = "Trunic",
                  selectize = FALSE
      )
      
    })
    
    output$player_choice_comparison_2 <- renderUI({
      players = whole_data.df %>% filter(data.teamName == input$team_choice_comparison_2) %>% 
        distinct(data.familyName) %>% pull()
      
      selectInput("player_choice_p2", label = h3("Select player 2"), choices = players,
                  selected = "Cazzaniga",
                  selectize = FALSE
      )
    })
    
    #DATA FOR THE PLAYERS
    output$player_stats_p1_accuracy<-renderText({
        subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice_p1)
      paste(" Accuracy:", round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset))
    })
    
    output$player_stats_p2_accuracy<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice_p2)
      paste(" Accuracy:", round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset))
    }) 
    
    output$player_stats_p1_2pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice_p1, whole_data.df$data.actionType == "2pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted",nrow(subset)
      )
    })
    
    output$player_stats_p2_2pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice_p2, whole_data.df$data.actionType == "2pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
            )
    }) 
  
    output$player_stats_p1_3pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice_p1, whole_data.df$data.actionType == "3pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
      )
    })
    
    output$player_stats_p2_3pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.familyName == input$player_choice_p2, whole_data.df$data.actionType == "3pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
      )
    })   
    
    #DATA FOR THE TEAMS
    output$team_stats_t1_accuracy<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice_comparison_1)
      paste(" Accuracy:", round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset))
    })
    
    output$team_stats_t2_accuracy<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice_comparison_2)
      paste(" Accuracy:", round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset))
    }) 
    
    output$team_stats_t1_2pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice_comparison_1, whole_data.df$data.actionType == "2pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
      )
    })
    
    output$team_stats_t2_2pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice_comparison_2, whole_data.df$data.actionType == "2pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
      )
    }) 
    
    output$team_stats_t1_3pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice_comparison_1, whole_data.df$data.actionType == "3pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
      )
    })
    
    output$team_stats_t2_3pt<-renderText({
      subset = filter(whole_data.df, whole_data.df$data.teamName == input$team_choice_comparison_2, whole_data.df$data.actionType == "3pt")
      paste(" Accuracy:",round(((mean(subset$data.success, na.RM=TRUE)))*100,2),"%","
","Successful:",sum(subset$data.success, na.RM=TRUE)-1,"
","Missed:",(nrow(subset)-sum(subset$data.success, na.RM=TRUE))+1,"
","Attempted:",nrow(subset)
      )
    })   
    
})