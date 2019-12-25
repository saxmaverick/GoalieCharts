
GoalieCharts <- function(team,player) {

  library(plotly); library(tidyverse);
  
  setwd("~/Tableau/Goalie Charts")
  
  Sys.setenv("plotly_username"="bryan.bastin")
  Sys.setenv("plotly_api_key"="Xfix727qyFggzamt603R")
  
  pbpteam <- paste(as.character(team))
  titleplayer <- str_to_title(gsub("[.]", " ",paste(as.character(player)))) 
  raw_pbp_data <- read_csv("Goalie_PBP.csv", 
                            col_types = cols(away_on_7 = col_character(), 
                            home_on_7 = col_character()))
  processed_pbp <- raw_pbp_data %>%
    mutate(
      goalie = ifelse(
        home_team == pbpteam, 
        home_goalie, 
        away_goalie
      ),
      real_x = ifelse(
        event_zone == "Off",
        ifelse(
          coords_x < 0, 
          (coords_x*-1)-89, 
          coords_x-89
        ),
        ifelse(
          event_zone == "Def",
          abs(coords_x)+89,
          coords_x+89
        )
      ),
      real_y = ifelse(
        coords_x < 0,
        coords_y*-1,
        coords_y
      ),
      angle = ifelse(
        real_y < 0,
        90+event_angle,
        ifelse(
          real_y == 0,
          90,
          90-event_angle
        )
      ),
      group = ceiling(angle/18),
      goal  = ifelse(event_type == "GOAL",1,0),
      shot  = 1
    ) %>%
    filter(goalie == paste(as.character(player)),
           event_team != pbpteam)

  goalie_details <- processed_pbp %>%
    group_by(goalie,group) %>%
    summarize(
      goals = sum(goal),
      shots = sum(shot),
      shot_distance = mean(event_distance),
      xG    = sum(pred_goal)
    ) %>%
    mutate(
      save = ifelse(goals == 0, 1, 1-(goals/shots)),
      angle_group = (group*18)-9
    ) 
  
  goal_info <- processed_pbp %>%
    filter(goal == 1) %>%
    group_by(group) %>%
    summarize(
      goal_distance = mean(event_distance)
    )
  goalie_joined <- bind_cols(goalie_details) %>%
    left_join(goal_info,by="group") %>%
    mutate(
      goal_distance = ifelse(is.na(goal_distance),0,goal_distance)
    ) %>%
    data.frame

  p <- plot_ly(
    type = 'barpolar'
  ) %>%
    add_trace(
      r = goalie_joined$goal_distance,
      theta = goalie_joined$angle_group,
      #width = (pi/3)*goalie_joined$goals,
      text = goalie_joined$goals,
      marker = list(
        color = goalie_joined$save,
        cmin = 0.75,
        cmax = 1.00,
        cmid = 0.95,
        colorscale = "RdBu",
        reversescale = TRUE,
        showscale = FALSE
      )
    ) %>%
    add_trace(
      r = goalie_joined$shots,
      theta = goalie_joined$angle_group,
      marker = list(
        color = goalie_joined$save,
        cmin = 0.75,
        cmax = 1.00,
        cmid = 0.95,
        colorscale = "RdBu",
        reversescale = TRUE,
        showscale = TRUE,
        colorbar = list(
          x = 0.45,
          thickness = 10,
          len = .55,
          nticks = 6,
          tickformat = "%",
          title = list(
            text = "Save Percentage",
            side = "right"
          )
        )
      ),
      subplot = 'polar2'
    ) %>%
    layout(
      plot_bgcolor = "#E5ECF6", 
      paper_bgcolor = "white",
      polar = list(
        domain = list(
          x = c(0,0.4),
          y = c(0,1)
        ),
        bgcolor = "#E5ECF6",
        sector = c(0,180),
        hole = 0.12,
        angularaxis = list(
          showline = F,
          showgrid = FALSE,
          visible = T,
          ticks = "",
          gridcolor = "white",
          linecolor = "white"
        ),
        radialaxis = list(
          visible = TRUE,
          range = c(0,55),
          showline = F,
          showgrid = T,
          ticks = "",
          gridcolor = "white",
          linecolor = "white",
          title = "<br>Avg Goal Distance",
          nticks = 5,
          ticksuffix = "ft.",
          tickangle = -45,
          layer = "below traces"
        )
      ),
      polar2 = list(
        domain = list(
          x = c(0.6,1),
          y = c(0,1)
        ),
        bgcolor = "#E5ECF6",
        sector = c(0,180),
        hole = 0.12,
        angularaxis = list(
          showline = F,
          showgrid = FALSE,
          visible = T,
          ticks = "",
          gridcolor = "white",
          linecolor = "white"
        ),
        radialaxis = list(
          visible = TRUE,
          range = c(0,max(goalie_joined$shots)+20),
          showline = F,
          showgrid = T,
          ticks = "",
          gridcolor = "white",
          linecolor = "white",
          title = "<br>Number of Shots Faced",
          nticks = 5,
          tickangle = -45
        )
      ),
      showlegend = F,
      title = list(
        text = paste("<br><br><b>",as.character(titleplayer)," - ",as.character(team),"</b><br>Save Percentage by Shot Angle"),
        size = 19
      )
    ) 
  
  plotly_IMAGE(p, width = 800, height = 400, format = "png", out_file = paste("Goaltenders - ",as.character(titleplayer),".png"))
  #api_create(p, width = 800, height = 400, filename = paste("Goaltenders - ",as.character(titleplayer)))
}

