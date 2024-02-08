if (!require("png")) install.packages("png")
library(png)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)
if (!require("webshot")) install.packages("webshot")
library(webshot)
if (!require("StatsBombR")) install.packages("StatsBombR")
library(StatsBombR)
if (!require("officer")) install.packages("officer")
library(officer)

library(ggplot2)
library(ggstar)

StatsBombData #Les données de la coupe du monde 2022 au Qatar

Info_Matches <- (Matches %>% dplyr::select(match_id, match_date, home_team.home_team_name, away_team.away_team_name))

Events <- StatsBombData %>%
  dplyr::left_join(Info_Matches, by = 'match_id')

EventsTeam <- Events %>%
  dplyr::filter(away_team.away_team_name == 'Argentina' | home_team.home_team_name == 'Argentina') 

CornerPlusUn <- EventsTeam %>%
  dplyr::filter(pass.type.name == 'Corner') %>%
  dplyr::filter(pass.outcome.name != 'Out' | is.na(pass.outcome.name)) %>%
  dplyr::select(match_id, index) %>%
  dplyr::mutate(index_plus_un = index + 1) %>%
  dplyr::left_join(TeamShiny, by  = c('match_id', 'index_plus_un' = 'index')) %>%
  dplyr::mutate(next_possession_team.name = possession_team.name) %>%
  dplyr::mutate(next_team.name = team.name) %>%
  dplyr::select(match_id, index, index_plus_un, next_possession_team.name, next_team.name)


TeamCorner <- EventsTeam %>%
  dplyr::filter(pass.type.name == 'Corner') %>%
  dplyr::filter(pass.outcome.name != 'Out' | is.na(pass.outcome.name)) %>%
  dplyr::left_join(CornerPlusUn, by = c('match_id', 'index')) %>%
  dplyr::mutate(FirstBall = ifelse(team.name == next_team.name, 'Win', 'Lost'))

TeamDefendingCorner <- TeamCorner %>%
  dplyr::filter(team.name != 'Argentina')

DefLeftGoal <- TeamDefendingCorner %>%
  dplyr::filter(pass.goal_assist == TRUE) %>%
  dplyr::filter(location.y == 0.1)

DefLeftShot <- TeamDefendingCorner %>%
  dplyr::filter(pass.shot_assist == TRUE) %>%
  dplyr::filter(location.y == 0.1)

DefLeftOther <- TeamDefendingCorner %>%
  dplyr::filter(is.na(pass.shot_assist) & is.na(pass.goal_assist)) %>%
  dplyr::filter(location.y == 0.1)



CornerMap <- function(Goal, Shot, Other, Which){
  
  ggplot() +
    annotate("rect",xmin = 119.9, xmax = 120, ymin =36, ymax = 44, fill = NA, colour = "#666666", size = 1) +
    annotate("rect",xmin = 71, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "#CCCCCC", size = 1) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "#CCCCCC", size = 1) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "#CCCCCC", size = 1) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "#CCCCCC", size = 1) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "#CCCCCC", size = 1) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "#CCCCCC", size = 1) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "#CCCCCC", size =1) +
    #annotate("segment", x = 120, xend = 120, y = 36, yend = 44, colour = "#CCCCCC", size = 1)+
    #annotate("segment", x = 70, xend = 70, y = -0.5, yend = 80.5, colour = "#CCCCCC", size = 1) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "#CCCCCC", size = 1) +
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "#CCCCCC", size = 1) +
    theme(rect = element_blank(),
          line = element_blank()) +
    # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "#CCCCCC", size = 1.05) +
    # add centre spot
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 1,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="#CCCCCC") +
    
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 1,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="#CCCCCC") +
    
    geom_point(data = Goal, aes(x = pass.end_location.x, y = pass.end_location.y, fill = FirstBall, color = FirstBall),
               size = 2, alpha = 1, shape = 22, stroke = 1) + #3
    
    geom_point(data = Goal, aes(x = pass.end_location.x, y = pass.end_location.y, color = FirstBall),
               size = 3.5, alpha = 1, shape = 22, stroke = 1) +
    
    geom_point(data = Shot, aes(x = pass.end_location.x, y = pass.end_location.y, color = FirstBall),
               size = 1.5, alpha = 1, shape = 22, stroke = 2) +
    
    geom_point(data = Other, aes(x = pass.end_location.x, y = pass.end_location.y, fill = FirstBall, color = FirstBall),
               size = 3.5, alpha = 1, shape = 21, stroke = 0) +
    
    #  geom_point(data = CGNoTouch, aes(x = pass.end_location.x, y = end_location.y),
    #             size = 3.5, alpha = 1, color = 'LightGrey', shape = 23) +
    
    
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0),
          plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0),
          axis.text.y=element_blank(),
          legend.position = "none",
          legend.title=element_text(size=22,family="Source Sans Pro"),
          legend.text=element_text(size=20,family="Source Sans Pro"),
          legend.margin = margin(c(20, 10, -85, 50)),
          legend.key.size = unit(1.5, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans Pro", colour = "black", hjust = 0),
          legend.direction = "horizontal",
          axis.ticks=element_blank(),
          aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "white"),
          strip.text.x = element_text(size=13,family="Source Sans Pro")) +
    labs(title = 'Argentina',
         subtitle = paste0('Corner ', Which, ' Toulouse \nLigue 1, 2022/2023')) +
    #annotate(geom = 'text', x = 125, y = 5, label = 'Attacking Corner') + #4
    scale_color_manual(values = c('Lost' = '#3371ac', 'Win' = '#dc2228')) +
    scale_fill_manual(values = c('Lost' = '#3371ac', 'Win' = '#dc2228')) +
    scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") + #6
    guides(fill = guide_colourbar(title.position = "top"),
           shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
    coord_flip(xlim = c(70, 121)) 
  
}

CornerMap(DefLeftGoal, DefLeftShot, DefLeftOther, 'Against')



CornerMap <- function(Goal, Shot, Other, Which){
  
  plot_ly() %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      y = ~Goal$pass.end_location.x,
      x = ~Goal$pass.end_location.y,
      color = ~Goal$FirstBall,
      colors = c("#dc2228", "#3371ac"),
      marker = list(size = 10, symbol = 'circle'),
      legendgroup = "Goal"
    ) %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      y = ~Shot$pass.end_location.x,
      x = ~Shot$pass.end_location.y,
      color = ~Shot$FirstBall,
      colors = c("#dc2228", "#3371ac"),
      marker = list(size = 8, symbol = 'circle-open', line = list(color = ~Shot$FirstBall,
                                                                  colors = c("#dc2228", "#3371ac"), width = 3)),
      legendgroup = "Shot"
    ) %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      y = ~Other$pass.end_location.x,
      x = ~Other$pass.end_location.y,
      color = ~Other$FirstBall,
      colors = c("#dc2228", "#3371ac"),
      marker = list(size = 10, symbol = 'square'),
      legendgroup = "Other"
    ) %>%
    layout(
      xaxis = list(range = c(0, 90), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F),
      yaxis = list(range = c(59, 129), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F)
      , plot_bgcolor = '#d3ffda'
    ) %>%
    #add_trace(
    #  type = "scatter",
    #  mode = "none",
    #  fill = "toself",
    #  fillcolor = "#d3ffda",
    #  y = c(70, 70, 120, 120, 70),
    #  x = c(0, 80, 80, 0, 0),
    #  showlegend = FALSE
    #) %>%
    plotly::add_trace(y = c(70, 70, 120, 120, 70),
                      x = c(0, 80, 80, 0, 0),
                      mode = "lines",
                      line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE
    ) %>%
    plotly::add_trace(y = c(121, 121), x = c(36, 44), mode = 'lines', line = list(color = 'black', width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(120, 121), x = c(44, 44), mode = 'lines', line = list(color = 'black', width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(120, 121), x = c(36, 36), mode = 'lines', line = list(color = 'black', width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(102, 120), x = c(62, 62), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(102, 120), x = c(18, 18), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(102, 102), x = c(18, 62), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(114, 120), x = c(50, 50), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(114, 120), x = c(30, 30), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(114, 114), x = c(30, 50), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(108), x = c(40), mode = "markers", marker = list(size = 5, color = "black"), type = 'scatter', showlegend = FALSE) %>%
    hide_legend() %>%
    add_paths(y=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), 
              x=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), color = I('black'), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(x = .5, y = 126, mode = 'text', type = 'scatter', text = 'player', textposition = 'middle right', textfont = list(color = 'red', size = 22), showlegend = FALSE) %>%
    plotly::add_trace(x = .5, y = 123, mode = 'text', type = 'scatter', text = paste0('Team', ', ', 'CountryName'), textposition = 'middle right', textfont = list(size = 15), showlegend = FALSE) %>%
    plotly::add_trace(x = .5, y = 121, mode = 'text', type = 'scatter', text = paste0('CompetitionName', ' ', 'SeasonName'), textposition = 'middle right', textfont = list(size = 15), showlegend = FALSE) %>%
    plotly::add_trace(x = 80, y = 126, mode = 'text', type = 'scatter', text = paste0('Exp. goals: ', 'xg_goal', ' (', 'goal_test', ' goals /', 'shot_test', ' shots)')
                      , textposition = 'middle left' , textfont = list(size = 20), showlegend = FALSE)
}

CornerMap(DefLeftGoal, DefLeftShot, DefLeftOther, 'Against')





#################TIRS###############


#ShotTeam <- EventsTeam %>%
#  dplyr::filter(type.name == 'Shot') %>%
#  dplyr::filter(play_pattern.name == 'From Corner' | play_pattern.name == 'From Free Kick') %>%
#  dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
#  dplyr::select(play_pattern.name, player.name, team.name, possession_team.name, minute, shot.statsbomb_xg,
#                shot.body_part.name, shot.outcome.name, match_id, possession, location.x, location.y, pass.end_location.x, 
#                pass.end_location.y, timestamp)

ShotTeam <- EventsTeam %>%
  dplyr::filter(team.name == 'Argentina') %>%
  dplyr::left_join(IndexMatches, by = 'match_id') %>%
  dplyr::filter(type.name == 'Shot') %>%
  dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
  dplyr::filter(location.x >= 90) %>%
  dplyr::mutate(technique = '') %>%
  dplyr::mutate(formes = '') %>%
  dplyr::mutate(color = ifelse(shot.outcome.name == 'Goal', '#109618', '#dc3912')) %>%
  dplyr::mutate(formes = ifelse(shot.body_part.name == 'Head', 'circle', 'hexagon'))

ShotTeam <- ShotTeam %>%
  dplyr::filter(index_match < Round)

for(i in seq(from = 1, to = length(ShotTeam$id))){
  if
  (ShotTeam$shot.type.name[i] == 'Free Kick'){
    ShotTeam$technique[i] = 'Free Kick'
    ShotTeam$formes[i] = 'square'
  } else if (ShotTeam$shot.body_part.name[i] == 'Head'){
    ShotTeam$technique[i] = 'Head'
    ShotTeam$formes[i] = 'circle'
  } else {
    ShotTeam$technique[i] = 'Foot/Other'
    ShotTeam$formes[i] = 'hexagon'
  }
}

InfosCorner <- Events %>%
  dplyr::filter(type.name == 'Pass') %>%
  dplyr::filter(pass.type.name == 'Corner' | pass.type.name == 'Free Kick') %>%
  dplyr::mutate(time_corner = timestamp,
                location_corner = location) %>%
  dplyr::mutate(corner = ifelse(pass.type.name == 'Corner', 
                                ifelse(location_corner == 'c(120, 80)', 'Right', 'Left'), 'Free Kick')) %>%
  dplyr::select(match_id, possession, corner, time_corner)

InfosMatchTeam <- Matches %>%
  dplyr::select(match_id, match_date, home_team.home_team_name, away_team.away_team_name)

ShotTeamCIFK <- ShotTeam %>%
  dplyr::left_join(InfosMatchTeam) %>%
  dplyr::mutate(team_vs.name = ifelse(possession_team.name == home_team.home_team_name, 
                                      away_team.away_team_name, home_team.home_team_name)) %>%
  dplyr::left_join(InfosCorner, by = c('match_id', 'possession')) %>%
  separate(timestamp, into = c('heur', 'min', 'sec'), sep = ':') %>%
  separate(time_corner, into = c('h', 'm', 's'), sep = ':') %>%
  dplyr::mutate(time_diff = (as.numeric(min) * 60 + as.numeric(sec)) - (as.numeric(m) * 60 + as.numeric(s))) %>%
  dplyr::filter(corner == 'Right' & time_diff < 20 |corner == 'Left' & time_diff < 20 |corner == 'Free Kick' & time_diff < 15 )

ShotTeamCIFK <- ShotTeamCIFK %>%
  dplyr::filter(possession_team.name != 'Argentina')




ShotMap <- function(Data, Which, Where, From){
  shotmapxgcolors <- c("#192780","#2a5d9f","#40a7d0","#87cdcf","#e7f8e6","#f4ef95","#fde960",
                       "#f5b94d","#ed8a37","#d54f1b","#bf0000","#7f0000")
  
  DBase <- Data %>%
    dplyr::filter(play_pattern.name == From) 
  #%>%
  #  dplyr::filter(corner == Where)
  
  Goal <- DBase %>% dplyr::filter(shot.outcome.name == 'Blocked')
  Blocked <- DBase %>% dplyr::filter(shot.outcome.name == 'Blocked')
  OffTarget <- DBase %>% dplyr::filter(shot.outcome.name == 'Off T')
  Saved <- DBase %>% dplyr::filter(shot.outcome.name == 'Saved')
  
  xG <- DBase %>% dplyr::select(shot.statsbomb_xg)
  ExpectedGoal <- round(sum(xG), digits = 2)
  
  ShotNb <- count(Data)
  GoalSelect <- DBase %>% dplyr::filter(shot.outcome.name == 'Goal')
  GoalNb <- count(GoalSelect)
  
  
  ggplot() +
    annotate("rect",xmin = 119.9, xmax = 120, ymin =36, ymax = 44, fill = NA, colour = "#666666", size = 1) +
    annotate("rect",xmin = 71, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "#666666", size = 1) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "#666666", size = 1) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "#666666", size = 1) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "#666666", size = 1) +
    #annotate("segment", x = 120, xend = 120, y = 36, yend = 44, colour = "#CCCCCC", size = 1)+
    #annotate("segment", x = 70, xend = 70, y = -0.5, yend = 80.5, colour = "#666666", size = 1)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "#666666", size = 1)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "#666666", size = 1)+
    theme(rect = element_blank(),
          line = element_blank()) +
    # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "#666666", size = 1.05) +
    # add centre spot
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 1,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="#666666") +
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 1,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="#666666") +
    
    geom_star(data = Goal, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, starshape = shot.body_part.name),
              size = 6, alpha = 0.9, color = 'black') + #3
    
    geom_star(data = Goal, aes(x = location.x, y= location.y, starshape = shot.body_part.name),
              size = 8, alpha = 0.9, color = 'black') +
    
    geom_star(data = Blocked, aes(x = location.x, y = location.y, starshape = shot.body_part.name),
              size = 6, alpha = 0.8, starstroke = 0.5, color = '#aaaaa2', fill ='LightGrey') +
    
    geom_star(data = OffTarget, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, starshape = shot.body_part.name),
              size = 6, alpha = 0.8, color = '#aaaaaa', starstroke = 0.5) +
    
    geom_star(data = Saved, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, starshape = shot.body_part.name),
              size = 6, alpha = 0.8, color = 'Black', starstroke = 1) +
    
    
    
    
    geom_star(aes(x = 65, y = 2), starshape = 13,
              size = 6, color = 'black') + #3
    
    geom_text(aes(x = 65, y = 7), label = 'free kick') +
    
    geom_star(aes(x = 60, y = 2), starshape = 11,
              size = 6, color = 'black') +
    
    geom_text(aes(x = 60, y = 11.5), label = 'assisted with a throuball') +
    
    
    geom_star(aes(x = 55, y = 2), starshape = 28,
              size = 6, color = 'black') +
    
    geom_text(aes(x = 55, y = 13), label = 'following a successful dribble') +
    
    
    geom_star(aes(x = 65, y = 25), starshape = 15,
              size = 6, color = 'black') +
    
    geom_text(aes(x = 65, y = 29), label = 'header') +
    
    
    geom_star(aes(x = 60, y = 25), starshape = 6,
              size = 6, color = 'black') +
    
    geom_text(aes(x = 60, y = 30), label = 'foot/other') +
    
    
    
    
    geom_star(aes(x = 65, y = 46), starshape = 6,
              size = 6, color = 'Black', starstroke = 1) +
    
    geom_star(aes(x = 65, y = 46), starshape = 6,
              size = 6, color = 'black') + #3
    
    geom_text(aes(x = 62, y = 46), label = 'goal') +
    
    
    geom_star(aes(x = 65, y = 54), starshape = 6,
              size = 6, color = 'black') +
    
    geom_text(aes(x = 62, y = 54), label = 'saved') +
    
    
    geom_star(aes(x = 65, y = 62), starshape = 6,
              size = 6, starstroke = 0.5, color = '#aaaaa2', fill ='LightGrey') +
    
    geom_text(aes(x = 62, y = 62), label = 'off target') +
    
    
    geom_star(aes(x = 65, y = 70), starshape = 6,
              size = 6, color = '#aaaaaa', starstroke = 0.5) +
    
    geom_text(aes(x = 62, y = 70), label = 'blocked') +
    
    geom_text(aes(x = 58, y = 47), label = 'low xG') +
    geom_star(aes(x = 55, y = 46), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#192780', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 48), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#2a5d9f', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 50), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#40a7d0', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 52), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#87cdcf', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 54), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#e7f8e6', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 56), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#f4ef95', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 58), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#fde960', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 60), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#f5b94d', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 62), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#ed8a37', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 64), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#d54f1b', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 66), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#bf0000', starstroke = 0.5) +
    geom_star(aes(x = 55, y = 68), starshape = 6,
              size = 6, color = '#aaaaaa', fill = '#7f0000', starstroke = 0.5) +
    geom_text(aes(x = 58, y = 67), label = 'high xG') +
    
    
    #geom_path(size = 10, lineend = 'square') +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0),
          plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0),
          axis.text.y=element_blank(),
          legend.position = "none",
          legend.title=element_text(size=22,family="Source Sans Pro"),
          legend.text=element_text(size=20,family="Source Sans Pro"),
          legend.margin = margin(c(20, 10, -85, 50)),
          legend.key.size = unit(1.5, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans Pro", colour = "black", hjust = 0),
          legend.direction = "horizontal",
          axis.ticks=element_blank(),
          aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "white"),
          strip.text.x = element_text(size=13,family="Source Sans Pro")) +
    labs(title = 'Argentina',
         subtitle = paste0('Shot ', Which, 'Argentina', ' \nLigue 1, 2022/2023'),
         caption = paste0('Exp. Goals: ', round(sum(ExpectedGoal), digits = 2),' (', count(Goal), ' goals/ ', count(DBase), ' shots)')) + #4
    scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,0.8), oob=scales::squish, name = "Expected Goals
Value") + #5
    scale_starshape_manual(values = c("Head" = 15, "Right Foot" = 6, "Left Foot" = 6, "Other" = 6), name ="") + #6
    guides(fill = guide_colourbar(title.position = "top"),
           shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
    coord_flip(xlim = c(50, 121))
  
  
  
  
  
  
}



ShotMap(ShotTeam, 'against', 'Left', 'From Corner')





ShotMap <- function(Data, Which, Where, From){
  
  shotmapxgcolors <- c("#192780","#2a5d9f","#40a7d0","#87cdcf","#e7f8e6","#f4ef95","#fde960",
                       "#f5b94d","#ed8a37","#d54f1b","#bf0000","#7f0000")
  
  DBase <- Data %>%
    dplyr::filter(play_pattern.name == From) 
  #%>%
  #  dplyr::filter(corner == Where)
  
  Goal <- DBase %>% dplyr::filter(shot.outcome.name == 'Blocked')
  Blocked <- DBase %>% dplyr::filter(shot.outcome.name == 'Blocked')
  OffTarget <- DBase %>% dplyr::filter(shot.outcome.name == 'Off T')
  Saved <- DBase %>% dplyr::filter(shot.outcome.name == 'Saved')
  
  xG <- DBase %>% dplyr::select(shot.statsbomb_xg)
  ExpectedGoal <- round(sum(xG), digits = 2)
  
  ShotNb <- count(Data)
  GoalSelect <- DBase %>% dplyr::filter(shot.outcome.name == 'Goal')
  GoalNb <- count(GoalSelect)
  
  
  
  plot_ly() %>%
    layout(
      xaxis = list(range = c(0, 90), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F),
      yaxis = list(range = c(59, 129), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F)
      , plot_bgcolor = '#d3ffda'
    ) %>%
    #add_trace(
    #  type = "scatter",
    #  mode = "none",
    #  fill = "toself",
    #  fillcolor = "#d3ffda",
    #  y = c(70, 70, 120, 120, 70),
    #  x = c(0, 80, 80, 0, 0),
    #  showlegend = FALSE
    #) %>%
    plotly::add_trace(y = c(70, 70, 120, 120, 70),
                      x = c(0, 80, 80, 0, 0),
                      mode = "lines",
                      line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE
    ) %>%
    plotly::add_trace(y = c(121, 121), x = c(36, 44), mode = 'lines', line = list(color = 'black', width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(120, 121), x = c(44, 44), mode = 'lines', line = list(color = 'black', width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(120, 121), x = c(36, 36), mode = 'lines', line = list(color = 'black', width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(102, 120), x = c(62, 62), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(102, 120), x = c(18, 18), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(102, 102), x = c(18, 62), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(114, 120), x = c(50, 50), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(114, 120), x = c(30, 30), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(114, 114), x = c(30, 50), mode = "lines", line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(y = c(108), x = c(40), mode = "markers", marker = list(size = 5, color = "black"), type = 'scatter', showlegend = FALSE) %>%
    hide_legend() %>%
    add_paths(y=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), 
              x=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), color = I('black'), type = 'scatter', showlegend = FALSE) %>%
    plotly::add_trace(x = .5, y = 126, mode = 'text', type = 'scatter', text = 'player', textposition = 'middle right', textfont = list(color = 'red', size = 22), showlegend = FALSE) %>%
    plotly::add_trace(x = .5, y = 123, mode = 'text', type = 'scatter', text = paste0('Team', ', ', 'CountryName'), textposition = 'middle right', textfont = list(size = 15), showlegend = FALSE) %>%
    plotly::add_trace(x = .5, y = 121, mode = 'text', type = 'scatter', text = paste0('CompetitionName', ' ', 'SeasonName'), textposition = 'middle right', textfont = list(size = 15), showlegend = FALSE) %>%
    plotly::add_trace(x = 80, y = 126, mode = 'text', type = 'scatter', text = paste0('Exp. goals: ', 'xg_goal', ' (', 'goal_test', ' goals /', 'shot_test', ' shots)')
                      , textposition = 'middle left' , textfont = list(size = 20), showlegend = FALSE
                      ) %>%
    plotly::add_trace(data = Blocked, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers',
                      showlegend = FALSE 
                      , marker = list(size = 25, color = 'LightGrey', opacity = 0.8, line = list(color = 'black', width = 1), symbol = ~formes
                                      
                      )
    ) %>%
    plotly::add_trace(data = OffTarget, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers', 
                      showlegend = FALSE
                      , marker = list(size = 25, opacity = 0.8, line = list(color = 'black', width = 1), symbol = ~formes
                                      , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                      , cmax = 1, cmin = 0
                      )
    ) %>%
    plotly::add_trace(data = Saved, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers', 
                      showlegend = FALSE
                      , marker = list(size = 25, opacity = 0.8, line = list(color = 'black', width = 2), symbol = ~formes
                                      , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                      , cmax = 1, cmin = 0
                      )
    ) %>%
    plotly::add_trace(data = Goal, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers' 
                      ,showlegend = FALSE
                      , marker = list(size = 27, color = 'white', opacity = 1, line = list(color = 'black', width = 1), symbol = ~formes)) %>%
    plotly::add_trace(data = Goal, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers' 
                      , showlegend = T 
                      , marker = list(size = 22, opacity = 1, line = list(color = 'black', width = 1), symbol = ~formes
                                      , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                      , cmax = 1, cmin = 0))
}




ShotMap(ShotTeam, 'against', 'Left', 'From Corner')



################Histogramme##############################

AllTeams <- StatsBombData %>% dplyr::select(team.name) %>% dplyr::distinct()

Index <- data.frame(index = as.factor(1:nrow(AllTeams)))

ShotEvents <- StatsBombData %>%
  dplyr::select(match_id, pass.type.name, possession_team.name, team.name, type.name, 
         shot.type.name, shot.statsbomb_xg, play_pattern.name)

InfosMatch <- Matches %>%
  dplyr::select(match_id, match_date, home_team.home_team_name, away_team.away_team_name, match_date)

ShotTeams <- ShotEvents %>%
  dplyr::left_join(InfosMatch) %>%
  dplyr::mutate(team_vs.name = ifelse(team.name == home_team.home_team_name, away_team.away_team_name, home_team.home_team_name))


##Corners Subis

CornerSubiTeams <- ShotTeams %>%
  dplyr::filter(pass.type.name == 'Corner') %>%
  dplyr::group_by(team_vs.name) %>%
  dplyr::count(team_vs.name)

ExpectedGoalCornerSubi <- ShotTeams %>%
  dplyr::filter(type.name == 'Shot') %>%
  dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
  dplyr::filter(play_pattern.name == 'From Corner') %>%
  dplyr::group_by(team_vs.name) %>%
  dplyr::summarise(xG_CornerSubi = sum(shot.statsbomb_xg, na.rm = T))

xG_CornerSubi <- CornerSubiTeams %>%
  dplyr::left_join(ExpectedGoalCornerSubi) %>%
  dplyr::mutate(xG = xG_CornerSubi/n) %>%
  dplyr::arrange(xG)

xGCornerSubi <- xG_CornerSubi %>% dplyr::bind_cols(index)


##Corners

CornerTeams <- ShotTeams %>%
  dplyr::filter(pass.type.name == 'Corner') %>%
  dplyr::group_by(team.name) %>%
  dplyr::count(team.name)

ExpectedGoalCorner <- ShotTeams %>%
  dplyr::filter(type.name == 'Shot') %>%
  dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
  dplyr::filter(play_pattern.name == 'From Corner') %>%
  dplyr::group_by(team.name) %>%
  dplyr::summarise(xG_Corner = sum(shot.statsbomb_xg, na.rm = T))

xG_Corner <- CornerTeams %>%
  dplyr::left_join(ExpectedGoalCorner) %>%
  dplyr::mutate(xG = xG_Corner/n) %>%
  dplyr::arrange(desc(xG))

xGCorner <- xG_Corner %>% dplyr::bind_cols(index)


##IFK Subis

IFKSubiTeams <- ShotTeams %>%
  dplyr::filter(pass.type.name == 'Free Kick') %>%
  dplyr::group_by(team_vs.name) %>%
  dplyr::count(team_vs.name)

ExpectedGoalIFKSubi <- ShotTeams %>%
  dplyr::filter(type.name == 'Shot') %>%
  dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
  dplyr::filter(play_pattern.name == 'From Free Kick') %>%
  dplyr::group_by(team_vs.name) %>%
  dplyr::summarise(xG_IFKSubi = sum(shot.statsbomb_xg, na.rm = T))

xG_IFKSubi <- IFKSubiTeams %>%
  dplyr::left_join(ExpectedGoalIFKSubi) %>%
  dplyr::mutate(xG = xG_IFKSubi/n) %>%
  dplyr::arrange(xG)

xGIFKSubi <- xG_IFKSubi %>% dplyr::bind_cols(index)

##IFK

IFKTeams <- ShotTeams %>%
  dplyr::filter(pass.type.name == 'Free Kick') %>%
  dplyr::group_by(team.name) %>%
  dplyr::count(team.name)

ExpectedGoalIFK <- ShotTeams %>%
  dplyr::filter(type.name == 'Shot') %>%
  dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
  dplyr::filter(play_pattern.name == 'From Free Kick') %>%
  dplyr::group_by(team.name) %>%
  dplyr::summarise(xG_IFK = sum(shot.statsbomb_xg, na.rm = T))

xG_IFK<- IFKTeams %>%
  dplyr::left_join(ExpectedGoalIFK) %>%
  dplyr::mutate(xG = xG_IFK/n) %>%
  dplyr::arrange(desc(xG))

xGIFK <- xG_IFK %>% dplyr::bind_cols(index)






#Histogrammes des comparaisons des équipes
xgcolors <- c('#de262a', '#f2323c', '#fd4968', '#fe618c', '#fb6d9c', '#f981b5', '#f4a2d8', '#f3ace1', '#f2b5e8', '#f1baed',
              '#fdc0ef', '#ceb0e5', '#9e90d1', '#8380c9', '#6f7ac4', '#6174bc', '#536baf', '#385691', '#28477e', '#06013f')

#Fonction qui créé les histogrammes de comparaison
comparaison_team <- function(data, type_action){
  ggplot(data = data, aes(x = reorder(team.name, xG), y = xG, fill = index)) +
    geom_bar(stat = 'identity', width = 0.8, colour = 'white') +
    labs(y = paste0('xG/', type_action)) +
    geom_vline(xintercept = 0.05, linetype = 'dashed', size = 1, color = '#d5d5d5') +
    theme(axis.line = element_line(size = 0.2, colour = 'black'),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=11, color="#333333", family="Source Sans Pro"),
          axis.title.x = element_text(size=14, color="#333333", family="Source Sans Pro"),
          axis.text.x = element_text(size=10, color="#333333", family="Source Sans Pro"),
          #axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", colour ="white"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
          plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
          plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
          text=element_text(family="Source Sans Pro"),
          legend.title=element_blank(),
          legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
          legend.position = "none") +
    labs(title = 'Ligue 1',
         subtitle = '2022/2023',
         caption = paste0('League Average xG/', type_action)) +
    scale_fill_manual(values = xgcolors) +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() 
}

#Fonction qui créé les histogrammes de comparaison
comparaison_team_subi <- function(data, type_action){
  ggplot(data = data, aes(x = reorder(team_vs.name, desc(xG)), y = xG, fill = index)) +
    geom_bar(stat = 'identity', width = 0.8, colour = 'white') +
    labs(y = paste0('xG/', type_action)) +
    geom_vline(xintercept = 0.05, linetype = 'dashed', size = 1, color = '#d5d5d5') +
    theme(axis.line = element_line(size = 0.2, colour = 'black'),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=11, color="#333333", family="Source Sans Pro"),
          axis.title.x = element_text(size=14, color="#333333", family="Source Sans Pro"),
          axis.text.x = element_text(size=10, color="#333333", family="Source Sans Pro"),
          #axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", colour ="white"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
          plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
          plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
          text=element_text(family="Source Sans Pro"),
          legend.title=element_blank(),
          legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
          legend.position = "none") +
    labs(title = 'Ligue 1',
         subtitle = '2022/2023',
         caption = paste0('League Average xG/', type_action)) +
    scale_fill_manual(values = xgcolors) +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() 
}

comparaison_team_subi(xGIFKSubi, 'Indirect Free Kick Conceded')


type_action = 'Indirect Free Kick Conceded'
xGmax = max(xGIFKSubi$xG)

plot_ly(xGIFKSubi, x = ~xG, y = ~reorder(team_vs.name, xG), type = 'bar', 
        color = ~index, colors = xgcolors,
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(
    xaxis = list(title = 'Expected Goals/Indirect Free Kick', tickangle = -45, color = 'white', showgrid = FALSE),
    yaxis = list(title = 'Team', color = 'white', showgrid = FALSE),
    showlegend = FALSE,
    shapes = list(
      list(
        type = 'line',
        x0 = 0.05,
        x1 = 0.05,
        y0 = 0,
        y1 = xGmax,
        line = list(color = '#d5d5d5', width = 1, dash = 'dash')
      )
    ),
    plot_bgcolor = '#8A1538',
    paper_bgcolor = '#8A1538'
  ) %>%
  add_annotations(
    text = 'League Average xG/Indirect Free Kick',
    x = 0.05,
    y = xGmax + 0.1,
    showarrow = FALSE,
    font = list(color = 'white')
  )





###############duels aériens############

TeamName <- StatsBombData %>%
  dplyr::select(team.id, team.name) %>%
  dplyr::distinct()

TeamId <- StatsBombR::get.minutesplayed(StatsBombData) %>%
  dplyr::select(player.id, team.id) %>%
  dplyr::distinct() %>%
  dplyr::left_join(TeamName)

minutesplayed <- StatsBombR::get.minutesplayed(StatsBombData) %>%
  dplyr::select(player.id, team.id, MinutesPlayed) %>%
  dplyr::group_by(player.id, team.id) %>%
  dplyr::mutate(MinutesPlayed = sum(MinutesPlayed)) %>%
  dplyr::distinct()

Aerial <- StatsBombData %>%
  dplyr::filter(miscontrol.aerial_won == T | pass.aerial_won == T |
                  shot.aerial_won == TRUE | clearance.aerial_won == TRUE |
                  duel.type.name == 'Aerial Lost')

AerialLost <- Aerial %>%
  dplyr::filter(duel.type.name == 'Aerial Lost') %>%
  dplyr::group_by(player.name, player.id) %>%
  count(player.name)

AerialWon <- Aerial %>%
  dplyr::filter(miscontrol.aerial_won == TRUE | pass.aerial_won == TRUE |
                  shot.aerial_won == TRUE | clearance.aerial_won == TRUE) %>%
  dplyr::group_by(player.name) %>%
  count(player.name)

AerialDuels <- AerialWon %>%
  dplyr::full_join(AerialLost, by = 'player.name') %>%
  dplyr::mutate(AerialWon = ifelse(is.na(n.x), 0, n.x)) %>%
  dplyr::mutate(AerialLost = ifelse(is.na(n.y), 0, n.y)) %>%
  dplyr::mutate(WinPercentage = (AerialWon / (AerialLost + AerialWon))) %>%
  dplyr::left_join(minutesplayed, by = 'player.id') %>%
  dplyr::mutate(AerialWonGame = (AerialWon / MinutesPlayed * 90)) %>%
  dplyr::filter(MinutesPlayed >= 90) %>%
  dplyr::select(player.name, AerialWon, WinPercentage, player.id, MinutesPlayed) %>%
  dplyr::arrange(AerialWon) %>%
  dplyr::left_join(TeamId, by = 'player.id') %>%
  dplyr::select(player.name, team.name, AerialWon, WinPercentage, MinutesPlayed) %>%
  dplyr::filter(team.name == 'Argentina') # %in% input$team

TeamPlayers <- AerialDuels %>% dplyr::select(player.name) %>% dplyr::distinct()
IndexPlayer <- data.frame(index = as.factor(1:nrow(TeamPlayers)))

AerialDuelsTeam <- AerialDuels %>%
  dplyr::bind_cols(IndexPlayer)


plot_ly(AerialDuelsTeam, x = ~AerialWon, y = ~reorder(player.name, AerialWon), type = 'bar', 
        color = ~IndexPlayer, colors = xgcolors,
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(
    xaxis = list(title = 'Aerial Duels Won', tickangle = -45, color = 'white', showgrid = FALSE),
    yaxis = list(title = 'Team', color = 'white', showgrid = FALSE),
    showlegend = FALSE,
    shapes = list(
      list(
        type = 'line',
        x0 = 0.05,
        x1 = 0.05,
        y0 = 0,
        y1 = xGmax,
        line = list(color = '#d5d5d5', width = 1, dash = 'dash')
      )
    ),
    plot_bgcolor = '#8A1538',
    paper_bgcolor = '#8A1538'
  ) %>%
  add_annotations(
    text = 'Aerial Duels Won',
    x = 0.05,
    y = xGmax + 0.1,
    showarrow = FALSE,
    font = list(color = 'white')
  )






