library(dplyr)
library(plotly)
library(StatsBombR)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)


ui <- shiny::fluidPage(
  shiny::navbarPage('Set Pieces, World Cup Qatar 2022',
                    theme = shinytheme('journal'), 
                    shiny::tabPanel('Team',
                                    fluid = T,
                                    selectInput(inputId = 'Team', 'Team', 
                                                choices = c('Denmark',
                                                            'Tunisia',
                                                            'Japan',
                                                            'Spain',
                                                            'Serbia',
                                                            'Switzerland',
                                                            'Australia',
                                                            'Brazil',
                                                            'Cameroon',
                                                            'Saudi Arabia',
                                                            'Mexico',
                                                            'Wales',
                                                            'England',
                                                            'South Korea',
                                                            'Portugal',
                                                            'Germany',
                                                            'Poland',
                                                            'Argentina',
                                                            'France',
                                                            'Ecuador',
                                                            'Senegal',
                                                            'Belgium',
                                                            'Canada',
                                                            'Uruguay',
                                                            'Iran',
                                                            'United States',
                                                            'Netherlands',
                                                            'Morocco',
                                                            'Croatia',
                                                            'Qatar',
                                                            'Costa Rica',
                                                            'Ghana'),
                                                selected = 'Argentina'
                                    )
                                    
                    ),
                    
                    shiny::tabPanel('Defensive set pieces',
                                    
                                    h2(paste0('Comparaison xG/corner défensif')),
                                    
                                    fluidRow(column(width = 6, plotlyOutput('comparaison_corner_def'))),
                                    
                                    h2(paste0('Corners défensifs')),
                                    
                                    fluidRow(column(width = 6, h2('Depuis les corners côtés gauche', align = 'left'), plotlyOutput('corner_left_def')),
                                             column(width = 6, h2('Depuis les corners côtés droit', align = 'left'), plotlyOutput('corner_right_def'))),
                                    
                                    fluidRow(column(width = 6, h2('Depuis les corners côtés gauche', align = 'left'), plotlyOutput('shot_corner_left_def')),
                                             column(width = 6, h2('Depuis les corners côtés droit', align = 'left'), plotlyOutput('shot_corner_rigth_def'))),
                                    
                                    h2(paste0('Comparaison xG/Coup franc indirect défensif')),
                                    
                                    fluidRow(column(width = 6, plotlyOutput('comparaison_ifk_def'))),
                                    
                                    h2(paste0('Tirs provenant de coup franc indirect défensif')),
                                    
                                    fluidRow(column(width = 6, plotlyOutput('shot_ifk_def')))
                    ),
                    
                    shiny::tabPanel('Offensive set pieces',
                                    
                                    h2(paste0('Comparaison xG/corner offensif')),
                                    
                                    shiny::fluidRow(shiny::column(width = 6, plotlyOutput('comparaison_corner_off'))),
                                    
                                    h2(paste0('Corners offensifs')),
                                    
                                    fluidRow(column(width = 6, h2('Depuis les corners côtés gauche', align = 'left'), plotlyOutput('corner_left_off')),
                                             column(width = 6, h2('Depuis les corners côtés droit', align = 'left'), plotlyOutput('corner_right_off'))),
                                    
                                    fluidRow(column(width = 6, h2('Depuis les corners côtés gauche', align = 'left'), plotlyOutput('shot_corner_left_off')),
                                             column(width = 6, h2('Depuis les corners côtés droit', align = 'left'), plotlyOutput('shot_corner_rigth_off'))),
                                    
                                    h2(paste0('Comparaison xG/Coup franc indirect offensif')),
                                    
                                    fluidRow(column(width = 6, plotlyOutput('comparaison_ifk_off'))),
                                    
                                    h2(paste0('Tirs provenant de coup franc indirect offensif')),
                                    
                                    fluidRow(column(width = 6, plotlyOutput('shot_ifk_off')))
                    ),
                    
                    shiny::tabPanel('Aerial Duels',
                                    
                                    selectInput(inputId = 'minutes', 'Minutes jouées', choices = c(300, 600),
                                                selected = 300),
                                    
                                    fluidRow(column(width = 6, h2('Comparaison du nombre de duels aériens gagnés', align = 'left'), plotlyOutput('comparaison_aerial_win'))),
                                    
                                    fluidRow(column(width = 6, h2('Comparaison des % de duels aériens gagnés', align = 'left'), plotlyOutput('comparaison_aerial_pourc'))),
                                    
                                    
                      downloadButton('describe_download',"Download Report",class="butt" ),br(),
                      tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                      radioButtons('format', 'Document format', c('PDF', 'Word'),
                                   inline = TRUE)
                    )
  )
)

server <- function(input, output, session){
  
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  if (!require("plotly")) install.packages("plotly")
  library(plotly)
  if (!require("openxlsx")) install.packages("openxlsx")
  library(openxlsx)
  library(StatsBombR)
  
  FreeCompetitions <- FreeCompetitions()
  
  Comp <- FreeCompetitions() %>%
    dplyr::filter(country_name == 'International' 
                  & competition_name == 'FIFA World Cup'
                  & season_name == '2022')
  
  Matches <- FreeMatches(Comp)
  
  StatsBombData <- get.matchFree(Matches[1,])
  
  for(i in seq(from = 2, to = nrow(Matches))){
    StatsBombData <- dplyr::bind_rows(StatsBombData, get.matchFree(Matches[i,]))
  }
  
  StatsBombData = allclean(StatsBombData)
  StatsBombData = cleanlocations(StatsBombData)
  
  
  
  
  InfoMatches <- reactive({
    Matches %>%
      dplyr::select(match_id, match_date, home_team.home_team_name, away_team.away_team_name)
  })
  
  Events <- reactive({
    StatsBombData %>%
      dplyr::left_join(InfoMatches(), by = 'match_id')
  })
  
  EventsTeam <- reactive({
    Events() %>%
      dplyr::filter(home_team.home_team_name %in% input$Team | away_team.away_team_name %in% input$Team)
  })
  
  NextCorner <- reactive({
    EventsTeam() %>%
      dplyr::filter(pass.type.name == 'Corner') %>%
      dplyr::filter(pass.outcome.name != 'Out' | is.na(pass.outcome.name)) %>%
      dplyr::select(match_id, index) %>%
      dplyr::mutate(index_next = index + 1) %>%
      dplyr::left_join(EventsTeam(), by = c('match_id', 'index_next' = 'index')) %>%
      dplyr::mutate(next_possession_team.name = possession_team.name) %>%
      dplyr::mutate(next_team.name = team.name) %>%
      dplyr::select(match_id, index, index_next, next_possession_team.name, next_team.name)
  })
  
  TeamCorner <- reactive({
    EventsTeam() %>%
      dplyr::filter(pass.type.name == 'Corner') %>%
      dplyr::filter(pass.outcome.name != 'Out' | is.na(pass.outcome.name)) %>%
      dplyr::left_join(NextCorner(), by = c('match_id', 'index')) %>%
      dplyr::mutate(FirstBall = ifelse(team.name == next_team.name, 'Won', 'Lost'))
  })
  
  TeamDefendingCorner <- reactive({
    TeamCorner() %>% 
      dplyr::filter(team.name != input$Team) 
  })
  
  
  
  CornerMap <- function(WhichTeam, Where){
    
    if (WhichTeam == 'for'){ 
      TeamCornerInfo <- reactive({
        TeamCorner() %>% 
          dplyr::filter(team.name %in% input$Team) 
      })
    } else {
      TeamCornerInfo <- reactive({
        TeamCorner() %>% 
          dplyr::filter(team.name != input$Team) 
      })
    }
    
    if (Where == 'Left'){
      Goal <- reactive({
        TeamCornerInfo() %>%
          dplyr::filter(pass.goal_assist == T) %>%
          dplyr::filter(location.y == 0.1)
      })
      
      Shot <- reactive({
        TeamCornerInfo() %>%
          dplyr::filter(pass.shot_assist == T) %>%
          dplyr::filter(location.y == 0.1)
      })
      
      Other <- reactive({
        TeamCornerInfo() %>%
          dplyr::filter(is.na(pass.shot_assist) & is.na(pass.goal_assist)) %>%
          dplyr::filter(location.y == 0.1)
      })
    } else {
      Goal <- reactive({
        TeamCornerInfo() %>%
          dplyr::filter(pass.goal_assist == T) %>%
          dplyr::filter(location.y > 50)
      })
      
      Shot <- reactive({
        TeamCornerInfo() %>%
          dplyr::filter(pass.shot_assist == T) %>%
          dplyr::filter(location.y > 50)
      })
      
      Other <- reactive({
        TeamCornerInfo() %>%
          dplyr::filter(is.na(pass.shot_assist) & is.na(pass.goal_assist)) %>%
          dplyr::filter(location.y > 50)
      })
    }
    
    plot_ly() %>%
      add_trace(
        data = Goal(),
        type = "scatter",
        mode = "markers",
        y = ~pass.end_location.x,
        x = ~pass.end_location.y,
        color = ~FirstBall,
        colors = c("#dc2228", "#3371ac"),
        marker = list(size = 10, symbol = 'circle'),
        legendgroup = "Goal"
      ) %>%
      add_trace(
        data = Shot(),
        type = "scatter",
        mode = "markers",
        y = ~pass.end_location.x,
        x = ~pass.end_location.y,
        color = ~FirstBall,
        colors = c("#dc2228", "#3371ac"),
        marker = list(size = 8, symbol = 'circle-open', line = list(color = ~FirstBall,
                                                                    colors = c("#dc2228", "#3371ac"), width = 3)),
        legendgroup = "Shot"
      ) %>%
      add_trace(
        data = Other(),
        type = "scatter",
        mode = "markers",
        y = ~pass.end_location.x,
        x = ~pass.end_location.y,
        color = ~FirstBall,
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
  
  corner_left_def_map <- reactive(CornerMap('against', 'Left'))
  
  corner_right_def_map <- reactive(CornerMap('against', 'Right'))
  
  corner_left_off_map <- reactive(CornerMap('for', 'Left'))
  
  corner_right_off_map <- reactive(CornerMap('for', 'Right'))
  
  
  #Map des corners gauches defensifs
  output$corner_left_def <- renderPlotly({corner_left_def_map()})
  
  #Map des corners droits défensifs
  output$corner_right_def <- renderPlotly({corner_right_def_map()})
  
  #Map des corners gauches offensifs
  output$corner_left_off <- renderPlotly({corner_left_off_map()})
  
  #Map des corners droits offensifs
  output$corner_right_off <- renderPlotly({corner_right_off_map()})
  
  
  
  
  ShotTeam <- reactive({
    StatsBombData %>%
      #dplyr::filter(team.name == 'Argentina') %>%
      #dplyr::left_join(IndexMatches(), by = 'match_id') %>%
      dplyr::filter(type.name == 'Shot') %>%
      dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
      dplyr::filter(location.x >= 90) %>%
      dplyr::mutate(technique = '') %>%
      dplyr::mutate(formes = '') %>%
      dplyr::mutate(color = ifelse(shot.outcome.name == 'Goal', '#109618', '#dc3912')) %>%
      dplyr::mutate(formes = ifelse(shot.body_part.name == 'Head', 'circle', 
                                    ifelse( shot.type.name == 'Free Kick', 'square', 'hexagon'))) %>%
      dplyr::mutate(technique = ifelse(shot.body_part.name == 'Head', 'Head', 
                                       ifelse( shot.type.name == 'Free Kick', 'Free Kick', 'Foot/Other')))
  })
  
  
  InfosCorner <- reactive({
    Events() %>%
      dplyr::filter(type.name == 'Pass') %>%
      dplyr::filter(pass.type.name == 'Corner' | pass.type.name == 'Free Kick') %>%
      dplyr::mutate(time_corner = timestamp,
                    location_corner = location) %>%
      dplyr::mutate(corner = ifelse(pass.type.name == 'Corner', 
                                    ifelse(location_corner == 'c(120, 80)', 'Right', 'Left'), 'Free Kick')) %>%
      dplyr::select(match_id, possession, corner, time_corner)
  })
  
  InfosMatchTeam <- reactive({
    Matches %>%
      dplyr::select(match_id, match_date, home_team.home_team_name, away_team.away_team_name)
  })
  
  ShotTeamSetPieces <- reactive({
    ShotTeam() %>%
      dplyr::left_join(InfosMatchTeam()) %>%
      dplyr::mutate(team_vs.name = ifelse(possession_team.name == home_team.home_team_name, 
                                          away_team.away_team_name, home_team.home_team_name)) %>%
      dplyr::left_join(InfosCorner(), by = c('match_id', 'possession')) %>%
      separate(timestamp, into = c('heur', 'min', 'sec'), sep = ':') %>%
      separate(time_corner, into = c('h', 'm', 's'), sep = ':') %>%
      dplyr::mutate(time_diff = (as.numeric(min) * 60 + as.numeric(sec)) - (as.numeric(m) * 60 + as.numeric(s))) %>%
      dplyr::filter(corner == 'Right' & time_diff < 20 |corner == 'Left' & time_diff < 20 |corner == 'Free Kick' & time_diff < 15 )
  })
  
  ShotMap <- function(WhichTeam, Where, From){
    
    shotmapxgcolors <- c("#192780","#2a5d9f","#40a7d0","#87cdcf","#e7f8e6","#f4ef95","#fde960",
                         "#f5b94d","#ed8a37","#d54f1b","#bf0000","#7f0000")
    
    if (WhichTeam == 'For'){
      DataSetPieces <- reactive({
        ShotTeamSetPieces() %>%
          dplyr::filter(team.name %in% input$Team) %>%
          dplyr::filter(play_pattern.name == From) %>%
          dplyr::filter(corner == Where)
      })
    } else {
      DataSetPieces <- reactive({
        ShotTeamSetPieces() %>%
          dplyr::filter(team_vs.name %in% input$Team) %>%
          dplyr::filter(play_pattern.name == From) %>%
          dplyr::filter(corner == Where)
      })
    }
    
    Goal <- reactive({DataSetPieces() %>% dplyr::filter(shot.outcome.name == 'Goal')})
    Blocked <- reactive({DataSetPieces() %>% dplyr::filter(shot.outcome.name == 'Blocked')})
    OffTarget <- reactive({DataSetPieces() %>% dplyr::filter(shot.outcome.name == 'Off T')})
    Saved <- reactive({DataSetPieces() %>% dplyr::filter(shot.outcome.name == 'Saved')})
    Other <- reactive({DataSetPieces() %>% dplyr::filter(shot.outcome.name == 'Wayward'|
                                                           shot.outcome.name == 'Post'|
                                                           shot.outcome.name == 'Saved to Post')})
    
    xG <- reactive({DataSetPieces() %>% dplyr::select(shot.statsbomb_xg)})
    xpectedGoal <- round(sum(xG()), digits = 2)
    
    ShotNb <- count(DataSetPieces())
    GoalSelect <- reactive({DataSetPieces() %>% dplyr::filter(shot.outcome.name == 'Goal')})
    GoalNb <- count(GoalSelect())
    
    
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
      plotly::add_trace(data = Blocked(), y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers',
                        showlegend = FALSE 
                        , marker = list(size = 25, color = 'LightGrey', opacity = 0.8, line = list(color = 'black', width = 1), symbol = ~formes
                                        
                        )
      ) %>%
      plotly::add_trace(data = OffTarget(), y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers', 
                        showlegend = FALSE
                        , marker = list(size = 25, opacity = 0.8, line = list(color = 'black', width = 1), symbol = ~formes
                                        , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                        , cmax = 1, cmin = 0
                        )
      ) %>%
      plotly::add_trace(data = Saved(), y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers', 
                        showlegend = FALSE
                        , marker = list(size = 25, opacity = 0.8, line = list(color = 'black', width = 2), symbol = ~formes
                                        , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                        , cmax = 1, cmin = 0
                        )
      ) %>%
      plotly::add_trace(data = Goal(), y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers' 
                        ,showlegend = FALSE
                        , marker = list(size = 27, color = 'white', opacity = 1, line = list(color = 'black', width = 1), symbol = ~formes)) %>%
      plotly::add_trace(data = Goal(), y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers' 
                        , showlegend = T 
                        , marker = list(size = 22, opacity = 1, line = list(color = 'black', width = 1), symbol = ~formes
                                        , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                        , cmax = 1, cmin = 0))
  }
  
  
  shot_map_corner_left_def <- reactive(ShotMap('Against', 'Left', 'From Corner'))
  
  shot_map_corner_right_def <- reactive(ShotMap('Against', 'Right', 'From Corner'))
  
  shot_map_ifl_def <- reactive(ShotMap('Against', 'Free Kick', 'From Free Kick'))
  
  shot_map_corner_left_off <- reactive(ShotMap('For', 'Left', 'From Corner'))
  
  shot_map_corner_right_off <- reactive(ShotMap('For', 'Right', 'From Corner'))
  
  shot_map_ifl_off <- reactive(ShotMap('For', 'Free Kick', 'From Free Kick'))
  
  
  
  #Map des tirs provenant de corners défensifs
  output$shot_corner_left_def <- renderPlotly({shot_map_corner_left_def()})
  
  output$shot_corner_rigth_def <- renderPlotly({shot_map_corner_right_def()})
  
  #Map des tirs provenant des coup franc indirect défentsifs
  output$shot_ifk_def <- renderPlotly({shot_map_ifl_def()})
  
  #Map des tirs provenant des cornes offensifs
  output$shot_corner_left_off <- renderPlotly({shot_map_corner_left_off()})
  
  output$shot_corner_rigth_off <- renderPlotly({shot_map_corner_right_off()})
  
  #Map des tirs provenant de coup franc indirect offensifs
  output$shot_ifk_off <- renderPlotly({shot_map_ifl_off()})
  
  
  
  
  AllTeams <- StatsBombData %>% dplyr::select(team.name) %>% dplyr::distinct()
  
  Index <- data.frame(index = as.factor(1:nrow(AllTeams)))
  
  ShotEvents <- reactive({
    StatsBombData %>%
      dplyr::select(match_id, pass.type.name, possession_team.name, team.name, type.name, 
                    shot.type.name, shot.statsbomb_xg, play_pattern.name)
  })
  
  InfosMatch <- reactive({
    Matches %>%
      dplyr::select(match_id, match_date, home_team.home_team_name, away_team.away_team_name, match_date)
  })
  
  ShotTeams <- reactive({
    ShotEvents() %>%
      dplyr::left_join(InfosMatch()) %>%
      dplyr::mutate(team_vs.name = ifelse(team.name == home_team.home_team_name, 
                                          away_team.away_team_name, home_team.home_team_name))
  })
  
  xgcolors <- c('#de262a', '#f2323c', '#fd4968', '#fe618c', '#fb6d9c', '#f981b5', '#f4a2d8', '#f3ace1', '#f2b5e8', '#f1baed',
                '#fdc0ef', '#ceb0e5', '#9e90d1', '#8380c9', '#6f7ac4', '#6174bc', '#536baf', '#385691', '#28477e', '#06013f')
  
  comparaison_team <- function(TeamName, PassType){
    
    PassTypeTeam <- reactive({
      ShotTeams() %>%
        dplyr::filter(pass.type.name == PassType) %>%
        dplyr::group_by(team.name) %>%
        dplyr::count(team.name)
    })
    
    ExpectedGoal <- reactive({
      ShotTeams() %>%
        dplyr::filter(type.name == 'Shot') %>%
        dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
        dplyr::filter(play_pattern.name == paste0('From ', PassType)) %>%
        dplyr::group_by(team.name) %>%
        dplyr::summarise(sum.xG = sum(shot.statsbomb_xg, na.rm = T))
    })
    
    xG <- reactive({
      PassTypeTeam() %>%
        dplyr::left_join(ExpectedGoal()) %>%
        dplyr::mutate(xG = sum.xG/n) %>%
        dplyr::arrange(desc(xG)) %>%
        dplyr::bind_cols(Index)
    })
    
    plot_ly(xG(), x = ~xG, y = ~reorder(team.name, xG), type = 'bar', 
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
            #y1 = xGmax,
            line = list(color = '#d5d5d5', width = 1, dash = 'dash')
          )
        ),
        plot_bgcolor = '#8A1538',
        paper_bgcolor = '#8A1538'
      ) %>%
      add_annotations(
        text = 'League Average xG/Indirect Free Kick',
        x = 0.05,
        #y = xGmax + 0.1,
        showarrow = FALSE,
        font = list(color = 'white')
      )
    
    
  } 
  
  
  comparaison_ifk_defff <- reactive(comparaison_team(TeamName = 'team_vs.name', 'Free Kick'))
  
  comparaison_ifk_offf <- reactive(comparaison_team(TeamName = 'team.name', 'Free Kick'))
  
  comparaison_corner_defff <- reactive(comparaison_team(TeamName = 'team_vs.name', 'Corner'))
  
  comparaison_corner_offf <- reactive(comparaison_team(TeamName = 'team.name', 'Corner'))
  
  
  
  #Histogramme des xG par coup franc indirect défensif
  output$comparaison_ifk_def <- renderPlotly({comparaison_ifk_defff()})
  
  #Histogramme des xG par coup franc indirect offensif
  output$comparaison_ifk_off <- renderPlotly({comparaison_ifk_offf()})
  
  #Histogramme des xG par corner défensif
  output$comparaison_corner_def <- renderPlotly({comparaison_corner_defff()})
  
  #Histogramme des xG par corner offensif
  output$comparaison_corner_off <- renderPlotly({comparaison_corner_offf()})
  
  
  
  
  
  
  TeamName <- reactive({
    StatsBombData %>%
      dplyr::select(team.id, team.name) %>%
      dplyr::distinct()
  })
  
  TeamId <- reactive({
    get.minutesplayed(StatsBombData) %>%
      dplyr::select(player.id, team.id) %>%
      dplyr::distinct() %>%
      dplyr::left_join(TeamName())
  })
  
  minutesplayed <- reactive({
    get.minutesplayed(StatsBombData) %>%
      dplyr::select(player.id, team.id, MinutesPlayed) %>%
      dplyr::group_by(player.id, team.id) %>%
      dplyr::mutate(MinutesPlayed = sum(MinutesPlayed)) %>%
      dplyr::distinct()
  })
  
  Aerial <- reactive({
    StatsBombData %>%
      dplyr::filter(miscontrol.aerial_won == T | pass.aerial_won == T |
                      shot.aerial_won == T | clearance.aerial_won == T |
                      duel.type.name == 'Aerial Lost')
  })
  
  AerialLost <- reactive({
    Aerial() %>%
      dplyr::filter(duel.type.name == 'Aerial Lost') %>%
      dplyr::group_by(player.name, player.id) %>%
      count(player.name)
  })
  
  AerialWon <- reactive({
    Aerial() %>%
      dplyr::filter(miscontrol.aerial_won == T | pass.aerial_won == T |
                      shot.aerial_won == T | clearance.aerial_won == T) %>%
      dplyr::group_by(player.name) %>%
      count(player.name)
  })
  
  AerialDuels <- reactive({
    AerialWon() %>%
      dplyr::full_join(AerialLost(), by = 'player.name') %>%
      dplyr::mutate(AerialWon = ifelse(is.na(n.x), 0, n.x)) %>%
      dplyr::mutate(AerialLost = ifelse(is.na(n.y), 0, n.y)) %>%
      dplyr::mutate(WinPercentage = (AerialWon / (AerialLost + AerialWon))) %>%
      dplyr::left_join(minutesplayed(), by = 'player.id') %>%
      dplyr::mutate(AerialWonGame = (AerialWon / MinutesPlayed * 90)) %>%
      dplyr::filter(MinutesPlayed >= 90) %>%
      dplyr::select(player.name, AerialWon, WinPercentage, player.id, MinutesPlayed) %>%
      dplyr::arrange(AerialWon) %>%
      dplyr::left_join(TeamId(), by = 'player.id') %>%
      dplyr::select(player.name, team.name, AerialWon, WinPercentage, MinutesPlayed) %>%
      dplyr::filter(team.name == input$Team)
  })
  
  TeamPlayers <- reactive({
    AerialDuels() %>%
      dplyr::select(player.name) %>%
      dplyr::distinct()
  })
  
  IndexPlayer <- reactive(data.frame(index = as.factor(1:nrow(TeamPlayers()))))
  
  AerialDuelsTeam <- reactive({
    AerialDuels() %>%
      dplyr::bind_cols(IndexPlayer())
  })
  
  
  
  AerialWonRank <- function(data){
    plot_ly(AerialDuelsTeam(), x = ~AerialWon, y = ~reorder(player.name, AerialWon), type = 'bar', 
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
            #y1 = xGmax,
            line = list(color = '#d5d5d5', width = 1, dash = 'dash')
          )
        ),
        plot_bgcolor = '#8A1538',
        paper_bgcolor = '#8A1538'
      ) %>%
      add_annotations(
        text = 'Aerial Duels Won',
        x = 0.05,
        #y = xGmax + 0.1,
        showarrow = FALSE,
        font = list(color = 'white')
      )
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
}


##shinyApp(ui, server)



