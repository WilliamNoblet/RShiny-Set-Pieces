


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


#Save the events database in Excel in xlsx format

Matches <- separate(
  Matches,
  away_team.managers,
  into = c("away_team.managers.id", "away_team.managers.name", "away_team.managers.nickname", 
           "away_team.managers.dob", "away_team.managers.country.id", "away_team.managers.country.name"),
  sep = ","
)

Matches <- separate(
  Matches,
  home_team.managers,
  into = c("home_team.managers.id", "home_team.managers.name", "home_team.managers.nickname", 
           "home_team.managers.dob", "home_team.managers.country.id", "home_team.managers.country.name"),
  sep = ","
)



library(stringr)


Matches$away_team.managers.id <- stringr::str_replace_all(Matches$away_team.managers.id, 'list\\(id = ', '')

Matches$away_team.managers.name <- stringr::str_replace_all(Matches$away_team.managers.name, 'name = ', '')
Matches$away_team.managers.name <- stringr::str_replace_all(Matches$away_team.managers.name, '"', '')

Matches$away_team.managers.nickname <- stringr::str_replace_all(Matches$away_team.managers.nickname, 'nickname = ', '')
Matches$away_team.managers.nickname <- stringr::str_replace_all(Matches$away_team.managers.nickname, '"', '')

Matches$away_team.managers.dob <- stringr::str_replace_all(Matches$away_team.managers.dob, 'dob = ', '')
Matches$away_team.managers.dob <- stringr::str_replace_all(Matches$away_team.managers.dob, '"', '')

Matches$away_team.managers.country.id <- stringr::str_replace_all(Matches$away_team.managers.country.id, 'country.id = ', '')

Matches$away_team.managers.country.name <- stringr::str_replace_all(Matches$away_team.managers.country.name, 'country.name = ', '')
Matches$away_team.managers.country.name <- stringr::str_replace_all(Matches$away_team.managers.country.name, '"', '')
Matches$away_team.managers.country.name <- stringr::str_replace_all(Matches$away_team.managers.country.name, '\\)', '')



Matches$home_team.managers.id <- stringr::str_replace_all(Matches$home_team.managers.id, 'list\\(id = ', '')

Matches$home_team.managers.name <- stringr::str_replace_all(Matches$home_team.managers.name, 'name = ', '')
Matches$home_team.managers.name <- stringr::str_replace_all(Matches$home_team.managers.name, '"', '')

Matches$home_team.managers.nickname <- stringr::str_replace_all(Matches$home_team.managers.nickname, 'nickname = ', '')
Matches$home_team.managers.nickname <- stringr::str_replace_all(Matches$home_team.managers.nickname, '"', '')

Matches$home_team.managers.dob <- stringr::str_replace_all(Matches$home_team.managers.dob, 'dob = ', '')
Matches$home_team.managers.dob <- stringr::str_replace_all(Matches$home_team.managers.dob, '"', '')

Matches$home_team.managers.country.id <- stringr::str_replace_all(Matches$home_team.managers.country.id, 'country.id = ', '')

Matches$home_team.managers.country.name <- stringr::str_replace_all(Matches$home_team.managers.country.name, 'country.name = "', '')
Matches$home_team.managers.country.name <- stringr::str_replace_all(Matches$home_team.managers.country.name, '"', '')
Matches$home_team.managers.country.name <- stringr::str_replace_all(Matches$home_team.managers.country.name, '\\)', '')



print(Matches)

#Save the events database in Excel in xlsx format
write.xlsx(Matches, 'Free_Matches_WC_2022.xlsx')




filtered_data <- StatsBombData[is.null(StatsBombData$tactics.lineup) | sapply(StatsBombData$tactics.lineup, is.null), ]


is.null(StatsBombData$tactics.lineup[[3]]) == TRUE

filtered_data <- StatsBombData %>%
  dplyr::filter(is.null(tactics.lineup) == TRUE)



type.name <- StatsBombData %>% dplyr::select(type.name) %>% distinct()


MatchesEvents <- StatsBombData %>% 
  dplyr::filter(!(type.name == 'Starting XI' | type.name == 'Tactical Shift')) %>%
  dplyr::select(-tactics.lineup)


ShotsEvents <- StatsBombData %>%
  dplyr::filter(type.name == 'Shot') %>%
  dplyr::select(
    id, index, period, timestamp, minute, second, possession, duration, related_events,
    location, under_pressure, off_camera, counterpress, out,type.id, type.name,
    possession_team.id, possession_team.name,
    play_pattern.id, play_pattern.name,
    team.id, team.name, tactics.formation,
    player.id, player.name, position.id, position.name,
    location.x, location.y,
    match_id, competition_id, season_id,
    
    shot.statsbomb_xg	,
    shot.end_location, shot.first_time, shot.freeze_frame, shot.key_pass_id,
    shot.one_on_one, shot.deflected, shot.open_goal, shot.aerial_won,
    shot.technique.id, shot.technique.name, shot.body_part.id, shot.body_part.name,
    shot.type.id, shot.type.name, shot.outcome.id, shot.outcome.name,
    shot.end_location.x, shot.end_location.y, shot.end_location.z,
    shot.follows_dribble, shot_impact_height,
    
    player.name.GK, player.id.GK, location.x.GK, location.y.GK, 
    DistToGoal, DistToKeeper,
    AngleToGoal, AngleToKeeper, AngleDeviation, avevelocity,
    DistSGK, density, density.incone, distance.ToD1,
    distance.ToD2, AttackersBehindBall, DefendersBehindBall, DefendersInCone,
    InCone.GK, DefArea, distance.ToD1.360, distance.ToD2.360,
    milliseconds, ElapsedTime, StartOfPossession, TimeInPoss,
    TimeToPossEnd
    )

nrow(ShotsEvents)

write.xlsx(ShotsEvents, 'ShotsEvents_WC_2022.xlsx')

PassesEvents <- StatsBombData %>%
  dplyr::filter(type.name == 'Pass') %>%
  dplyr::select(
    id, index, period, timestamp, minute, second, possession, duration, related_events,
    location, under_pressure, off_camera, counterpress, out,type.id, type.name,
    possession_team.id, possession_team.name,
    play_pattern.id, play_pattern.name,
    team.id, team.name, tactics.formation,
    player.id, player.name, position.id, position.name,
    location.x, location.y,
    match_id, competition_id, season_id,
    
    pass.length, pass.angle, pass.end_location, pass.cross,
    pass.assisted_shot_id, pass.shot_assist, pass.deflected, pass.aerial_won,
    pass.switch, pass.outswinging, pass.cut_back, pass.goal_assist,
    pass.through_ball, pass.miscommunication, pass.recipient.id, pass.recipient.name,
    pass.height.id, pass.height.name, pass.body_part.id, pass.body_part.name,
    pass.type.id, pass.type.name, pass.outcome.id, pass.outcome.name,
    pass.technique.id, pass.technique.name, carry.end_location, ball_receipt.outcome.id,
    pass.end_location.x, pass.end_location.y,
    pass.no_touch, dribble.overrun,
    pass.straight, pass.inswinging,
    
    player.name.GK, player.id.GK, location.x.GK, location.y.GK, 
    DistToGoal, DistToKeeper,
    AngleToGoal, AngleToKeeper, AngleDeviation, avevelocity,
    DistSGK, density, density.incone, distance.ToD1,
    distance.ToD2, AttackersBehindBall, DefendersBehindBall, DefendersInCone,
    InCone.GK, DefArea, distance.ToD1.360, distance.ToD2.360,
    milliseconds, ElapsedTime, StartOfPossession, TimeInPoss,
    TimeToPossEnd
  )

nrow(PassesEvents)

write.xlsx(PassesEvents, 'PassesEvents_WC_2022.xlsx')




colnames(StatsBombData)




