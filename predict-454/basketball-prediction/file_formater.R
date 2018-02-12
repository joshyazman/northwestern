team_id_df <- read_csv('teams.csv')
clean_format <- function(csv_name){ 
  require(dplyr)
  require(readr)
  require(lubridate)
  
  season_averager <- function(yr){
    total.df <- list()
    for(i in seq(1,length(unique(yr$team_id)))){
      keeper <- unique(yr$team_id)[i]
      year.df.start <- yr%>%filter(team_id == keeper)
      if(nrow(year.df.start) < 12){
        next
      } 
      year.df.end <- list()
      for(n in seq(12,max(year.df.start$game))){
        if(n %in% year.df.start$game){ # check that the game num in question is in the data, otherwise the row will just be a dupe of the row prior
          gid <- year.df.start$game_id[year.df.start$game == n]
          wl <- year.df.start$outcome[year.df.start$game == n]
          year.df.end[[n]] <- year.df.start%>%filter(game < n)%>%
            select(-opp_id, -game_id, -year)%>%
            group_by(team_id)%>%
            summarise_all(funs(mean, sum), na.rm = T)%>%
            mutate(n.games = n,
                   game_id = gid,
                   outcome = wl)
        } else {
          next
        }
      }
      total.df[[i]] <- bind_rows(year.df.end)
    }
    return(bind_rows(total.df))
  }
  
  original <- read_csv(csv_name)%>%
    # clean up some of the data and/or convert to numbers
    mutate(ot = ifelse(gsub('[^0-9]','', outcome)=='',0,
                       as.numeric(gsub('[^0-9]','', outcome))),
           outcome = ifelse(grepl('W',outcome),1,0), 
           point.spread = points_for - points_against,
           drb.for = TRB.for - ORB.for,
           drb.against = TRB.against - ORB.for,
           home = ifelse(location == 'home',1,0),
           minutes = 40 + (ot*5),
           Date = as.Date(Date, format = '%m/%d/%Y'),
           year = ifelse(month(Date) < 5,
                         year(Date),
                         year(Date)+1))%>%
    # rename a bunch of columns and drop some in the process
    select(game = G, date = Date, outcome, ot, home, outcome, 
           point.spread, fg.pct.for = FGpct.for,  three.pct.for = `3Ppct.for`, 
           orb.for = ORB.for, drb.for, stl.for = STL, ast.for = AST.for, 
           blk.for = BLK,tov.for = TOV, fouls.for = PF, 
           fg.pct.against = FGpct.against, three.pct.against = `3Ppct.against`, 
           orb.against = ORB.against, drb.against, stl.against = STL.against, 
           ast.against = AST.against, blk.against = BLK.against, 
           tov.against = TOV.against, fouls.against = PF.against,
           FGA.for, FGA.against,`3PA.for`, `3PA.against`, FTA.for, FTA.against,
           FTpct.for, FTpct.against, minutes, Opponent,team, year)%>%
    left_join(team_id_df%>%select(team, team_id))%>%
    left_join(team_id_df%>%select(Opponent, opp_id = team_id))%>%
    mutate(game_id = ifelse(team_id < opp_id,
                            paste0(gsub('/','',date),team_id, opp_id),
                            paste0(gsub('/','',date), opp_id, team_id)),
           rownum = row_number(),
           team_game_id = paste0(team_id, game, year),
           opp_game_id = paste0(opp_id, game, year),
           fg.pct.spread = fg.pct.for - fg.pct.against,
           shots.for = (FGA.for/minutes)*40,
           shots.against = (FGA.against/minutes)*40,
           three.pct.spread = three.pct.for - three.pct.against,
           threes.for = (`3PA.for`/minutes)*40,
           threes.against = (`3PA.against`/minutes)*40,
           ft.pct.spread = FTpct.for - FTpct.against,
           ft.for = (FTA.for/minutes)*40,
           ft.against = (FTA.against/minutes)*40,
           orb.spread = orb.for - orb.against,
           orb.for = (orb.for/minutes)*40,
           orb.against.norm = (orb.against/minutes)*40,
           drb.spread = drb.for - drb.against,
           drb.for = (drb.for/minutes)*40,
           drb.against = (drb.against/minutes)*40,
           steal.spread = stl.for - stl.against,
           steal.for = (stl.for/minutes)*40,
           steal.against = (stl.against/minutes)*40,
           assist.spread = ast.for - ast.against,
           assist.for = (ast.for/minutes)*40,
           assist.against = (ast.against/minutes)*40,
           block.spread = blk.for - blk.against,
           blk.for = (blk.for/minutes)*40,
           blk.against = (blk.against/minutes)*40,
           tov.spread = tov.for - tov.against,
           tov.for = (tov.for/minutes)*40,
           tov.against = (tov.against/minutes)*40,
           foul.spread = fouls.for - fouls.against,
           fouls.for = (fouls.for/minutes)*40,
           fouls.against = (fouls.against/minutes)*40)%>%
    # filter(!is.na(game_id))%>%
    select(game_id, team_id, opp_id, outcome, ot, home, point.spread, fg.pct.spread, 
           shots.for, shots.against, three.pct.spread, threes.for, threes.against, 
           ft.pct.spread, ft.for, ft.against, orb.spread, orb.for, orb.against,
           drb.spread, drb.for, drb.against, steal.spread, steal.for, steal.against,
           assist.spread, assist.for, assist.against, block.spread,
           blk.for, blk.against, tov.spread, tov.for, tov.against, foul.spread, 
           fouls.for, fouls.against, year, game)
  
  if(length(unique(original$year)) > 1){
    year_list <- list()
    for(i in unique(original$year)){
      year_list[[i]] <- original%>%filter(year == i)
    }
    year_list <- year_list[unique(original$year)]  
    
    team_averages <- list()
    for(i in seq(1,length(year_list))){
      team_averages[[i]] <- season_averager(year_list[[i]])
    }
    all_averages <- bind_rows(team_averages)
  } else {
    all_averages <- season_averager(original)
  }
  
  game_log <- original%>%
    group_by(game_id)%>%
    top_n(1,wt = team_id)%>%
    ungroup()
  
  games_clean <- game_log%>%
    select(game_id, team_id, opp_id, outcome, ot, home, point.spread)%>%
    inner_join(all_averages, by = c('game_id','team_id'))%>%
    inner_join(all_averages, by = c('game_id','opp_id' = 'team_id'))%>%
    select(-outcome_sum.x, -home_sum.x, -point.spread_sum.x, -fg.pct.spread_sum.x,
           -shots.for_sum.x, -shots.against_sum.x, -three.pct.spread_sum.x, -threes.for_sum.x,
           -threes.against_sum.x, -ft.pct.spread_sum.x, -ft.for_sum.x, -ft.against_sum.x,
           -orb.spread_sum.x, -orb.for_sum.x, -orb.against_sum.x, -drb.spread_sum.x,
           -drb.for_sum.x, -drb.against_sum.x, -steal.spread_sum.x, -steal.for_sum.x,
           -steal.against_sum.x, -assist.spread_sum.x, -assist.for_sum.x, -assist.against_sum.x,
           -block.spread_sum.x, -blk.for_sum.x, -blk.against_sum.x, -tov.spread_sum.x,
           -tov.for_sum.x, -tov.against_sum.x, -foul.spread_sum.x, -fouls.for_sum.x,
           -fouls.against_sum.y, -game_sum.y, -outcome_sum.y, -outcome.y, -n.games.y, 
           -home_sum.y, -point.spread_sum.y, -fg.pct.spread_sum.y, -ot_mean.x, -ot_mean.y,
           -shots.for_sum.y, -shots.against_sum.y, -three.pct.spread_sum.y, -threes.for_sum.y,
           -threes.against_sum.y, -ft.pct.spread_sum.y, -ft.for_sum.y, -ft.against_sum.y,
           -orb.spread_sum.y, -orb.for_sum.y, -orb.against_sum.y, -drb.spread_sum.y,
           -drb.for_sum.y, -drb.against_sum.y, -steal.spread_sum.y, -steal.for_sum.y,
           -steal.against_sum.y, -assist.spread_sum.y, -assist.for_sum.y, -assist.against_sum.y,
           -block.spread_sum.y, -blk.for_sum.y, -blk.against_sum.y, -tov.spread_sum.y,
           -tov.for_sum.y, -tov.against_sum.y, -foul.spread_sum.y, -fouls.for_sum.y,
           -fouls.against_sum.y, -game_sum.y)%>%
    mutate(mean_spread_diff = point.spread_mean.x-point.spread_mean.y,
           mean_fg_pct_spread_diff = fg.pct.spread_mean.x-fg.pct.spread_mean.y,
           mean_shots_for_diff = shots.for_mean.x-shots.for_mean.y,
           mean_shots_against_diff = shots.against_mean.x-shots.against_mean.y,
           mean_three_pct_diff = three.pct.spread_mean.x-three.pct.spread_mean.y,
           mean_threes_for_diff = threes.for_mean.x-threes.for_mean.y,
           mean_threes_against_diff = threes.against_mean.x-threes.against_mean.y,
           mean_ft_pct_diff = ft.pct.spread_mean.x-ft.pct.spread_mean.y,
           mean_ft_for_diff = ft.for_mean.x-ft.for_mean.y,
           mean_ft_against_diff = ft.against_mean.x-ft.against_mean.y,
           mean_orb_spread_diff = orb.spread_mean.x-orb.spread_mean.y,
           mean_orb_for_diff = orb.for_mean.x-orb.for_mean.y,
           mean_orb_against_diff = orb.against_mean.x-orb.against_mean.y,
           mean_drb_spread_diff = drb.spread_mean.x-drb.spread_mean.y,
           mean_drb_for_diff = drb.for_mean.x-drb.for_mean.y,
           mean_drb_against_diff = drb.against_mean.x-drb.against_mean.y,
           mean_steal_spread_diff = steal.spread_mean.x-steal.spread_mean.y,
           mean_steal_for_diff = steal.for_mean.x-steal.for_mean.y,
           mean_steal_against_diff = steal.against_mean.x-steal.against_mean.y,
           mean_assist_spread_diff = assist.spread_mean.x-assist.spread_mean.y,
           mean_assist_for_diff = assist.for_mean.x-assist.for_mean.y,
           mean_assist_against_diff = assist.against_mean.x-assist.against_mean.y,
           mean_foul_spread_diff = foul.spread_mean.x-foul.spread_mean.y,
           mean_foul_for_diff = fouls.for_mean.x-fouls.for_mean.y,
           mean_foul_against_diff = fouls.against_mean.x-fouls.against_mean.y,
           mean_tov_spread_diff = tov.spread_mean.x-tov.spread_mean.y,
           mean_tov_for_diff = tov.for_mean.x-tov.for_mean.y,
           mean_tov_against_diff = tov.against_mean.x-tov.against_mean.y,
           mean_block_spread_diff = block.spread_mean.x-block.spread_mean.y,
           mean_block_for_diff = blk.for_mean.x-blk.for_mean.y,
           mean_block_against_diff = blk.against_mean.x-blk.against_mean.y,
           sum_ot_diff = ot_sum.x - ot_sum.y)%>%
    select(game_id, team_id, opp_id, outcome = outcome.x, ot, home, point.spread,
           contains('_diff'))
  scaled_games <- games_clean%>%
    select(-game_id, -team_id, -opp_id, -outcome, -ot, -home, -point.spread)%>%
    scale(center = T, scale = T)%>%
    data.frame()%>%
    bind_cols(games_clean%>%
                select(game_id, team_id, opp_id, outcome, ot, home, point.spread))
  return(list(scaled_games, all_averages, game_log))
}
