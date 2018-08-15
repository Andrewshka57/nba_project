#Statystyka gierek
game_17_18 <- fread('2017-18_teamBoxScore.csv')
game_16_17 <- fread('2016-17_teamBoxScore.csv')
games_all <- rbind(game_16_17, game_17_18)

rm(game_16_17)
rm(game_17_18)


##Statystyka graczy per game
players_16_17 <- fread('2016-17_playerBoxScore.csv')
players_17_18 <- fread('2017-18_playerBoxScore.csv')
players_all <- rbind(players_16_17, players_17_18)

rm(players_16_17)
rm(players_17_18)

#Ranking per day
stand_17_18 <- fread('2017-18_standings.csv')
stand_16_17 <- fread('2016-17_standings.csv')
stand_all <- rbind(stand_16_17, stand_17_18)

rm(stand_16_17)
rm(stand_17_18)

# head(games_all)
# head(stand_all)
# head(players_all)
stand_all$stDate <- as.Date(stand_all$stDate)
games_all$gmDate <- as.Date(games_all$gmDate)
players_all$gmDate <- as.Date(players_all$gmDate)
players_all$player <- paste0(players_all$playFNm, '_', players_all$playLNm)

players_all <- players_all[, .(gmDate, teamAbbr, player, playStat, playMin, playPTS, playAST, playTO, playSTL, playBLK, playFGA, playFGM, play2PA, play2PM, play3PA,play3PM, playORB, playDRB, opptAbbr)]

players_all <- players_all[order(gmDate, teamAbbr, playStat, player)]

#Average def rebounds per game 
stand_all$avg_def_rebounds <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_def_rebounds[i] <- mean(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamDRB)
} 


#Average off rebounds per game
stand_all$avg_off_rebounds <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_off_rebounds[i] <- mean(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamORB)
}

#Average deff rebounds per game with exactly this team
games_all$avg_deff_rebounds_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_deff_rebounds_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamDRB)
}

#Average off rebounds per game with exactly this team
games_all$avg_off_rebounds_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_off_rebounds_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamORB)
}

#View(games_all[, .(avg_deff_rebounds_exactly, avg_off_rebounds_exactly)])


#Average points per game made with exactly this team
games_all$avg_points_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_points_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamPTS)
}


#Average points per game lost with exactly this team
games_all$avg_points_lost_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_points_lost_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$opptPTS)
}


#View(games_all[, .(avg_points_exactly, avg_points_lost_exactly)])


#Average turnovers per game all
stand_all$avg_turnovers_all <- NA
for (i in 1:nrow(games_all)) {
  stand_all$avg_turnovers_all[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$teamTO)
}


#Average turnovers per game exactly with this team
games_all$avg_turnovers_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_turnovers_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamTO)
}


#Average assists per game all
stand_all$avg_assists_all <- NA
for (i in 1:nrow(games_all)) {
  stand_all$avg_assists_all[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$teamAST)
}


#Average assists per game exactly with this team
games_all$avg_assists_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_assists_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamAST)
}


#Average steals per game all
stand_all$avg_steals_all <- NA
for (i in 1:nrow(games_all)) {
  stand_all$avg_steals_all[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$teamSTL)
}


#Average steals per game exactly with this team
games_all$avg_steals_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_steals_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamSTL)
}



#Average blocks per game all
stand_all$avg_blocks_all <- NA
for (i in 1:nrow(games_all)) {
  stand_all$avg_blocks_all[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$teamBLK)
}


#Average blocks per game exactly with this team
games_all$avg_blocks_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$avg_blocks_exactly[i] <- mean(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$teamBLK)
}


#2 points percentage all
stand_all$two_points_prc_all <- NA
for (i in 1:nrow(games_all)) {
  stand_all$two_points_prc_all[i] <- sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team2PM)/sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team2PA)
}

#2 points percentage exactly with this team
games_all$two_points_prc_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$two_points_prc_exactly[i] <- sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$team2PM)/sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$team2PA)
}


#3 points percentage all
stand_all$three_points_prc_all <- NA
for (i in 1:nrow(games_all)) {
  stand_all$three_points_prc_all[i] <- sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team3PM)/sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team3PA)
}

#3 points percentage exactly with this team
games_all$three_points_prc_exactly <- NA
for (i in 1:nrow(games_all)) {
  games_all$three_points_prc_exactly[i] <- sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$team3PM)/sum(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i] & opptAbbr == games_all$opptAbbr[i]]$team3PA)
}

##############################################
#Last 5 games

#Average points per game last 5 games
stand_all$avg_points_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_points_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamPTS, 5))
} 

#Average points lost per game last 5 games
stand_all$avg_points_lost_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_points_lost_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$opptPTS, 5))
} 

#Average deff rebounds per game last 5 games
stand_all$avg_deff_rebounds_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_deff_rebounds_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamDRB, 5))
} 


#Average off rebounds per game last 5 games
stand_all$avg_off_rebounds_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_off_rebounds_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamORB, 5))
} 

#Average assists per game last 5 games
stand_all$avg_assists_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_assists_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamAST, 5))
} 

#Average turnovers per game last 5 games
stand_all$avg_turnovers_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_turnovers_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamTO, 5))
} 


#Average steals per game last 5 games
stand_all$avg_steals_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_steals_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamSTL, 5))
} 

#Average blocks per game last 5 games
stand_all$avg_blocks_5last <- NA
for (i in 1:nrow(stand_all)) {
  stand_all$avg_blocks_5last[i] <- mean(tail(games_all[gmDate < stand_all$stDate[i] & teamAbbr == stand_all$teamAbbr[i]]$teamBLK, 5))
} 


#2 points percentage last 5 games
stand_all$two_points_prc_5last <- NA
for (i in 1:nrow(games_all)) {
  stand_all$two_points_prc_5last[i] <- sum(tail(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team2PM, 5))/sum(tail(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team2PA, 5))
}

#3 points percentage last 5 games
stand_all$three_points_prc_5last <- NA
for (i in 1:nrow(games_all)) {
  stand_all$three_points_prc_5last[i] <- sum(tail(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team3PM, 5))/sum(tail(games_all[gmDate < games_all$gmDate[i] & teamAbbr == games_all$teamAbbr[i]]$team3PA, 5))
}


#################################################################
###Players stats###

players_all$player_avg_points_5last <- NA

for (i in 1:nrow(players_all)) {
  players_all$player_avg_points_5last[i] <- mean(tail(players_all[gmDate < players_all$gmDate[i] & player == players_all$player[i]]$playPTS, 5))
}


players_all$player_two_points_prc_5last <- NA

for (i in 1:nrow(players_all)) {
  players_all$player_two_points_prc_5last[i] <- sum(tail(players_all[gmDate < players_all$gmDate[i] & player == players_all$player[i]]$play2PM, 5))/sum(tail(players_all[gmDate < players_all$gmDate[i] & player == players_all$player[i]]$play2PA, 5))
}

players_all$player_three_points_prc_5last <- NA

for (i in 1:nrow(players_all)) {
  players_all$player_three_points_prc_5last[i] <- sum(tail(players_all[gmDate < players_all$gmDate[i] & player == players_all$player[i]]$play3PM, 5))/sum(tail(players_all[gmDate < players_all$gmDate[i] & player == players_all$player[i]]$play3PA, 5))
}

#Best starters per points

best_points <- players_all[playStat == 'Starter'][order(gmDate, teamAbbr, -player_avg_points_5last)]

#View(head(best_points))

stand_all$bestplayer1_points_5last <- NA

for (i in 1:nrow(stand_all)) {
  stand_all$bestplayer1_points_5last[i] <- best_points[gmDate == best_points$gmDate[i] & teamAbbr == best_points$teamAbbr[i]]$player_avg_points_5last[1]
}

stand_all$bestplayer2_points_5last <- NA

for (i in 1:nrow(stand_all)) {
  stand_all$bestplayer2_points_5last[i] <- best_points[gmDate == best_points$gmDate[i] & teamAbbr == best_points$teamAbbr[i]]$player_avg_points_5last[2]
}

stand_all$bestplayer3_points_5last <- NA

for (i in 1:nrow(stand_all)) {
  stand_all$bestplayer3_points_5last[i] <- best_points[gmDate == best_points$gmDate[i] & teamAbbr == best_points$teamAbbr[i]]$player_avg_points_5last[3]
}

stand_all$bestplayer4_points_5last <- NA

for (i in 1:nrow(stand_all)) {
  stand_all$bestplayer4_points_5last[i] <- best_points[gmDate == best_points$gmDate[i] & teamAbbr == best_points$teamAbbr[i]]$player_avg_points_5last[4]
}

stand_all$bestplayer5_points_5last <- NA

for (i in 1:nrow(stand_all)) {
  stand_all$bestplayer5_points_5last[i] <- best_points[gmDate == best_points$gmDate[i] & teamAbbr == best_points$teamAbbr[i]]$player_avg_points_5last[5]
}

#Best bench per points
best_bench_points <- players_all[playStat == 'Bench'][order(gmDate, teamAbbr, -player_avg_points_5last)]


stand_all$bestplayer6_points_5last <- NA

for (i in 1:nrow(stand_all)) {
  stand_all$bestplayer6_points_5last[i] <- best_bench_points[gmDate == best_bench_points$gmDate[i] & teamAbbr == best_bench_points$teamAbbr[i]]$player_avg_points_5last[1]
}

fwrite(stand_all, 'stand_all.csv')
fwrite(games_all, 'games_all.csv')
fwrite(players_all, 'players_all.csv')

rm(best_points)
rm(best_bench_points)
######################################################






# best_points[gmDate < best_points$gmDate[i] & teamAbbr == best_points$teamAbbr[i]]$play3PM
# 
# 
# View(best_points[teamAbbr == 'CLE' & gmDate == '2016-10-29', .(gmDate, teamAbbr, player, player_avg_points_5last)])
# best_points[teamAbbr == 'CLE' & gmDate == '2016-10-29']$player_avg_points_5last[1]
# 
# 
# tail(players_all[gmDate < '2016-11-05' & player == 'Kevin_Love']$playPTS, 5)
# head(players_all[teamAbbr == 'CLE' & player == 'Kevin_Love']$playPTS, 6)
# View(players_all[teamAbbr == 'CLE' & player == 'LeBron_James'])
# 
# 
# games_all[teamAbbr == 'MIA' & opptAbbr == 'MIL']$teamDRB
# 
# 
# 
# View(games_all[teamAbbr == 'HOU'])
# mean(games_all[gmDate < stand_all$stDate[2261] & teamAbbr == stand_all$teamAbbr[2261]]$teamDRB)
