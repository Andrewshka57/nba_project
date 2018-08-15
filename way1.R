library(data.table)
setwd("C:/Praca magisterska/NBA/nba-enhanced-stats")

games_all <- fread('games_all.csv')
stand_all <- fread('stand_all.csv')
players_all <- fread('players_all.csv')


#str(stand_all)
stand_all_final <- stand_all[, .(stDate, teamAbbr, gameWon, gameLost, homeWin, homeLoss, awayWin, awayLoss, lastFive, ptsScore, ptsAllow, sos, stkType, avg_def_rebounds, avg_off_rebounds, avg_turnovers_all, avg_assists_all, avg_steals_all, avg_blocks_all, avg_points_5last, avg_points_lost_5last, avg_deff_rebounds_5last, avg_off_rebounds_5last, avg_assists_5last, avg_turnovers_5last, avg_steals_5last, avg_blocks_5last, two_points_prc_5last, two_points_prc_all, three_points_prc_all, three_points_prc_5last, bestplayer1_points_5last, bestplayer2_points_5last, bestplayer3_points_5last, bestplayer4_points_5last, bestplayer5_points_5last, bestplayer6_points_5last)]

#str(games_all)
games_all_final <- games_all[, .(gmDate, gmTime, teamAbbr, teamConf, teamLoc, teamRslt, teamDayOff, opptAbbr, opptConf, opptDayOff, avg_deff_rebounds_exactly, avg_off_rebounds_exactly, avg_points_exactly, avg_points_lost_exactly, avg_turnovers_exactly, avg_assists_exactly, avg_steals_exactly, avg_blocks_exactly, two_points_prc_exactly, three_points_prc_exactly)]

test <- merge(games_all_final, stand_all_final, by.x = c("gmDate", "teamAbbr"), by.y = c("stDate", "teamAbbr"))

#str(test)

stand_all_final_opp <- stand_all_final
#colnames(stand_all_final_opp)
colnames(stand_all_final_opp)[3:37] <- paste0('opp_', colnames(stand_all_final_opp)[3:37])


test1 <- merge(test, stand_all_final_opp, by.x = c("gmDate", "opptAbbr"), by.y = c('stDate', 'teamAbbr'))
#colnames(test1)

games_all_final_opp <- games_all_final
#colnames(games_all_final_opp)
colnames(games_all_final_opp)[c(4:7, 11:20)] <- paste0('opp_', colnames(games_all_final_opp)[c(4:7, 11:20)])

test2 <- merge(test1, games_all_final_opp[, c(1, 3:7, 11:20)], by.x = c("gmDate", "opptAbbr"), by.y = c('gmDate', 'teamAbbr'))



#colnames(test2)


test2[, `:=`(diff_Dayoff = teamDayOff - opp_teamDayOff,
             diff_deff_rebounds_exactly = avg_deff_rebounds_exactly - opp_avg_deff_rebounds_exactly,
             diff_off_rebounds_exactly = avg_off_rebounds_exactly - opp_avg_off_rebounds_exactly,
             diff_points_exactly = (avg_points_exactly - avg_points_lost_exactly) - (opp_avg_points_exactly - opp_avg_points_lost_exactly),
             diff_turnovers_exactly = avg_turnovers_exactly - opp_avg_turnovers_exactly,
             diff_assits_exactly = avg_assists_exactly - opp_avg_assists_exactly,
             diff_steals_exactly = avg_steals_exactly - opp_avg_steals_exactly,
             diff_blocks_exactly = avg_blocks_exactly - opp_avg_blocks_exactly,
             diff_two_points_prc_exactly = two_points_prc_exactly - opp_two_points_prc_exactly,
             diff_three_points_prc_exactly = three_points_prc_exactly - opp_three_points_prc_exactly,
             diff_games = (gameWon - gameLost) - (opp_gameWon - opp_gameLost),
             diff_home_games = (homeWin - homeLoss) - (opp_homeWin - opp_homeLoss),
             diff_away_games = (awayWin - awayLoss) - (opp_awayWin - opp_awayLoss),
             diff_lastFive = lastFive - opp_lastFive,
             diff_pts = (ptsScore - ptsAllow) - (opp_ptsScore - opp_ptsAllow),
             diff_def_rebounds = avg_def_rebounds - opp_avg_def_rebounds,
             diff_off_rebounds = avg_off_rebounds - opp_avg_off_rebounds,
             diff_turnovers = avg_turnovers_all - opp_avg_turnovers_all,
             diff_assists = avg_assists_all - opp_avg_assists_all,
             diff_steals = avg_steals_all - opp_avg_steals_all,
             diff_blocks = avg_blocks_all - opp_avg_blocks_all,
             diff_points_5last = (avg_points_5last - avg_points_lost_5last) - (opp_avg_points_5last - opp_avg_points_lost_5last),
             diff_def_rebounds_5last = avg_deff_rebounds_5last - opp_avg_deff_rebounds_5last,
             diff_off_rebounds_5last = avg_off_rebounds_5last - opp_avg_off_rebounds_5last,
             
             diff_assists_5last = avg_assists_5last - opp_avg_assists_5last,
             diff_turnovers_5last = avg_turnovers_5last - opp_avg_turnovers_5last,
             diff_blocks_5last = avg_blocks_5last - opp_avg_blocks_5last,
             diff_steals_5last = avg_steals_5last - opp_avg_steals_5last,
             diff_two_points_prc = two_points_prc_all - opp_two_points_prc_all,
             diff_three_points_prc = three_points_prc_all - opp_three_points_prc_all,
             diff_two_points_prc_5last = two_points_prc_5last - opp_two_points_prc_5last,
             diff_three_points_prc_5last = three_points_prc_5last - opp_three_points_prc_5last)]


colnames(test2)

wynik <- test2[, .(gmDate,
                   gmTime,
                   #teamAbbr,
                   #opptAbbr,
                   teamConf,
                   teamLoc,
                   teamRslt,
                   diff_Dayoff,
                   diff_deff_rebounds_exactly, 
                   diff_off_rebounds_exactly, 
                   diff_points_exactly,
                   diff_turnovers_exactly,
                   diff_assits_exactly,
                   diff_steals_exactly, 
                   diff_blocks_exactly,
                   diff_two_points_prc_exactly, 
                   diff_three_points_prc_exactly,
                   diff_games,
                   diff_home_games, 
                   diff_away_games,
                   diff_lastFive,
                   diff_pts,
                   diff_def_rebounds,
                   diff_off_rebounds,
                   diff_turnovers,
                   diff_assists,
                   diff_steals,
                   diff_blocks,
                   diff_points_5last,
                   diff_def_rebounds_5last, 
                   diff_off_rebounds_5last, 
                   diff_assists_5last,
                   diff_turnovers_5last, 
                   diff_blocks_5last, 
                   diff_steals_5last, 
                   diff_two_points_prc, 
                   diff_three_points_prc,
                   diff_two_points_prc_5last, 
                   diff_three_points_prc_5last,
                   bestplayer1_points_5last,
                   bestplayer2_points_5last,
                   bestplayer3_points_5last,
                   bestplayer4_points_5last,
                   bestplayer5_points_5last,
                   bestplayer6_points_5last,
                   opp_bestplayer1_points_5last,
                   opp_bestplayer2_points_5last,
                   opp_bestplayer3_points_5last,
                   opp_bestplayer4_points_5last,
                   opp_bestplayer5_points_5last,
                   opp_bestplayer6_points_5last,
                   sos,
                   opp_sos
)]

#unique(wynik$gmDate)
sapply(wynik, function(x) {sum(is.na(x))})
wynik <- wynik[(gmDate >= '2016-11-25' &  gmDate <= '2017-04-12') | gmDate >= '2017-11-17']

#Dwa podejœcia:
#1) usun¹æ zmienne z brakami danych
#2) usun¹æ braki danych

##1)

sapply(wynik, function(x) {sum(is.na(x))})
wynik[, `:=`(diff_steals = NULL,
             diff_turnovers = NULL,
             diff_assists = NULL,
             diff_blocks = NULL,
             diff_two_points_prc = NULL,
             diff_three_points_prc = NULL,
             diff_two_points_prc_5last = NULL,
             diff_three_points_prc_5last = NULL
)]

wynik <- wynik[!(is.na(diff_assits_exactly))]
wynik <- wynik[!(is.na(bestplayer5_points_5last))]
wynik <- wynik[!(is.na(opp_bestplayer5_points_5last))]


rm(games_all, games_all_final, games_all_final_opp, players_all, stand_all, stand_all_final, stand_all_final_opp, test, test1, test2)
#str(wynik)

wynik$teamRslt <- as.factor(ifelse(wynik$teamRslt == 'Win', 1, 0))
wynik$teamLoc <- as.factor(ifelse(wynik$teamLoc == 'Home', 1, 0))
wynik$teamConf <- as.factor(ifelse(wynik$teamConf == 'West', 1, 0))
wynik$diff_sos = wynik$sos - wynik$opp_sos
wynik[, ':='(sos = NULL,
             opp_sos = NULL,
             gmDate = NULL,
             gmTime = NULL)]

###################################################################
################################################################
#Model building


#Test wspó³liniowoœci

library(corrplot)

cor_matrix = cor(wynik[, -c("teamConf", "teamLoc", "teamRslt", "id")])

t= c()
for (i in 1:length(colnames(cor_matrix))) {
  if (any((cor_matrix[, i] >= 0.5 | cor_matrix[, i] <= -0.5) & cor_matrix[, i] != 1)) {t <- c(t, colnames(cor_matrix[, i, drop = F]))
  } else {next}
 } 
#t - zmienne skorelowane (wsp.cor >= |0.5|)

dev.new(width=5, height=4)
corrplot(cor(wynik[, c(get("t"))]), method = 'number')

#diff_turnovers_exactly vs diff_steals_exactly = -0.73
var(wynik$diff_turnovers_exactly)
var(wynik$diff_steals_exactly)
#usuniemy diff_steals_exactly

#diff_games vs diff_home_games = 0.92
var(wynik$diff_games)
var(wynik$diff_home_games)
var(wynik$diff_away_games)
var(wynik$diff_pts)

#diff_games vs diff_away_games = 0.93
#diff_home_games vs diff_away_games = 0.72
#diff_games vs diff_pts = 0.87
#diff_home_games vs diff_pts = 0.81
#diff_away_games vs diff_pts = 0.81
#usuniemy diff_games, diff_home_games, diff_away_games

#diff_points_5last vs diff_lastFive = 0.73
var(wynik$diff_points_5last)
var(wynik$diff_lastFive)
#usuniemy diff_lastFive


corrplot(cor(wynik[, .(diff_games, diff_home_games, diff_away_games, diff_lastFive, diff_pts, diff_off_rebounds, diff_points_5last)]), method = 'number')

# wynik[, `:=`(diff_lastFive = NULL,
#              diff_games = NULL, 
#              diff_home_games = NULL,
#              diff_away_games = NULL,
#              diff_steals_exactly = NULL)]

#!!!!!!!!!#
#Usuniêcie zmiennych pogorszy³o model
#################################################
library(caTools)
set.seed(123)

wynik$id = c(1:3538)
split <- sample.split(wynik$id, SplitRatio = 0.60)
training_set <- subset(wynik, split == T)
test_set <- subset(wynik, split == F)

#str(training_set)
training_set$id = NULL


classifier <- glm(teamRslt ~ .,
                  family = binomial,
                  data = training_set)
summary(classifier)

str(training_set)


#Stepwise selection
library(caret)
library(leaps)
library(MASS)

?stepAIC()
# stepAIC() [MASS package], which choose the best model by AIC. It has an option named direction, which can take the following values: i) “both” (for stepwise regression, both forward and backward selection); “backward” (for backward selection) and “forward” (for forward selection). It return the best final model.

step.model <- stepAIC(classifier, direction = "both", 
                      trace = TRUE)
summary(step.model)

test_set$id <- NULL
#str(test_set)
prob_pred <- predict(classifier, type = 'response', newdata = test_set)
prob_pred_stepwise <- predict(step.model, type = 'response', newdata = test_set)

y_pred <- ifelse(prob_pred >= 0.5, 1, 0)
y_pred_stepwise <- ifelse(prob_pred_stepwise >= 0.5, 1, 0)

cm_glm <- table(test_set$teamRslt, y_pred)
cm_glm_stepwise <- table(test_set$teamRslt, y_pred_stepwise)

#Reczny dobor zmiennych

summary(step.model)

classifier_manual <- glm(teamRslt ~ diff_turnovers_exactly + diff_steals_exactly + diff_lastFive + diff_pts + diff_points_5last + diff_assists_5last + bestplayer6_points_5last + opp_bestplayer4_points_5last + diff_sos - 1,
                  family = binomial,
                  data = training_set)

summary(classifier_manual)

prob_pred_manual <- predict(classifier_manual, type = 'response', newdata = test_set)
y_pred_manual <- ifelse(prob_pred_manual >= 0.5, 1, 0)
cm_glm_manual <- table(test_set$teamRslt, y_pred_manual)


classifier_manual1 <- glm(teamRslt ~ diff_lastFive + diff_pts + diff_points_5last - 1,
                         family = binomial,
                         data = training_set)

summary(classifier_manual1)

prob_pred_manual1 <- predict(classifier_manual1, type = 'response', newdata = test_set)
y_pred_manual1 <- ifelse(prob_pred_manual1 >= 0.5, 1, 0)

cm_glm_manual1 <- table(test_set$teamRslt, y_pred_manual1)

cm_glm
acc_glm = (623+595)/(1416) #0.8601695
cm_glm_stepwise
acc_glm_stepwise = (621+595)/1416 #0.8587571
cm_glm_manual #gorzej
cm_glm_manual1 #
acc_glm_manual1 = (620+600)/1416 #0.8615819


#Drzewa decyzyjnie
library(rpart)
classifier_tree = rpart(teamRslt ~ .,
                        data = training_set, cp = 0)

summary(classifier_tree)
# Predicting the Test set results
y_pred_tree = predict(classifier_tree, newdata = test_set, type = 'class')

cm_tree = table(test_set$teamRslt, y_pred_tree)
acc_tree = (582+547)/1416 #0.7973164

library(rattle)

classifier_tree$cptable
#plotcp(classifier_tree)
cp_optimal <- classifier_tree$cptable[which(classifier_tree$cptable[,"xerror"]==min(classifier_tree$cptable[,"xerror"])),"CP"]
rpart_optimal <- prune(classifier_tree, cp_optimal)

y_pred_tree_optimal = predict(rpart_optimal, newdata = test_set, type = 'class')

cm_tree_optimal = table(test_set$teamRslt, y_pred_tree_optimal)
acc_tree_optimal = (629+569)/1416 #0.08460452
#fancyRpartPlot(rpart_optimal)


library(party)
ctree_initial <- ctree(teamRslt~., data=training_set,controls = ctree_control(teststat = "quad", testtype = "Bonferroni", mincriterion = 0.99))

ctree_prediction <- predict(ctree_initial, newdata=test_set)
cm_ctree = table(test_set$teamRslt, ctree_prediction)
acc_ctree = (629+569)/1416 #0.08460452



library(randomForest)
#str(training_set)
classifier_forest = randomForest(teamRslt ~ .,
                                 data = training_set,
                                 ntree = 500)

# Predicting the Test set results
y_pred_forest = predict(classifier_forest, newdata = test_set)

cm_forest = table(test_set$teamRslt, y_pred_forest)
acc_forest = (600+558)/1416

varImpPlot(classifier_forest)

mtry<-c(1:ncol(training_set))
error<-c()

for(i in 1:length(mtry)){
  print(i)
  error[i] <- randomForest(teamRslt~., data=training_set, ntree=500, mtry=mtry[i])$err.rate[500, 1]
}


plot(mtry, error, type="l")
which.min(error)
erpos100  <- which.min(error)
er100 <- error[which.min(error)]
erpos500 <- which.min(error)
er500 <- error[which.min(error)]

rf_optimal <- randomForest(teamRslt~., data=training_set, ntree=500, mtry=21, do.trace=T)

y_pred_forest_optimal = predict(rf_optimal, newdata = test_set)
cm_forest_optimal = table(test_set$teamRslt, y_pred_forest_optimal)
acc_forest_optimal = (611+582)/1416
