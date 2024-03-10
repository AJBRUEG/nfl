#Question: Have super bowl games gotten closer in score (outcome not known until later in the game)?
#How: view Win Probability (nflfastR) for super bowl games over the years


#-----SETUP-----

#setwd
setwd("C:/Users/drewj/OneDrive/R_based_work/nfl")

#libraries
library(nflfastR)
library(nflreadr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggpmisc)

#-----GETDATA-----

# #get data
# season_2022 <- nflreadr::load_pbp(seasons=2022)
# 
# #get super bowl
# ##NOTE: THIS WOULD BE THE LAST GAME_ID
# ##tail(df$game_id,n=1)
# sb22 <- season_2022 %>%
#   filter(game_id==tail(game_id,n=1))

#LOOP TO GET ALL THE DATA (HAVE THIS IN A LIST)
all_data <- as.list(seq(from=2000,to=2023,by=1))
all_data <- lapply(all_data,function(x){
  #print year
  print(x)
  #get data
  df <- nflreadr::load_pbp(seasons=x)
  #create column ID for season(year)
  df$season <- x
  #extract just the super bowl
  sb <- df %>%
    filter(game_id==tail(game_id,n=1))
  rm(df)
  #return sb
  return(sb)
  
})

#make one large dataframe
df1 <- dplyr::bind_rows(all_data,.id="identifier")


#-----USE PRE-CALCULATED WP-----

#the columns "home_wp" and "away_wp" give the win probabilities for the home and away teams, respectively
#the column "vegas_home_wp" gives the estimated win probability for the home team incorporating pre-game Vegas line
#this is, I assume, based off of the function "nflfastR::calculate_win_probability", which uses XGBOOST

#you'll need to determine who the winning team is
#the dataframe has "home_score" & "away_score"; you can check this at the end of the game to see who won (home or away)
#then use the columns "home_team" & "away_team" to ID who won
winners <- df1 %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(winner=ifelse(sum(home_score)>sum(away_score),home_team,away_team))

#combine winners with df1
df2 <- left_join(x=df1,y=winners,by="season")

#create columns that give the wp and vegas_wp for the winning team
#home_wp = win prob for home team
#vegas_home_wp = win prob for home team incorporating pre-game vegas line
df2 <- df2 %>%
  dplyr::mutate(wp_winner=ifelse(winner==home_team,home_wp,1-home_wp),
                vegas_wp_winner=ifelse(winner==home_team,vegas_home_wp,1-vegas_home_wp))


#graph wp and vegas_wp
ggplot(df2 %>%
         select(play_id,game_seconds_remaining,wp_winner,vegas_wp_winner,season))+
         #pivot_longer(cols=c(wp_winner,vegas_wp_winner),names_to="Metric",values_to="Probability")
         
  geom_line(aes(x=game_seconds_remaining,y=vegas_wp_winner,group=season,color=season),linewidth=1.5)+
  facet_wrap(.~season)+
  scale_x_reverse()+
  scale_y_continuous(labels=scales::percent)+
  #clean up graph
  theme(panel.background = element_blank())+
  theme(axis.line=element_line(color="black"))+
  theme(legend.position = "bottom")+
  #labels
  labs(x="Game Seconds Remaining",y="Winning Probability")+
  #hide legend
  theme(legend.position="none")+
  #draw line at 50%
  geom_hline(yintercept=.5,color="black")

#determine the last point in the game that the probability crossed the 50% line
df3 <- df2 %>%
  dplyr::filter(!is.na(game_seconds_remaining)) %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(last_p50=ifelse(min(vegas_wp_winner)>.5,3600,min(game_seconds_remaining[vegas_wp_winner<.5])))

#graph time remaining
ggplot(df3,aes(x=season,y=last_p50))+
  geom_point()+
  stat_poly_line()+
  stat_poly_eq(use_label(labels=c("R2")),
               label.x="right")+
  #labels
  labs(x="Season",y="Seconds remaining",title="Seconds Remaining in game\nwhen winning team had <50% probability of winning")

#CONCLUSION: Super bowl games have trended to being closer. It appears that since ~2007 games have been close up to the closing seconds of the game
#2000-2011: last time winning team had <50% chance of winning game was ~halftime
#2012-2023: last time winning team had <50% chance of winning game was in 4th quarter