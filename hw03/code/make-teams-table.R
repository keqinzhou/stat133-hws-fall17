# ===================================================================
# Title: Ranking NBA Teams 
# Description:
#   This script performs tasks on exporting and analyzing data.
# Input(s): data file 'nba2017-players.csv'
# Output(s): data file 'summary-height-weight.txt'
# Author: Katherine Zhou
# Date: 10-09-2017
# ===================================================================
library(readr)
library(dplyr)
library(ggplot2)
# data preparation

#raw data and dictionaries
roster <- read.csv('data/nba2017-roster.csv',header=TRUE, sep=",")
stats <- read.csv('data/nba2017-stats.csv',header=TRUE,sep=",")
ros<- data.frame(roster)
sta <- data.frame(stats)

# add new variables and efficiency calculation
missed_fg <- sta$field_goals_atts-sta$field_goals_made
missed_ft <- sta$points1_atts-sta$points1_made
points <- sta$points1_made+2*sta$points2_made+3*sta$points3_made
rebounds <-sta$off_rebounds+sta$def_rebounds
efficiency <- (points+rebounds+sta$assists+sta$steals+sta$blocks-missed_fg-missed_ft-sta$turnovers)/sta$games_played
sta <- mutate(sta,missed_fg=missed_fg,missed_ft=missed_ft,points=points,rebounds=rebounds,efficiency=efficiency)

# create nba2017_teams.csv
sink(file = 'output/efficiency-summary.txt')
summary(sta$efficiency)
sink()

dat <- merge(sta,ros)
dat_t <- summarise(
  group_by(dat,team),
  experience=sum(experience),
  salary=sum(salary)/10^6,
  points3=sum(points3_made),
  points2=sum(points2_made),
  free_throws=sum(points1_made),
  points=sum(points),
  off_rebounds=sum(off_rebounds),
  def_rebounds=sum(def_rebounds),
  assists=sum(assists),
  steals=sum(steals),
  blocks=sum(blocks),
  turnovers=sum(turnovers),
  fouls=sum(fouls),
  efficiency=sum(efficiency)
)
sink(file='output/teams-summary.txt')
summary(dat_t)
sink()
write.csv(dat_t,file='data/nba2017-teams.csv',row.names=FALSE)
teams <- dat_t$team

# star and experience_salary plots
pdf(file='images/teams_star_plot.pdf')
stars(dat_t[,-1],labels=as.character(dat_t$team))
dev.off()


ggplot(dat_t,aes(x=experience,y=salary))+
         geom_point(aes(color = team))
ggsave('images/experience_salary.pdf')   
         
