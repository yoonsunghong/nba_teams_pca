# ===================================================================
# Title: make-teams-table
# Description:
#   This script creates a csv data file 'nba-2017-teams.csv'
# Input(s): data file 'nba2017-roster.csv', 'nba2017-stats.csv'
# Output(s): data files 'nba-2017-teams.csv'
# Author: Yoon Sung Hong
# Date: 10-05-2017
# ===================================================================

#packages and setting the working directory
library(readr)
library(dplyr)
library(ggplot2)
setwd("~/stat133/stat133-hws-fall17/hw03/")

#reading the data files
roster <- read.csv("data/nba2017-roster.csv", colClasses = c("player" = "character",
                                                             "team" = "character"
                                                             ))
stats <- read.csv("data/nba2017-stats.csv", colClasses = c("player" = "character")
                  )
str(roster)
str(stats)

#creating new variaables
stats <- mutate(stats, missed_fg = field_goals_atts - field_goals_made)
stats <- mutate(stats, missed_ft = points1_atts - points1_made)
stats <- mutate(stats, points = 3*(points3_made)+2*(points2_made)+points1_made)
stats <- mutate(stats, rebounds = off_rebounds + def_rebounds)
stats <- mutate(stats, efficiency=(points+ rebounds + assists + steals + blocks - missed_fg - missed_ft - turnovers)/games_played)
str(stats)

#sinking efficiency summary
sink(file = "output/efficiency-summary.txt")
summary(stats$efficiency)
sink()

#merging 2 dataframes; before merging, making sure that the both data is arranged the same way
roster <- arrange(roster, player)
stats <- arrange(stats, player)
dat <- merge(roster, stats)

#creating a new dataframe 
teams <- dat %>%
  group_by(team) %>%
  summarise(
    experience = round(sum(experience, na.rm = TRUE), digits = 2),
    salary = round(sum(salary)/1000000, digits = 2),
    points3 = sum(points3_made),
    points2 = sum(points2_made),
    free_throws = sum(points1_made),
    points = sum(points),
    off_rebounds = sum(off_rebounds),
    def_rebounds = sum(def_rebounds),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    turnovers = sum(turnovers),
    fouls = sum(fouls),
    efficiency = sum(efficiency)
  )
summary(teams)

#sinking the teams summary
sink(file = "output/teams-summary.txt")
summary(teams)
sink()

#exporting teams table
write_csv(teams, "data/nba2017-teams.csv")

#exporting pdf
pdf(file = "images/teams_star_plot.pdf")
stars(teams[ ,-1], labels = teams$team)
dev.off()

#exporting ggplot
pdf(file = "images/experience_salary.pdf")
ggplot(data = teams, aes(x = experience, y = salary)) + geom_label(label = teams$team)
dev.off()




