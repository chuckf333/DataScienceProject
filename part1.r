## ----part1-packages, message=FALSE, warning=FALSE------------------------

include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}
include("tidyverse")
include("dplyr")
include("ggplot2")


## ----part1-csvload, message=FALSE, warning=FALSE-------------------------
Salaries <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/Salaries.csv")
Batting <- read_csv("https://raw.github.com/chadwickbureau/baseballdatabank/master/core/Batting.csv")


## ----part1-csvtables, results='asis'-------------------------------------
head(Salaries)
head(Batting)


## ----part1-factorassign--------------------------------------------------
Batting$yearID <- as.factor(Batting$yearID)
Batting$playerID <- as.factor(Batting$playerID)
Batting$teamID <- as.factor(Batting$teamID)
Batting$lgID <- as.factor(Batting$lgID)


Salaries$yearID <- as.factor(Salaries$yearID)
Salaries$teamID <- as.factor(Salaries$teamID)
Salaries$lgID <- as.factor(Salaries$lgID)
Salaries$playerID <- as.factor(Salaries$playerID)


## ----part1-batting-teamlevels--------------------------------------------
levels(Batting$teamID)


## ----part1-salary-teamlevels---------------------------------------------
levels(Salaries$teamID)


## ----part1-batting-leagues-----------------------------------------------
levels(Batting$lgID)


## ----part1-salary-leagues------------------------------------------------
levels(Salaries$lgID)


## ----part1-averaging-----------------------------------------------------
avgHR <- (Batting %>%
  group_by(yearID) %>%
  summarise(average = mean(HR)))

avgSal <- (Salaries %>%
  group_by(yearID) %>%
  summarise(averageInMillions = mean(salary)/1000000))


## ----part1-average-tableheads--------------------------------------------
head(avgHR)
head(avgSal)


## ----part1-plot1---------------------------------------------------------
ggplot(avgHR, aes(x = yearID, y = average)) + geom_jitter() + theme(axis.text.x = element_text(angle = 60, hjust = 1))


## ----part1-plot2---------------------------------------------------------
ggplot(avgSal, aes(x = yearID, y = averageInMillions)) + geom_jitter() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

