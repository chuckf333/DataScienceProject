## ----results='hide', message=FALSE, warning=FALSE, include=FALSE---------
suppressMessages(install.packages("tidyverse", repos="http://cran.us.r-project.org"))
suppressMessages(c(library("tidyverse"), library("dplyr"), library("ggplot2")))


## ----results='hide', message=FALSE, warning=FALSE------------------------
suppressMessages(Salaries <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/Salaries.csv"))
suppressMessages(Batting <- read_csv("https://raw.github.com/chadwickbureau/baseballdatabank/master/core/Batting.csv"))


## ----results='asis'------------------------------------------------------
head(Salaries)
head(Batting)


## ------------------------------------------------------------------------
Batting$yearID <- as.factor(Batting$yearID)
Batting$playerID <- as.factor(Batting$playerID)
Batting$teamID <- as.factor(Batting$teamID)
Batting$lgID <- as.factor(Batting$lgID)


Salaries$yearID <- as.factor(Salaries$yearID)
Salaries$teamID <- as.factor(Salaries$teamID)
Salaries$lgID <- as.factor(Salaries$lgID)
Salaries$playerID <- as.factor(Salaries$playerID)


## ------------------------------------------------------------------------
levels(Batting$yearID)


## ------------------------------------------------------------------------
levels(Salaries$yearID)


## ------------------------------------------------------------------------
levels(Batting$teamID)


## ------------------------------------------------------------------------
levels(Salaries$teamID)


## ------------------------------------------------------------------------
levels(Batting$lgID)


## ------------------------------------------------------------------------
levels(Salaries$lgID)


## ------------------------------------------------------------------------
avgHR <- (Batting %>%
  group_by(yearID) %>%
  summarise(average = mean(HR)))

avgSal <- (Salaries %>%
  group_by(yearID) %>%
  summarise(averageInMillions = mean(salary)/1000000))


## ------------------------------------------------------------------------
head(avgHR)
head(avgSal)


## ------------------------------------------------------------------------
ggplot(avgHR, aes(x = yearID, y = average)) + geom_jitter() + theme(axis.text.x = element_text(angle = 60, hjust = 1))


## ------------------------------------------------------------------------
ggplot(avgSal, aes(x = yearID, y = averageInMillions)) + geom_jitter() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

