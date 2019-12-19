## ----part2-packages, message=FALSE, warning=FALSE, results='hide'--------
include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}
include("knitr")
purl("p01.rmd", output = "part1.r")
source("part1.r")

include("rvest")


## ----part2-function------------------------------------------------------
read_stats <- function(statyear){
  
  url <- paste("https://www.baseball-reference.com/leagues/MLB/", statyear, sep = "")
  url <- paste(url, ".shtml", sep = "")
  
  statspage <- read_html(url)
  
  statslist  <- statspage %>% html_nodes("#teams_standard_batting")
  
  team_ID <- statslist %>%
                  html_nodes("th[data-stat='team_ID']") %>%
                  html_text() %>%
                  as.factor()
  team_ID <- team_ID[-1]
  team_ID <- team_ID[-length(team_ID)]
  team_ID <- team_ID[-length(team_ID)]
  
  batters_used <- statslist %>%
                  html_nodes("td[data-stat='batters_used']") %>%
                  html_text() %>%
                  as.integer()
  
  batters_used <- batters_used[-length(batters_used)]
  
  age_bat <- statslist %>%
                  html_nodes("td[data-stat='age_bat']") %>%
                  html_text() %>%
                  as.integer()
  
  age_bat <- age_bat[-length(age_bat)]
  
  runs_per_game <- statslist %>%
                  html_nodes("td[data-stat='runs_per_game']") %>%
                  html_text() %>%
                  as.integer()
  
  runs_per_game <- runs_per_game[-length(runs_per_game)]

  G <- statslist %>%
                  html_nodes("td[data-stat='G']") %>%
                  html_text() %>%
                  as.integer()
  
  G <- G[-length(G)]
  
  PA <- statslist %>%
                  html_nodes("td[data-stat='PA']") %>%
                  html_text() %>%
                  as.integer()
  
  PA <- PA[-length(PA)]
  
  AB <- statslist %>%
                  html_nodes("td[data-stat='AB']") %>%
                  html_text() %>%
                  as.integer()
  
  AB <- AB[-length(AB)]
  
  R <- statslist %>%
                  html_nodes("td[data-stat='R']") %>%
                  html_text() %>%
                  as.integer()
  
  R <- R[-length(R)]
  
  H <- statslist %>%
                  html_nodes("td[data-stat='H']") %>%
                  html_text() %>%
                  as.integer()
  
  H <- H[-length(H)]
  
  B2 <- statslist %>%
                  html_nodes("td[data-stat='2B']") %>%
                  html_text() %>%
                  as.integer()
  
  B2 <- B2[-length(B2)]
  
  B3 <- statslist %>%
                  html_nodes("td[data-stat='3B']") %>%
                  html_text() %>%
                  as.integer()
  
  B3 <- B3[-length(B3)]
  
  HR <- statslist %>%
                  html_nodes("td[data-stat='HR']") %>%
                  html_text() %>%
                  as.integer()
  
  HR <- HR[-length(HR)]
  
  RBI <- statslist %>%
                  html_nodes("td[data-stat='RBI']") %>%
                  html_text() %>%
                  as.integer()
  RBI <- RBI[-length(RBI)]
  
  SB <- statslist %>%
                  html_nodes("td[data-stat='SB']") %>%
                  html_text() %>%
                  as.integer()
  
  SB <- SB[-length(SB)]
  
  CS <- statslist %>%
                  html_nodes("td[data-stat='CS']") %>%
                  html_text() %>%
                  as.integer()
  
  CS <- CS[-length(CS)]
  
  BB <- statslist %>%
                  html_nodes("td[data-stat='BB']") %>%
                  html_text() %>%
                  as.integer()
  
  BB <- BB[-length(BB)]
  
  SO <- statslist %>%
                  html_nodes("td[data-stat='SO']") %>%
                  html_text() %>%
                  as.integer()
  
  SO <- SO[-length(SO)]
  
  TB <- statslist %>%
                  html_nodes("td[data-stat='TB']") %>%
                  html_text() %>%
                  as.integer()
  
  TB <- TB[-length(TB)]
  
  GIDP <- statslist %>%
                  html_nodes("td[data-stat='GIDP']") %>%
                  html_text() %>%
                  as.integer()
  
  GIDP <- GIDP[-length(GIDP)]
  
  HBP <- statslist %>%
                  html_nodes("td[data-stat='HBP']") %>%
                  html_text() %>%
                  as.integer()
  
  HBP <- HBP[-length(HBP)]
  
  SH <- statslist %>%
                  html_nodes("td[data-stat='SH']") %>%
                  html_text() %>%
                  as.integer()
  
  SH <- SH[-length(SH)]
  
  SF <- statslist %>%
                  html_nodes("td[data-stat='SF']") %>%
                  html_text() %>%
                  as.integer()
  
  SF <- SF[-length(SF)]
  
  IBB <- statslist %>%
                  html_nodes("td[data-stat='IBB']") %>%
                  html_text() %>%
                  as.integer()
  
  IBB <- IBB[-length(IBB)]
  
  LOB <- statslist %>%
                  html_nodes("td[data-stat='LOB']") %>%
                  html_text() %>%
                  as.integer()
  
  LOB <- LOB[-length(LOB)]
  
  year <- c(1:length(team_ID))
  year[1:length(year)] = statyear
  year <- as.factor(year)
  
  return(tibble(Year = year, Team_ID = team_ID, Batters_Used = batters_used, Batter_Avg_Age = age_bat, Runs_Per_Game = runs_per_game, G = G, PA = PA, AB = AB, R = R, H = H, Double = B2, Triple = B3, HR = HR, RBI = RBI, SB = SB, CS= CS, BB = BB, SO = SO , TB = TB, GIDP = GIDP, HBP = HBP, SH = SH, SF = SF, IBB = IBB, LOB = LOB))
}


## ----part2-scraping------------------------------------------------------
ScrapedBatting <- read_stats("1985")
chunk <- read_stats("1986")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1987")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1988")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1989")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1990")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1991")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1992")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1993")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1994")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1995")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1996")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1997")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1998")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("1999")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2000")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2001")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2002")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2003")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2004")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2005")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2006")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2007")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2008")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2009")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2010")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2011")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2012")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2013")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2014")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2015")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2016")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2017")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2018")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))
chunk <- read_stats("2019")
ScrapedBatting <- rbind(as.data.frame(ScrapedBatting), as.data.frame(chunk))

head(ScrapedBatting)


## ----part2-newtable, warning=FALSE---------------------------------------

avgSalByTeam <- (Salaries %>%
  group_by(yearID, teamID) %>%
    summarise(averageInMillions = mean(salary)/1000000))
colnames(avgSalByTeam)[colnames(avgSalByTeam)=="yearID"] <- "Year"
colnames(avgSalByTeam)[colnames(avgSalByTeam)=="teamID"] <- "Team_ID"

newTable <- ScrapedBatting %>%
  inner_join(avgSalByTeam, by = c("Team_ID", "Year"))

head(newTable)


## ----part2-model1--------------------------------------------------------
modelcheck <- lm(data=newTable, formula=averageInMillions~Year+Runs_Per_Game+H+R+Double+Triple+HR)
summary(modelcheck)


## ----part2-model2--------------------------------------------------------
modelcheck <- lm(data=newTable, formula=averageInMillions~Runs_Per_Game+H+R+Double+Triple+HR)
summary(modelcheck)


## ----part2-model3--------------------------------------------------------
modelcheck <- lm(data=newTable, formula=averageInMillions~H+R+Double+Triple+HR)
summary(modelcheck)


## ----part2-scatplots-----------------------------------------------------

qplot(averageInMillions , HR, data = newTable, xlab = "Home runs by team", ylab = "Average salary (in millions)") + stat_smooth(method = "lm")

qplot(averageInMillions , Double, data = newTable, xlab = "Doubles hit by team", ylab = "Average salary (in millions)") + stat_smooth(method = "lm")

