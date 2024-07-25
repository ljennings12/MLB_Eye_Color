## Ohio State University Sport Analytics Conference
## March 18, 2024

# Libraries
# install.packages("baseballr")
library(baseballr)
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(caret)
library(ROSE)

## Qualified Hitters
q_hitters <- read.csv("qualified_hitters_eyes.csv")

# Unbalanced data set
sum(q_hitters$eye != "Brown") / nrow(q_hitters)

# Look at day and night games
# Look daynight splits vs. eye color
# Make visualizations for both
# Add 'eye' into select statements
  
## Bar plot of eye color
ggplot(q_hitters, aes(eye, y = after_stat(count) / sum(after_stat(count)), fill = eye)) +
  geom_bar(color = "black") +
  labs(title = "Eye Color of Qualified MLB Players",
       x = "Eye Color",
       y = "Percent",
       fill = "Eye Color") +
  scale_fill_manual(values = c("#1b5675", "#542a0e", "#687c8b")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 28, hjust = 0.5, face = "bold"))

## Convert gray to blue since there is such a small % of grey
q_hitters <- q_hitters %>% 
  mutate(eye = ifelse(eye == "Gray", "Blue", eye))


## Bar plot of eye color
ggplot(q_hitters, aes(eye, y = after_stat(count) / sum(after_stat(count)), fill = eye)) +
  geom_bar(color = "black") +
  labs(title = "Eye Color of Qualified MLB Players",
       x = "Eye Color",
       y = "Percent",
       fill = "Eye Color") +
  scale_fill_manual(values = c("#6baed6", "#63390f")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 28, hjust = 0.5, face = "bold"))

## Player IDs
playerID <- read.csv("SFBB Player ID Map - PLAYERIDMAP.csv")


# Filter Player ID Dataframe
fangraphsID <- playerID %>% 
  filter(POS != "P", IDPLAYER != "ohtansh01p") %>% 
  mutate(LASTCOMMAFIRST = recode(LASTCOMMAFIRST, "Abreu, Jose" = "Abreu, José", "Acuna, Ronald" = "Acuña Jr., Ronald",
                                 "Aguilar, Jesus" = "Aguilar, Jesús", "Baez, Javier" = "Báez, Javier",
                                 "Brantley, Michael" = "Brantley Jr., Michael", 
                                 "Chisholm, Jazz" = "Chisholm Jr., Jazz",
                                 "Cruz, Nelson" = "Cruz Jr., Nelson",
                                 "Diaz, Elias" = "Díaz, Elias", "Diaz, Yandy" = "Díaz, Yandy",
                                 "Friedl, TJ" = "Friedl Jr., TJ", "Garcia, Adolis" = "García, Adolis",
                                 "Garcia, Avisail" = "García, Avisaíl", "Gimenez, Andres" = "Giménez, Andrés",
                                 "Gourriel, Lourdes" = "Gurriel Jr., Lourdes", 
                                 "Gurriel, Yulieski" = "Gurriel, Yuli", "Hernandez, Cesar" = "Hernández, César",
                                 "Hernandez, Enrique" = "Hernández, Enrique", "Hernandez, Teoscar" = "Hernández, Teoscar",
                                 "Lowe, Nate" = "Lowe, Nathaniel", "Mancini, Trey" = "Mancini III, Trey",
                                 "Melendez, MJ" = "Melendez Jr., MJ", "Moncada, Yoan" = "Moncada, Yoán",
                                 "Mullins, Cedric" = "Mullins II, Cedric", "Pena, Jeremy" = "Peña, Jeremy",
                                 "Pollock, A.J." = "Pollock, AJ", "Ramirez, Jose" = "Ramírez, José",
                                 "Robert, Luis" = "Robert Jr., Luis", "Rodriguez, Julio" = "Rodríguez, Julio",
                                 "Rooker, Brent" = "Rooker Jr., Brent", "Sano, Miguel" = "Sanó, Miguel",
                                 "Springer, George" = "Springer III, George", 
                                 "Suarez, Eugenio" = "Suárez, Eugenio", "Taylor, Michael" = "Taylor, Michael A.",
                                 "Urias, Luis" = "Urías, Luis", "Urshela, Giovanny" = "Urshela, Gio",
                                 "Voit, Luke" = "Voit III, Luke", "Wade, LaMonte" = "Wade Jr., LaMonte",
                                 "Farmer, Klye" = "Farmer, Kyle", "Castellanos, Nicholas" = "Castellanos, Nick")) %>% 
  filter(LASTCOMMAFIRST %in% q_hitters$last_name..first_name) %>% 
  mutate(last_name..first_name = LASTCOMMAFIRST) %>% 
  select(last_name..first_name, IDFANGRAPHS)

# Break hitters into each year to simplify for loops
q_hitters_21 <- q_hitters %>% 
  filter(year == 2021 & last_name..first_name %in% fangraphsID$last_name..first_name) %>% 
  inner_join(fangraphsID, by = "last_name..first_name", unmatched = "drop")

q_hitters_22 <- q_hitters %>% 
  filter(year == 2022 & last_name..first_name %in% fangraphsID$last_name..first_name) %>% 
  inner_join(fangraphsID, by = "last_name..first_name", unmatched = "drop")

q_hitters_23 <- q_hitters %>% 
  filter(year == 2023 & last_name..first_name %in% fangraphsID$last_name..first_name) %>% 
  inner_join(fangraphsID, by = "last_name..first_name", unmatched = "drop")


# Storage data frames
hitters_gl_2021 <- data.frame()
hitters_gl_2022 <- data.frame()
hitters_gl_2023 <- data.frame()

# 2021
for(i in 1:nrow(q_hitters_21)){
  fg_data <- fg_batter_game_logs(playerid = q_hitters_21$IDFANGRAPHS[i], year = 2021)
  hitters_gl_2021 <- rbind(hitters_gl_2021, fg_data, fill = TRUE)
}
  
# 2022
for(i in 1:nrow(q_hitters_22)){
  fg_data <- fg_batter_game_logs(playerid = q_hitters_22$IDFANGRAPHS[i], year = 2022)
  hitters_gl_2022 <- rbind(hitters_gl_2022, fg_data, fill = TRUE)
}

# 2023
for(i in 1:nrow(q_hitters_23)){
  fg_data <- fg_batter_game_logs(playerid = q_hitters_23$IDFANGRAPHS[i], year = 2023)
  hitters_gl_2023 <- rbind(hitters_gl_2023, fg_data, fill = TRUE)
}

## Still have to:
  # Select preliminary variables
  # Add eye color (look for dataset)
  # Game time (look for dataset)


## Game time
## Storage data frame
## 2021
games_2021 <- data.frame()

dates_2021 <- sort(unique(hitters_gl_2021$Date))

# Split into 2 for loops to make sure web scraping works

for(i in 1:91){
  mlbGame <- mlb_game_pks(date = dates_2021[i]) %>% 
    filter(isTie == FALSE) %>% 
    select(officialDate, dayNight, teams.home.team.name, teams.away.team.name, doubleHeader)
  
  games_2021 <- rbind(games_2021, mlbGame, fill = TRUE)
}

for(i in 92:length(dates_2021)){
  mlbGame <- mlb_game_pks(date = dates_2021[i]) %>% 
    filter(isTie == FALSE) %>% 
    select(officialDate, dayNight, teams.home.team.name, teams.away.team.name, doubleHeader)
  
  games_2021 <- rbind(games_2021, mlbGame, fill = TRUE)
}

## 2022
games_2022 <- data.frame()

dates_2022 <- sort(unique(hitters_gl_2022$Date))

for(i in 1:90){
  mlbGame <- mlb_game_pks(date = dates_2022[i]) %>% 
    filter(isTie == FALSE) %>% 
    select(officialDate, dayNight, teams.home.team.name, teams.away.team.name, doubleHeader)
  
  games_2022 <- rbind(games_2022, mlbGame, fill = TRUE)
}

for(i in 91:length(dates_2022)){
  mlbGame <- mlb_game_pks(date = dates_2022[i]) %>% 
    filter(isTie == FALSE) %>% 
    select(officialDate, dayNight, teams.home.team.name, teams.away.team.name, doubleHeader)
  
  games_2022 <- rbind(games_2022, mlbGame, fill = TRUE)
}

## 2023
games_2023 <- data.frame()

dates_2023 <- sort(unique(hitters_gl_2023$Date))

for(i in 1:91){
  mlbGame <- mlb_game_pks(date = dates_2023[i]) %>% 
    filter(isTie == FALSE) %>% 
    select(officialDate, dayNight, teams.home.team.name, teams.away.team.name, doubleHeader)
  
  games_2023 <- rbind(games_2023, mlbGame, fill = TRUE)
}

for(i in 92:length(dates_2023)){
  mlbGame <- mlb_game_pks(date = dates_2023[i]) %>% 
    filter(isTie == FALSE) %>% 
    select(officialDate, dayNight, teams.home.team.name, teams.away.team.name, doubleHeader)
  
  games_2023 <- rbind(games_2023, mlbGame, fill = TRUE)
}


## Add Time of Day to Hitters Data set

## 2021
# Recode Hitters dataset to fix opponents
hitters_gl_2021 <- hitters_gl_2021 %>% 
  mutate(home_team = ifelse(str_like(Opp, "@%"), str_remove(Opp, "@"), Team ), 
         away_team = ifelse(str_like(Opp, "@%"), Team, str_remove(Opp, "@")))

# Recode games dataset to change opponents to same format as hitters dataset
games_2021 <- games_2021[-14,]

games_2021 <- games_2021 %>% 
  mutate(home_team = recode(teams.home.team.name, 
                                       "New York Yankees" = "NYY", "Boston Red Sox" = "BOS",
                                       "Baltimore Orioles" = "BAL", "Tampa Bay Rays" = "TBR",
                                       "Toronto Blue Jays" = "TOR", "Chicago White Sox" = "CHW",
                                       "Kansas City Royals" = "KCR", "Minnesota Twins" = "MIN", 
                                       "Cleveland Guardians" = "CLE", "Cleveland Indians" = "CLE", 
                                       "Detroit Tigers" = "DET",
                                       "Texas Rangers" = "TEX", "Houston Astros" = "HOU",
                                       "Oakland Athletics" = "OAK", "Seattle Mariners" = "SEA",
                                       "Los Angeles Angels" = "LAA", "Atlanta Braves" = "ATL",
                                       "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSN", 
                                       "Miami Marlins" = "MIA", "New York Mets" = "NYM",
                                       "St. Louis Cardinals" = "STL", "Chicago Cubs" = "CHC",
                                       "Milwaukee Brewers" = "MIL", "Pittsburgh Pirates" = "PIT",
                                       "Cincinnati Reds" = "CIN", "Arizona Diamondbacks" = "ARI",
                                       "San Francisco Giants" = "SFG", "Los Angeles Dodgers" = "LAD",
                                       "San Diego Padres" = "SDP", "Colorado Rockies" = "COL"),
         away_team = recode(teams.away.team.name, 
                                       "New York Yankees" = "NYY", "Boston Red Sox" = "BOS",
                                       "Baltimore Orioles" = "BAL", "Tampa Bay Rays" = "TBR",
                                       "Toronto Blue Jays" = "TOR", "Chicago White Sox" = "CHW",
                                       "Kansas City Royals" = "KCR", "Minnesota Twins" = "MIN", 
                                       "Cleveland Guardians" = "CLE", "Cleveland Indians" = "CLE", 
                                       "Detroit Tigers" = "DET",
                                       "Texas Rangers" = "TEX", "Houston Astros" = "HOU",
                                       "Oakland Athletics" = "OAK", "Seattle Mariners" = "SEA",
                                       "Los Angeles Angels" = "LAA", "Atlanta Braves" = "ATL",
                                       "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSN", 
                                       "Miami Marlins" = "MIA", "New York Mets" = "NYM",
                                       "St. Louis Cardinals" = "STL", "Chicago Cubs" = "CHC",
                                       "Milwaukee Brewers" = "MIL", "Pittsburgh Pirates" = "PIT",
                                       "Cincinnati Reds" = "CIN", "Arizona Diamondbacks" = "ARI",
                                       "San Francisco Giants" = "SFG", "Los Angeles Dodgers" = "LAD",
                                       "San Diego Padres" = "SDP", "Colorado Rockies" = "COL")) %>% 
  select(officialDate, home_team, away_team, dayNight, doubleHeader)


hitters_21_join <- inner_join(hitters_gl_2021, games_2021, copy = TRUE, 
           by = c('home_team', 'away_team', 
                 'Date' = 'officialDate'),
           relationship = "many-to-many")


# Remove duplicates
# Remove doubleheaders for the simplicity of the inner join
hitters_21 <- hitters_21_join %>% 
  filter(doubleHeader == "N") %>% 
  distinct(.keep_all = TRUE)

## Join to get eye color
# Create name variable to match same format as savant
name <- str_split_fixed(hitters_21$PlayerName, " ", n = 2)
last_name_first_name <- paste(as.character(name[(nrow(hitters_21) + 1):(nrow(hitters_21) * 2)]), ",", 
                              as.character(name[1:(nrow(hitters_21))]))
last_name_first_name <- gsub("+ , ", ", ", last_name_first_name)

# Add name variable to dataset
hitters_21_name <- hitters_21 %>% 
  mutate(last_name..first_name = last_name_first_name)

# Change names that are incorrect
hitters_21_eye <- hitters_21_name %>% 
  mutate(last_name..first_name = recode(last_name..first_name, "Abreu, Jose" = "Abreu, José", "Acuna Jr., Ronald" = "Acuña Jr., Ronald",
       "Aguilar, Jesus" = "Aguilar, Jesús", "Baez, Javier" = "Báez, Javier",
       "Brantley, Michael" = "Brantley Jr., Michael", 
       "Chisholm Jr., Jazz" = "Chisholm Jr., Jazz",
       "Cruz, Nelson" = "Cruz Jr., Nelson",
       "Diaz, Elias" = "Díaz, Elias", "Diaz, Yandy" = "Díaz, Yandy",
       "Friedl, T.J." = "Friedl Jr., TJ", "Garcia, Adolis" = "García, Adolis",
       "Garcia, Avisail" = "García, Avisaíl", "Gimenez, Andres" = "Giménez, Andrés",
       "Hernandez, Cesar" = "Hernández, César",
       "Hernandez, Enrique" = "Hernández, Enrique", "Hernandez, Teoscar" = "Hernández, Teoscar",
       "Mancini, Trey" = "Mancini III, Trey",
       "Melendez, MJ" = "Melendez Jr., MJ", "Moncada, Yoan" = "Moncada, Yoán",
       "Mullins, Cedric" = "Mullins II, Cedric", "Pena, Jeremy" = "Peña, Jeremy",
       "Pollock, A.J." = "Pollock, AJ", "Ramirez, Jose" = "Ramírez, José",
       "Robert, Luis" = "Robert Jr., Luis", "Rodriguez, Julio" = "Rodríguez, Julio",
       "Rooker, Brent" = "Rooker Jr., Brent", "Sano, Miguel" = "Sanó, Miguel",
       "Springer, George" = "Springer III, George", 
       "Suarez, Eugenio" = "Suárez, Eugenio", "A. Taylor, Michael" = "Taylor, Michael A.",
       "Urias, Luis" = "Urías, Luis",
       "Voit, Luke" = "Voit III, Luke"))

# Join q_hitters to get qualified hitters with eye color
hitters_21_eye <- hitters_21_eye %>% 
  inner_join(q_hitters_21, 
             by = 'last_name..first_name') %>% 
  select(-c(player_id, year, IDFANGRAPHS))


## Add Time of Day to Hitters Data set

## 2022
# Recode Hitters dataset to fix opponents
hitters_gl_2022 <- hitters_gl_2022 %>% 
  mutate(home_team = ifelse(str_like(Opp, "@%"), str_remove(Opp, "@"), Team ), 
         away_team = ifelse(str_like(Opp, "@%"), Team, str_remove(Opp, "@")))

# Recode games dataset to change opponents to same format as hitters dataset
games_2022 <- games_2022[-8,]

games_2022 <- games_2022 %>% 
  mutate(home_team = recode(teams.home.team.name, 
                            "New York Yankees" = "NYY", "Boston Red Sox" = "BOS",
                            "Baltimore Orioles" = "BAL", "Tampa Bay Rays" = "TBR",
                            "Toronto Blue Jays" = "TOR", "Chicago White Sox" = "CHW",
                            "Kansas City Royals" = "KCR", "Minnesota Twins" = "MIN", 
                            "Cleveland Guardians" = "CLE", "Cleveland Indians" = "CLE", 
                            "Detroit Tigers" = "DET",
                            "Texas Rangers" = "TEX", "Houston Astros" = "HOU",
                            "Oakland Athletics" = "OAK", "Seattle Mariners" = "SEA",
                            "Los Angeles Angels" = "LAA", "Atlanta Braves" = "ATL",
                            "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSN", 
                            "Miami Marlins" = "MIA", "New York Mets" = "NYM",
                            "St. Louis Cardinals" = "STL", "Chicago Cubs" = "CHC",
                            "Milwaukee Brewers" = "MIL", "Pittsburgh Pirates" = "PIT",
                            "Cincinnati Reds" = "CIN", "Arizona Diamondbacks" = "ARI",
                            "San Francisco Giants" = "SFG", "Los Angeles Dodgers" = "LAD",
                            "San Diego Padres" = "SDP", "Colorado Rockies" = "COL"),
         away_team = recode(teams.away.team.name, 
                            "New York Yankees" = "NYY", "Boston Red Sox" = "BOS",
                            "Baltimore Orioles" = "BAL", "Tampa Bay Rays" = "TBR",
                            "Toronto Blue Jays" = "TOR", "Chicago White Sox" = "CHW",
                            "Kansas City Royals" = "KCR", "Minnesota Twins" = "MIN", 
                            "Cleveland Guardians" = "CLE", "Cleveland Indians" = "CLE", 
                            "Detroit Tigers" = "DET",
                            "Texas Rangers" = "TEX", "Houston Astros" = "HOU",
                            "Oakland Athletics" = "OAK", "Seattle Mariners" = "SEA",
                            "Los Angeles Angels" = "LAA", "Atlanta Braves" = "ATL",
                            "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSN", 
                            "Miami Marlins" = "MIA", "New York Mets" = "NYM",
                            "St. Louis Cardinals" = "STL", "Chicago Cubs" = "CHC",
                            "Milwaukee Brewers" = "MIL", "Pittsburgh Pirates" = "PIT",
                            "Cincinnati Reds" = "CIN", "Arizona Diamondbacks" = "ARI",
                            "San Francisco Giants" = "SFG", "Los Angeles Dodgers" = "LAD",
                            "San Diego Padres" = "SDP", "Colorado Rockies" = "COL")) %>% 
  select(officialDate, home_team, away_team, dayNight, doubleHeader)

hitters_22_join <- inner_join(hitters_gl_2022, games_2022, copy = TRUE, 
                              by = c('home_team', 'away_team', 
                                     'Date' = 'officialDate'),
                              relationship = "many-to-many")


# Remove duplicates
# Remove doubleheaders for the simplicity of the inner join
hitters_22 <- hitters_22_join %>% 
  filter(doubleHeader == "N") %>% 
  distinct(.keep_all = TRUE)


## Join to get eye color
# Create name variable to match same format as savant
name <- str_split_fixed(hitters_22$PlayerName, " ", n = 2)
last_name_first_name <- paste(as.character(name[(nrow(hitters_22) + 1):(nrow(hitters_22) * 2)]), ",", 
                              as.character(name[1:(nrow(hitters_22))]))
last_name_first_name <- gsub("+ , ", ", ", last_name_first_name)

# Add name variable to dataset
hitters_22_name <- hitters_22 %>% 
  mutate(last_name..first_name = last_name_first_name)

# Change names that are incorrect
hitters_22_eye <- hitters_22_name %>% 
  mutate(last_name..first_name = recode(last_name..first_name, "Abreu, Jose" = "Abreu, José", "Acuna Jr., Ronald" = "Acuña Jr., Ronald",
                                        "Aguilar, Jesus" = "Aguilar, Jesús", "Baez, Javier" = "Báez, Javier",
                                        "Brantley, Michael" = "Brantley Jr., Michael", 
                                        "Chisholm Jr., Jazz" = "Chisholm Jr., Jazz",
                                        "Cruz, Nelson" = "Cruz Jr., Nelson",
                                        "Diaz, Elias" = "Díaz, Elias", "Diaz, Yandy" = "Díaz, Yandy",
                                        "Friedl, T.J." = "Friedl Jr., TJ", "Garcia, Adolis" = "García, Adolis",
                                        "Garcia, Avisail" = "García, Avisaíl", "Gimenez, Andres" = "Giménez, Andrés",
                                        "Hernandez, Cesar" = "Hernández, César",
                                        "Hernandez, Enrique" = "Hernández, Enrique", "Hernandez, Teoscar" = "Hernández, Teoscar",
                                        "Kim, Ha-seong" = "Kim, Ha-Seong", "Mancini, Trey" = "Mancini III, Trey",
                                        "Melendez, MJ" = "Melendez Jr., MJ", "Moncada, Yoan" = "Moncada, Yoán",
                                        "Mullins, Cedric" = "Mullins II, Cedric", "Pena, Jeremy" = "Peña, Jeremy",
                                        "Pollock, A.J." = "Pollock, AJ", "Ramirez, Jose" = "Ramírez, José",
                                        "Robert, Luis" = "Robert Jr., Luis", "Rodriguez, Julio" = "Rodríguez, Julio",
                                        "Rooker, Brent" = "Rooker Jr., Brent", "Sano, Miguel" = "Sanó, Miguel",
                                        "Springer, George" = "Springer III, George", 
                                        "Suarez, Eugenio" = "Suárez, Eugenio", "A. Taylor, Michael" = "Taylor, Michael A.",
                                        "Urias, Luis" = "Urías, Luis",
                                        "Voit, Luke" = "Voit III, Luke"))

# Join q_hitters to get qualified hitters with eye color
hitters_22_eye <- hitters_22_eye %>% 
  inner_join(q_hitters_22, 
             by = 'last_name..first_name') %>% 
  select(-c(player_id, year, IDFANGRAPHS))


## Add Time of Day to Hitters Data set

## 2023
# Recode Hitters dataset to fix opponents
hitters_gl_2023 <- hitters_gl_2023 %>% 
  mutate(home_team = ifelse(str_like(Opp, "@%"), str_remove(Opp, "@"), Team ), 
         away_team = ifelse(str_like(Opp, "@%"), Team, str_remove(Opp, "@")))

# Recode games dataset to change opponents to same format as hitters dataset
games_2023 <- games_2023[-16,]

games_2023 <- games_2023 %>% 
  mutate(home_team = recode(teams.home.team.name, 
                            "New York Yankees" = "NYY", "Boston Red Sox" = "BOS",
                            "Baltimore Orioles" = "BAL", "Tampa Bay Rays" = "TBR",
                            "Toronto Blue Jays" = "TOR", "Chicago White Sox" = "CHW",
                            "Kansas City Royals" = "KCR", "Minnesota Twins" = "MIN", 
                            "Cleveland Guardians" = "CLE", "Cleveland Indians" = "CLE", 
                            "Detroit Tigers" = "DET",
                            "Texas Rangers" = "TEX", "Houston Astros" = "HOU",
                            "Oakland Athletics" = "OAK", "Seattle Mariners" = "SEA",
                            "Los Angeles Angels" = "LAA", "Atlanta Braves" = "ATL",
                            "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSN", 
                            "Miami Marlins" = "MIA", "New York Mets" = "NYM",
                            "St. Louis Cardinals" = "STL", "Chicago Cubs" = "CHC",
                            "Milwaukee Brewers" = "MIL", "Pittsburgh Pirates" = "PIT",
                            "Cincinnati Reds" = "CIN", "Arizona Diamondbacks" = "ARI",
                            "San Francisco Giants" = "SFG", "Los Angeles Dodgers" = "LAD",
                            "San Diego Padres" = "SDP", "Colorado Rockies" = "COL"),
         away_team = recode(teams.away.team.name, 
                            "New York Yankees" = "NYY", "Boston Red Sox" = "BOS",
                            "Baltimore Orioles" = "BAL", "Tampa Bay Rays" = "TBR",
                            "Toronto Blue Jays" = "TOR", "Chicago White Sox" = "CHW",
                            "Kansas City Royals" = "KCR", "Minnesota Twins" = "MIN", 
                            "Cleveland Guardians" = "CLE", "Cleveland Indians" = "CLE", 
                            "Detroit Tigers" = "DET",
                            "Texas Rangers" = "TEX", "Houston Astros" = "HOU",
                            "Oakland Athletics" = "OAK", "Seattle Mariners" = "SEA",
                            "Los Angeles Angels" = "LAA", "Atlanta Braves" = "ATL",
                            "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSN", 
                            "Miami Marlins" = "MIA", "New York Mets" = "NYM",
                            "St. Louis Cardinals" = "STL", "Chicago Cubs" = "CHC",
                            "Milwaukee Brewers" = "MIL", "Pittsburgh Pirates" = "PIT",
                            "Cincinnati Reds" = "CIN", "Arizona Diamondbacks" = "ARI",
                            "San Francisco Giants" = "SFG", "Los Angeles Dodgers" = "LAD",
                            "San Diego Padres" = "SDP", "Colorado Rockies" = "COL")) %>% 
  select(officialDate, home_team, away_team, dayNight, doubleHeader)

hitters_23_join <- inner_join(hitters_gl_2023, games_2023, copy = TRUE, 
                              by = c('home_team', 'away_team', 
                                     'Date' = 'officialDate'),
                              relationship = "many-to-many")


# Remove duplicates
# Remove doubleheaders for the simplicity of the inner join
hitters_23 <- hitters_23_join %>% 
  filter(doubleHeader == "N") %>% 
  distinct(.keep_all = TRUE)


## Join to get eye color
# Create name variable to match same format as savant
name <- str_split_fixed(hitters_23$PlayerName, " ", n = 2)
last_name_first_name <- paste(as.character(name[(nrow(hitters_23) + 1):(nrow(hitters_23) * 2)]), ",", 
                              as.character(name[1:(nrow(hitters_23))]))
last_name_first_name <- gsub("+ , ", ", ", last_name_first_name)

# Add name variable to dataset
hitters_23_name <- hitters_23 %>% 
  mutate(last_name..first_name = last_name_first_name)

# Change names that are incorrect
hitters_23_eye <- hitters_23_name %>% 
  mutate(last_name..first_name = recode(last_name..first_name, "Abreu, Jose" = "Abreu, José", "Acuna Jr., Ronald" = "Acuña Jr., Ronald",
                                        "Aguilar, Jesus" = "Aguilar, Jesús", "Baez, Javier" = "Báez, Javier",
                                        "Brantley, Michael" = "Brantley Jr., Michael", 
                                        "Chisholm Jr., Jazz" = "Chisholm Jr., Jazz",
                                        "Cruz, Nelson" = "Cruz Jr., Nelson",
                                        "Diaz, Elias" = "Díaz, Elias", "Diaz, Yandy" = "Díaz, Yandy",
                                        "Friedl, T.J." = "Friedl Jr., TJ", "Garcia, Adolis" = "García, Adolis",
                                        "Garcia, Avisail" = "García, Avisaíl", "Gimenez, Andres" = "Giménez, Andrés",
                                        "Hernandez, Cesar" = "Hernández, César",
                                        "Hernandez, Enrique" = "Hernández, Enrique", "Hernandez, Teoscar" = "Hernández, Teoscar",
                                        "Kim, Ha-seong" = "Kim, Ha-Seong", "Mancini, Trey" = "Mancini III, Trey",
                                        "Melendez, MJ" = "Melendez Jr., MJ", "Moncada, Yoan" = "Moncada, Yoán",
                                        "Mullins, Cedric" = "Mullins II, Cedric", "Pena, Jeremy" = "Peña, Jeremy",
                                        "Pollock, A.J." = "Pollock, AJ", "Ramirez, Jose" = "Ramírez, José",
                                        "Robert, Luis" = "Robert Jr., Luis", "Rodriguez, Julio" = "Rodríguez, Julio",
                                        "Rooker, Brent" = "Rooker Jr., Brent", "Sano, Miguel" = "Sanó, Miguel",
                                        "Springer, George" = "Springer III, George", 
                                        "Suarez, Eugenio" = "Suárez, Eugenio", "A. Taylor, Michael" = "Taylor, Michael A.",
                                        "Urias, Luis" = "Urías, Luis",
                                        "Voit, Luke" = "Voit III, Luke"))

# Join q_hitters to get qualified hitters with eye color
hitters_23_eye <- hitters_23_eye %>% 
  inner_join(q_hitters_23, 
             by = 'last_name..first_name') %>% 
  select(-c(player_id, year, IDFANGRAPHS))


## Make sure dataframes have same number of variables
# 2021
hitters_2021 <- hitters_21_eye %>% 
  select(last_name..first_name, eye, dayNight, `BB%`, 'K%', OPS, ISO, BABIP, 'LD%', 'GB%', 
         'FB%',`HR/FB`, wOBA, 'wRC+', WPA, RE24, 'WPA/LI',
         'O-Swing%', 'Z-Swing%', 'Swing%', 'O-Contact%', 'Z-Contact%', 'Contact%', 'Zone%',
         'SwStr%', 'Soft%',	'Med%',	'Hard%', EV, LA, 'Barrel%',	'HardHit%')

# 2022
hitters_2022 <- hitters_22_eye %>% 
  select(last_name..first_name, eye, dayNight, `BB%`, 'K%', OPS, ISO, BABIP, 'LD%', 'GB%', 
         'FB%',`HR/FB`, wOBA, 'wRC+', WPA, RE24, 'WPA/LI',
         'O-Swing%', 'Z-Swing%', 'Swing%', 'O-Contact%', 'Z-Contact%', 'Contact%', 'Zone%',
         'SwStr%', 'Soft%',	'Med%',	'Hard%', EV, LA, 'Barrel%',	'HardHit%')


# 2023
hitters_2023 <- hitters_23_eye %>% 
  select(last_name..first_name, eye, dayNight, `BB%`, 'K%', OPS, ISO, BABIP, 'LD%', 'GB%', 
         'FB%',`HR/FB`, wOBA, 'wRC+', WPA, RE24, 'WPA/LI',
         'O-Swing%', 'Z-Swing%', 'Swing%', 'O-Contact%', 'Z-Contact%', 'Contact%', 'Zone%',
         'SwStr%', 'Soft%',	'Med%',	'Hard%', EV, LA, 'Barrel%',	'HardHit%')


## Row bind dataframes
hitters_eye_color <- rbind(hitters_2021, hitters_2022, hitters_2023)


## Aggregate by name and dayNight
hitters_eye_color_group <- hitters_eye_color %>% 
  group_by(last_name..first_name, dayNight, eye) %>% 
  summarize(BB_rate = mean(`BB%`, na.rm = TRUE), 
            SO_rate = mean(`K%`, na.rm = TRUE), 
            OPS = mean(OPS, na.rm = TRUE), 
            ISO = mean(ISO, na.rm = TRUE), 
            BABIP = mean(BABIP, na.rm = TRUE), 
            LD_pct = mean(`LD%`, na.rm = TRUE), 
            GB_pct = mean(`GB%`, na.rm = TRUE), 
            FB_pct = mean(`FB%`, na.rm = TRUE),
            HR_to_FB = mean(`HR/FB`, na.rm = TRUE), 
            wOBA = mean(wOBA, na.rm = TRUE), 
            wRC_plus = mean(`wRC+`, na.rm = TRUE), 
            WPA = mean(WPA, na.rm = TRUE), 
            RE24 = mean(RE24, na.rm = TRUE), 
            WPA_lev = mean(`WPA/LI`, na.rm = TRUE),
            O_Swing_pct = mean(`O-Swing%`, na.rm = TRUE), 
            Z_Swing_pct = mean(`Z-Swing%`, na.rm = TRUE), 
            Swing_pct = mean(`Swing%`, na.rm = TRUE), 
            O_Contact_pct = mean(`O-Contact%`, na.rm = TRUE), 
            Z_Contact_pct = mean(`Z-Contact%`, na.rm = TRUE), 
            Contact_pct = mean(`Contact%`, na.rm = TRUE), 
            Zone_pct = mean(`Zone%`, na.rm = TRUE),
            SwStr_pct = mean(`SwStr%`, na.rm = TRUE), 
            Soft_pct = mean(`Soft%`, na.rm = TRUE),	
            Med_pct = mean(`Med%`, na.rm = TRUE),	
            Hard_pct = mean(`Hard%`, na.rm = TRUE), 
            EV = mean(EV, na.rm = TRUE), 
            LA = mean(LA, na.rm = TRUE), 
            Barrel_pct = mean(`Barrel%`, na.rm = TRUE),	
            HardHit_pct = mean(`HardHit%`, na.rm = TRUE))




#### Random forests ####

### Change eye color to factor
hitters_eye_color_fact <- hitters_eye_color_group %>% 
  mutate(eye = factor(eye, levels = c("Blue", "Brown")))

### Filter by Day or Night
## Day
day_eye <- hitters_eye_color_fact %>% 
  filter(dayNight == "day") %>% 
  ungroup() %>% 
  select(-c(last_name..first_name, dayNight))

# proportion of eye color
table(day_eye$eye)

## Night
night_eye <- hitters_eye_color_fact %>% 
  filter(dayNight == "night") %>% 
  ungroup() %>%
  select(-c(1:2))


### Day Random Forests First
## 1) Split into test and training data
set.seed(2903)
s <- sample(c(1:nrow(day_eye)), .7*nrow(day_eye), replace = FALSE)
train <- day_eye[s,]
test <- day_eye[-s,]

## 2) Fit on training model
set.seed(2903)
m <- sqrt(ncol(train) - 1)
rf_day_eye <- randomForest(eye ~ ., data = train, mtry = m,
                          ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_day_eye, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")


## Variable Importance
options(scipen = 999)

round(importance(rf_day_eye), 3)



### Night Random Forest
## 1) Split into test and training data
set.seed(2903)
s <- sample(c(1:nrow(night_eye)), .7*nrow(night_eye), replace = FALSE)
train <- night_eye[s,]
test <- night_eye[-s,]

## 2) Fit on training model
set.seed(2903)
m <- sqrt(ncol(train) - 1)
rf_night_eye <- randomForest(eye ~ ., data = train, mtry = m,
                       ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_night_eye, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")


## Variable Importance
options(scipen = 999)

round(importance(rf_night_eye), 3)



######################## TRY AGAIN WITH ROSE #########################

### Change eye color to factor
hitters_eye_color_fact <- hitters_eye_color_group %>% 
  mutate(eye = factor(eye, levels = c("Blue", "Brown")))

### Filter by Day or Night
## Day
day_eye <- hitters_eye_color_fact %>% 
  filter(dayNight == "day") %>% 
  ungroup() %>% 
  select(-c(last_name..first_name, dayNight))

# proportion of eye color
table(day_eye$eye)

## Night
night_eye <- hitters_eye_color_fact %>% 
  filter(dayNight == "night") %>% 
  ungroup() %>%
  select(-c(1:2))


### Day Random Forests First
## 1, a) Use ROSE to synthetically create rows to balance data set
# ROSE synthetically generating data
day_eye_data <- ROSE(eye ~ ., data = day_eye, seed = 2961)$data

table(day_eye_data$eye)

## 1, b) Split into test and training data
set.seed(2961)
s <- sample(c(1:nrow(day_eye_data)), .7*nrow(day_eye_data), replace = FALSE)
train <- day_eye_data[s,]
test <- day_eye_data[-s,]


## 2) Fit on training model
set.seed(2961)
m <- sqrt(ncol(train) - 1)
rf_rose_day <- randomForest(eye ~ ., data = train, mtry = m,
                           ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_rose_day, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")

## Variable Importance
round(importance(rf_rose_day), 3)

## 5) Plot tree
library(rpart.plot)
set.seed(0829)
tree_plot <- rpart(eye ~ ., day_eye_data, method = "class", 
                   control = rpart.control(
                     xval = 10
                   ))

printcp(tree_plot)
plotcp(tree_plot)

tree_plot <- prune(tree_plot, cp = 0.051)

rpart.plot(tree_plot, box.palette = "BnBu")

## MAY ONLY PUT IN SIGNIFICANT VARIABLES TO DECISION



### Night Random Forest
## 1, a) Use ROSE to synthetically create rows to balance data set
# ROSE synthetically generating data
night_eye_data <- ROSE(eye ~ ., data = night_eye, seed = 2961)$data

table(night_eye_data$eye)

## 1, b) Split into test and training data
set.seed(2961)
s <- sample(c(1:nrow(night_eye_data)), .7*nrow(night_eye_data), replace = FALSE)
train <- night_eye_data[s,]
test <- night_eye_data[-s,]

## 2) Fit on training model
set.seed(2961)
m <- sqrt(ncol(train) - 1)
rf_night_eye <- randomForest(eye ~ ., data = train, mtry = m,
                             ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_night_eye, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")


## Variable Importance
options(scipen = 999)

round(importance(rf_night_eye), 3)


## 4) Fit to full model
set.seed(2961)
m <- sqrt(ncol(train) - 1)
rf_night_eye <- randomForest(eye ~ ., data = night_eye_data, mtry = m,
                             ntree = 1000, importance = TRUE)


## 5) Plot tree
set.seed(2961)
tree_plot <- rpart(eye ~ ., night_eye_data, method = "class", 
                   control = rpart.control(
                     xval = 10
                   ))

printcp(tree_plot)
plotcp(tree_plot)

tree_plot <- prune(tree_plot, cp = 0.080)

rpart.plot(tree_plot)


############### TRAIN ###################
## 1, a) Use ROSE to synthetically create rows to balance data set
# ROSE synthetically generating data
day_eye_data <- ROSE(eye ~ ., data = day_eye, seed = 2961)$data

table(day_eye_data$eye)

## 1, b) Split into test and training data
set.seed(2961)
s <- sample(c(1:nrow(day_eye_data)), .7*nrow(day_eye_data), replace = FALSE)
train <- day_eye_data[s,]
test <- day_eye_data[-s,]


## 2) Fit on training model
set.seed(2961)
model <- train(
  eye ~ .,
  tuneLength = 3,
  data = train, 
  method = "ranger",
  num.trees = 1000,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE
  ),
  tuneGrid = data.frame(
    .mtry = c(4, 5, 6, 8),
    .splitrule = "gini",
    .min.node.size = 2)
)



## Confusion Matrix
pred_values <- predict(model, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")

## Variable Importance
round(importance(rf_rose_eye), 3)



### Night Random Forest
## 1, a) Use ROSE to synthetically create rows to balance data set
# ROSE synthetically generating data
night_eye_data <- ROSE(eye ~ ., data = night_eye, seed = 2961)$data

table(night_eye_data$eye)

## 1, b) Split into test and training data
set.seed(2961)
s <- sample(c(1:nrow(night_eye_data)), .7*nrow(night_eye_data), replace = FALSE)
train <- night_eye_data[s,]
test <- night_eye_data[-s,]

## 2) Fit on training model
## 2) Fit on training model
set.seed(2961)
model <- train(
  eye ~ .,
  tuneLength = 3,
  data = train, 
  method = "ranger",
  num.trees = 1000,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE
  ),
  tuneGrid = data.frame(
    .mtry = c(4, 5, 6, 8),
    .splitrule = "gini",
    .min.node.size = 2)
)


## Confusion Matrix
pred_values <- predict(model, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")




###### AFTER ROSE ###########
### Perform variable selection
### Remove variables that aren't significant for day and night
### Run random forests again (change mtry to sqrt(ncol(data) - 1))

######################## TRY AGAIN WITH ROSE #########################

### Day Random Forests First
## 1, a) Use ROSE to synthetically create rows to balance data set
# Remove insignificant variables
day_eye_data <- day_eye %>% 
  select(eye, LD_pct, EV, LA, wOBA, Zone_pct, O_Swing_pct)

# ROSE synthetically generating data
day_eye_data <- ROSE(eye ~ ., data = day_eye_data, seed = 2903)$data

table(day_eye_data$eye)

## 1, b) Split into test and training data
set.seed(2903)
s <- sample(c(1:nrow(day_eye_data)), .7*nrow(day_eye_data), replace = FALSE)
train <- day_eye_data[s,]
test <- day_eye_data[-s,]


## 2) Fit on training model
set.seed(2903)
m <- sqrt(ncol(train) - 1)
rf_rose_day_cond <- randomForest(eye ~ ., data = train, mtry = m,
                            ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_rose_day_cond, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")

## Variable Importance
round(importance(rf_rose_day_cond), 3)



### Night Random Forest
## 1, a) Use ROSE to synthetically create rows to balance data set
# ROSE synthetically generating data
night_eye_data <- ROSE(eye ~ ., data = night_eye, seed = 2903)$data

table(night_eye_data$eye)

## 1, b) Split into test and training data
set.seed(2903)
s <- sample(c(1:nrow(night_eye_data)), .7*nrow(night_eye_data), replace = FALSE)
train <- night_eye_data[s,]
test <- night_eye_data[-s,]

## 2) Fit on training model
set.seed(2903)
m <- sqrt(ncol(train) - 1)
rf_rose_night <- randomForest(eye ~ ., data = train, mtry = m,
                             ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_night_eye, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye, positive = "Blue")


## Variable Importance
options(scipen = 999)

round(importance(rf_night_eye), 3)





### Things to do next
  ## 1) Simulate blue eyes to balance out our classifications
  ## 2) Turn dayNight and eye into one variable for 4 classes
    ## > FAIL
  ## 3) Use train function and cross validation (train() and train_control() in dataCamp)











### Try Option 2
#### 2) Turn dayNight and eye into one variable for 4 classes
### Change eye color to factor
hitters_eye_color_fact <- hitters_eye_color_group %>% 
  mutate(eye_dayNight = paste0(eye, "_", dayNight),
         eye_dayNight = factor(eye_dayNight)) %>% 
  ungroup() %>% 
  select(-c(c(1:3)))



## 1) Split into test and training data
set.seed(2903)
s <- sample(c(1:nrow(hitters_eye_color_fact)), .7*nrow(hitters_eye_color_fact), replace = FALSE)
train <- hitters_eye_color_fact[s,]
test <- hitters_eye_color_fact[-s,]

## 2) Fit on training model
set.seed(2903)
m <- sqrt(ncol(train))
rf_fact_eye <- randomForest(eye_dayNight ~ ., data = train, mtry = m,
                           ntree = 1000, importance = TRUE)


## Confusion Matrix
pred_values <- predict(rf_fact_eye, newdata = data.frame(test))

confusionMatrix(pred_values, test$eye_dayNight)











############################## DRAFT CODE ##############################



### Best Subset Selection

## Make dataframe without numbers
hitters_nums <- hitters_eye_color_group %>% 
  ungroup() %>% 
  select(-c(1:3), `wRC+`)

cor(hitters_nums)

## First, we write a function to predict values for 'regsubsets' models
# predict() function not work with 'regsubsets' output
predict.regsubsets <- function (object ,newdata ,id ,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object ,id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}


## CV with 10 folds 

# number of folds
k <- 10  

# number of predictors
p <- 27  

# set seed
set.seed(456)

# create folds function
CV_folds <- createFolds(hitters_nums$wOBA, k = k)  
k_fold_mse <- matrix(0, nrow = k, ncol = p) # storage for k x p MSE_ij
for(i in 1:k){
  folds <- CV_folds[[i]]
  valid <- hitters_nums[folds,]
  train <- hitters_nums[-folds,]
  best.fit <- regsubsets(wOBA ~ ., data = train, method = "exhaustive", nvmax = p, really.big = TRUE)
  for(j in 1:p){
    # predicted val's for model j
    pred <- predict.regsubsets(best.fit, valid, id = j) 
    # validation MSE_ij
    k_fold_mse[i,j] <- mean((valid$wOBA - pred)^2)  
  }
}

k_fold_mse

m <- colMeans(k_fold_mse)  # mean MSE = CV_j over all k folds for each model(j=1,2,3)
m

se <- apply(k_fold_mse, 2, sd)/sqrt(k)  # stand error for each mean MSE
se


# one-standard error rule plot
library(plotrix)
v <- c(1:p) # number of models
upper <- m + se  # mean MSE plus 1 se
lower <- m - se  # mean MSE minus 1 se
# plot mean MSE's plus/minus 1 se
plotCI(v, m, ui=upper, li = lower,
       ylab = "k-fold CV MSE", xlab = "model")
lines(m, col = "red")  # connect CV_j points


# lowest mse value
which.min(m)

v <- c(4:12)
# plot mean MSE's plus/minus 1 se
plotCI(v, m[4:12], ui= upper[4:12], li = lower[4:12],
       ylab = "k-fold CV MSE", xlab = "model")
lines(m, col = "red")  # connect CV_j points
abline(h =m[5], col = "blue")



# Best 6-variable model
best.mod <- regsubsets(wOBA ~ ., data = hitters_nums, method = "exhaustive", nvmax = p, really.big = TRUE)
reg_summary <- summary(best.mod)
reg_summary


## Other options: consider LASSO or random forests






## Draft code
str_extract(q_hitters$last_name..first_name, '[A-Za-z]+')
str_extract(q_hitters$last_name..first_name, '\\w+')
str_extract(q_hitters$last_name..first_name, '\\z')


sub('^.* ([[:alnum:]]+)$', '\\1', q_hitters$last_name..first_name)

shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
word(q_hitters$last_name..first_name,-1)

?str_extract

fangraphsID <- playerID %>% 
  mutate(LASTNAME = )
filter(LASTCOMMAFIRST %in% q_hitters$last_name..first_name) %>% 
  select(LASTNAME, FIRSTNAME, IDFANGRAPHS) %>% 
  mutate(player_name = paste0(LASTNAME, ", ", FIRSTNAME)) %>% 
  filter(player_name %in% q_hitters$last_name..first_name)


q <- unique(q_hitters$last_name..first_name)
write.csv(q, "unique_names.csv")


# 2021
for(i in 1:2){
  fg_id <- playerid_lookup(last_name = str_extract(q_hitters$last_name..first_name[i], '[A-Za-z]+'), 
                           first_name = word(q_hitters$last_name..first_name[i], -1))$fangraphs_id
  fg_data <- fg_batter_game_logs(playerid = fg_id, year = 2021)
  hitters_gl_2021 <- rbind(hitters_gl_2021, fg_data)
}





fg_id <- playerid_lookup(last_name = str_extract(q_hitters$last_name..first_name[2], '[A-Za-z]+'), 
                         first_name = word(q_hitters$last_name..first_name[2], -1))
fg_id <- na.omit(fg_id)
fg_data <- fg_batter_game_logs(playerid = fg_id, year = 2021)
hitters_gl_2021 <- rbind(hitters_gl_2021, fg_data)

?playerid_lookup()



mlb_pbp(game_pk = 719263)

dateeee <- mlb_game_pks(date = "2021-04-01") %>%
  filter(isTie == FALSE) %>% 
  select(officialDate, dayNight, teams.home.team.name, teams.away.team.name)

dateeee$teams.home.team.name

# Has dayNight variable
mlb_game_pks(date = "2022-06-19")

# Look at IsTIE equal to False
# May just leave rain delays in there


# Remove duplicates
hits1_21 <- hits_21[!duplicated(hits_21),]

miggy <- hits1_21[c(60:64),]

duplicated(miggy[,1:276])

hits2_21 <- hits_21 %>% 
  filter(doubleHeader == "N") %>% 
  distinct(.keep_all = TRUE)

hits5_21 <- hits2_21[!duplicated(hits2_21),]

sum(hits2_21$dayNight == "day") / nrow(hits2_21)

hits3_21 <- distinct(hits_21, .keep_all = TRUE)

sum(hits3_21$dayNight == "day") / nrow(hits3_21)

hits4_21 <- distinct(hitters_gl_2021, .keep_all = TRUE)


unique(hitters_21_eye_1$last_name..first_name)

unique(hitters_21_eye$last_name..first_name)