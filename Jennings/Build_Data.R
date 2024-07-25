## MLB Eye Color Project
### Build Data

## libraries
remotes::install_github("BillPetti/baseballr")
library(baseballr)
library(tidyverse)

## set theme
theme_set(theme_bw())

## qualified hitters from 2021-23
### load FanGraphs hitters
qual_hitters <- baseballr::fg_bat_leaders(startseason = "2021", endseason = "2023")

### glimpse
glimpse(qual_hitters)

### summary of at bats
summary(qual_hitters$AB)

### remove all batters under 10 at-bats
qual_hitters |> 
  filter(AB > 10) |> 
  pull(AB) |> 
  summary()

### histogram of at-bats
qual_hitters |> 
  filter(AB > 10) |> 
  ggplot(aes(AB)) +
  geom_histogram(fill = "dodgerblue4", 
                 binwidth = 50) +
  scale_x_continuous(breaks = seq(0, 700, 100))

### 
qual_hitters <- qual_hitters |> 
  filter(AB > 240.5)
