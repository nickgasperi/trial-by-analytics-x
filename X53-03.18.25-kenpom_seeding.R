library(tidyverse)
library(readxl)
library(PerformanceAnalytics)

# convert 2025 NCAA CBB dataframe to tibble
kenpom2 = as_tibble(kenpom1)

# view tibble
kenpom2

# define numeric variables
kenpomNUM = kenpom2 %>%
  filter(!is.na(Seed)) %>%    # include only tournament teams
  select(Rk,
         Seed,
         Wins,
         Losses,
         NetRtg,
         ORtg,
         DRtg,
         AdjT,
         Luck,
         `opp-NetRtg`,
         `opp-ORtg`,
         `opp-DRtg`,
         `noncon-NetRtg`)

# plot correlation chart
corplot1 = chart.Correlation(kenpomNUM,
                             pch = 19,
                             cex.labels = 1.5)

# I manually cropped the final image to include only the first two rows