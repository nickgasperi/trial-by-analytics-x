# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(gghighlight)

# load 2025 data
nfldata25 = load_pbp(2025)

# tibble
passdata = nfldata25 %>%
  filter(!is.na(passer_player_id),
         !is.na(yards_gained)) %>%
  group_by(play_id,
           passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# add cumulative passing yds columns to existing tibble
passdata$cumyds = ave(passdata$yds, passdata$passer_player_id,
                      FUN = cumsum)
passdata$cumatt = ave(passdata$att, passdata$passer_player_id,
                      FUN = cumsum)

# check updated tibble for accuracy
passdata %>%
  filter(passer_player_name == "M.Stafford") %>%
  print(n = 15)

# plot data
passplot_a = ggplot(data = passdata,
                    aes(x = play_id,
                        y = cumyds,
                        group = passer_player_id)) +
  geom_line(aes(color = "#003594"),
            linewidth = 1.0) +
  gghighlight(passer_player_id == "00-0026498",
              label_key = passer_player_id,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5),
              use_group_by = FALSE) +
  scale_color_identity() +
  theme(legend.position = "none")

# view plot
passplot_a
  
  
  
