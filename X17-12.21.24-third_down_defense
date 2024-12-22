# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)

# load data
  nfldata = load_pbp(2024)

# filter data
  defshort = nfldata %>%
    filter(week < 16,
           play_type == "pass" | play_type == "run",
           !is.na(defteam),
           !is.na(epa),
           !is.na(success),
           down == 3,
           ydstogo <= 3,
           ) %>%
    group_by(defteam) %>%
    summarize(plays = n(),
              sucrate = sum(success)/sum(plays),
              epa = sum(epa)/sum(plays)) %>%
    arrange(-sucrate) %>%
    print(n = Inf)

# plot the data
  shortplot = ggplot(data = defshort, aes(x = epa, y = sucrate)) +
    geom_hline(yintercept = mean(defshort$sucrate), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(defshort$epa), linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_nfl_logos(aes(team_abbr = defteam), width = 0.06, alpha = 0.8) +
    scale_y_reverse() +
    scale_x_reverse() +
    labs(title = "NFL Defenses - EPA Per Play vs. Success Rate Allowed",
         subtitle = "2024 Wk 1-15 | 3rd Down & 5 or Less to Go",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "EPA", y = "Success Rate") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.caption = element_text(size = 13),
          axis.title = element_text(face = "bold.italic", size = 15),
          axis.text = element_text(size = 15))

# view the plot
  shortplot

# save the plot
  ggsave("X post 17.1 - def_shortyd3rd.png",
         width = 14, height = 10, dpi = "retina")

# to create second plot change [ ydstogo <= 5 ] to [ ydstogo >= 7 ]
# also change titles
# also change .png file name when saving
