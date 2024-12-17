# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)
  library(ggrepel)
  library(forcats)

# load data
  datanow = load_pbp(2024)

# filter data to rushing and passing plays
# filter epa to narrow list to bottom 15 player in epa/play
  totalepa = datanow %>%
    filter(play_type == "pass" | play_type == "run",
           !is.na(epa)) %>%
    group_by(id,
             name,
             posteam) %>%
    summarize(att = n(),
              epa = sum(epa)/sum(att),
              .groups = "drop") %>%
    filter(epa <= -.125,
           att > 100) %>%
    arrange(epa) %>%
    print(n = Inf)

# create plot
  avgepaplot = ggplot(data = totalepa, aes(x = reorder(id, epa), y = epa)) +
    geom_col(position = "dodge", aes(color = posteam, fill = posteam), width = 0.5) +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.055) +
    scale_color_nfl(type = "secondary") +
    scale_fill_nfl(alpha = 0.8) +
    labs(title = "EPA Per Play - Bottom 15 Players",
         subtitle = "2024 NFL Weeks 1-15 | Passing & Rushing Plays (min. 100 touches)",
         y = "EPA Per Play",
         caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.caption = element_text(size = 13),
          plot.background = element_rect(fill = "#F0F0F0"),
          axis.title.y = element_text(face = "bold.italic", size = 16),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_nfl_headshot(size = 1.8))

# view the plot
  avgepaplot

# save the plot
  ggsave("X post 11 - epapertouch_wk1_15.png",
         width = 14, height = 10, dpi = "retina")


