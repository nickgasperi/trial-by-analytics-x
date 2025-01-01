# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)
  library(ggrepel)

# load data
  nfldata = load_pbp(2024)

# filter data
  wk17 = nfldata %>%
    filter(week == 17,
           play_type == "pass",
           qb_spike == 0,
           !is.na(air_yards),
           !is.na(passer_player_id),
           !is.na(cpoe)) %>%
    group_by(passer_player_id,
             passer_player_name,
             posteam) %>%
    summarize(att = n(),
              epa = sum(epa)/sum(att),
              cpoe = mean(cpoe)) %>%
    filter(att > 10) %>%
    arrange(-att) %>%
    print(n = Inf)

# plot data
  wk17plot = ggplot(data = wk17, aes(x = cpoe, y = epa)) +
    geom_hline(yintercept = mean(wk17$epa), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(wk17$cpoe), linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.05, alpha = 0.8) +
    geom_text_repel(box.padding = 0.9,
                    aes(label = passer_player_name, color = posteam, fontface = "bold", size = 11)) +
    scale_color_nfl(type = "primary") +
    labs(title = "CPOE vs. EPA Per Pass Attempt",
         subtitle = "NFL Week 17 (min. 10 att.)",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "CPOE", y = "EPA") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.caption = element_text(size = 13),
          axis.title = element_text(face = "bold.italic", size = 15),
          axis.text = element_text(size = 15))

# view plot
  wk17plot

# save plot
  ggsave("X post 22 - wk17_qbs.png",
         width = 14, height = 10, dpi = "retina")
