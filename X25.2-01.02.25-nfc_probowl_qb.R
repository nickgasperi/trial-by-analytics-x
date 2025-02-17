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

# set qb and team groups for later viz
  qbnames1 = c("J.Goff", "B.Mayfield", "S.Darnold", "J.Daniels")
  teamnames1 = c("DET", "TB", "MIN", "WAS")

# filter data
# scrambles will be included
# mutate NA air_yards to 0 then filter using air_yards to include only QBs
  nflqbs = nfldata %>%
    filter(play_type == "pass" |play_type == "run",
           !is.na(id),
           !is.na(yards_gained),
           !is.na(epa),
           !is.na(success),
           qb_spike == 0, qb_kneel == 0) %>%
    group_by(id,
             name,
             posteam) %>%
    mutate(air_yards = ifelse(is.na(air_yards), 0, air_yards)) %>%
    summarize(plays = n(),
              epa = sum(epa)/sum(plays),
              succ = sum(success)/sum(plays),
              yds = sum(yards_gained),
              airyd = sum(air_yards)) %>%
    filter(airyd > 1840) %>%
    arrange(-plays) %>%
    print(n = Inf)

# plot data
  nfcqbplot2 = nflqbs %>%
    mutate(color2 = ifelse(posteam %in% teamnames1, NA, "b/w")) %>%
    mutate(width2 = ifelse(posteam %in% teamnames1, 0.08, 0.05)) %>%
    mutate(alpha2 = ifelse(posteam %in% teamnames1, 1.5, 1)) %>%
    mutate(name2 = ifelse(name %in% qbnames1, name, "")) %>%
    ggplot(aes(x = succ, y = epa)) +
    geom_hline(yintercept = mean(nflqbs$epa), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(nflqbs$succ), linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_nfl_logos(aes(team_abbr = posteam, color = color2, width = width2, alpha = alpha2)) +
    geom_text_repel(box.padding = 1.5, aes(label = name2, fontface = "bold", size = 2)) +
    scale_color_identity() +
    labs(title = "QB Success Rate vs. EPA Per Play",
         subtitle = "2024 NFL Wk 1-17 | Passes and Rushes",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "Success Rate", y = "EPA") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#F0F0F0"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 17),
          plot.caption = element_text(size = 10),
          axis.title = element_text(face = "bold.italic", size = 13),
          axis.text = element_text(size = 13))

# view plot
  nfcqbplot2

# save plot
  ggsave("X post 25.2 - probowl_qbs.png",
         width = 10.5, height = 7, dpi = "retina")
