# load packages
  library(tidyverse)
  library(dplyr)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)
  library(ggrepel)

# load 2024 data
  data1 = load_pbp(2024)

# filter data
  qbdata = data1 %>%
    filter(!is.na(epa),
           !is.na(passer_player_id),
           !is.na(yards_after_catch),
           !is.na(yards_gained)) %>%
    group_by(passer_player_id,
             passer_player_name,
             posteam) %>%
    summarize(completions = n(),
              epa = sum(epa)/sum(completions),
              yac = sum(yards_after_catch)/sum(completions),
              .groups = "drop") %>%
    filter(completions > 99) %>%
    arrange(completions) %>%
    print(n = Inf)

# plot the data
  plotqbdata = ggplot(data = qbdata, aes(x = yac, y = epa, )) +
    geom_hline(yintercept = mean(qbdata$epa), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(qbdata$yac), linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.045, alpha = 0.8) +
    geom_text_repel(box.padding = 0.8, aes(label = qbdata$passer_player_name)) +
    labs(title = "Average YAC and EPA Per Completion", subtitle = "2024 NFL Weeks 1-14 (min. 100 att.)",
         x = "YAC", y = "EPA",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 15),
          axis.title = element_text(face = "bold.italic", size = 13),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10))

# view the plot
  plotqbdata

# export the plot
  ggsave("X Post 7 - YAC and EPA.png", width = 14, height = 10, dpi = "retina")
