# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)
  library(ggrepel)

# load data
  data59 = load_pbp(2024)

# filter data
  season1 = data59 %>%
    filter(play_type == "run",
           !is.na(rusher_player_id),
           !is.na(epa)) %>%
    group_by(rusher_player_id,
             name,
             posteam) %>%
    summarize(rushes = n(),
              epa = sum(epa)/sum(rushes),
              .groups = "drop") %>%
    filter(rushes > 150 & epa > 0.06) %>%
    arrange(-epa) %>%
    print(n = Inf)

# plot the data
  plotseason1 = ggplot(data = season1, aes(x = reorder(rusher_player_id, -epa), y = epa)) +
    geom_col(position = "dodge",
             aes(color = posteam, fill = posteam), width = 0.7) +
    geom_text(label = round(season1$epa, 2),
              position = position_stack(vjust = 0.5),
              fontface = "bold", color = "white", size = 7) +
    scale_color_nfl(type = "secondary") +
    scale_fill_nfl() +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.085) +
    labs(title = "2024 NFL EPA Per Rush (min. 150 att.)",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         y = "EPA/Rush") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#F0F0F0"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.caption = element_text(size = 11),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_nfl_headshot(size = 2.8))

# view plot
  plotseason1

# save plot
  ggsave("X post 28 - rush_epa_2024.png",
         width = 10.5, height = 7, dpi = "retina")
