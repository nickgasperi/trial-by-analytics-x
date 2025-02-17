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
  day2rush = nfldata %>%
    mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards)) %>%
    filter(week == 19,
           posteam == "DEN" | posteam == "BUF" | posteam == "WAS" |
           posteam == "GB" | posteam == "PHI" | posteam == "TB",
           !is.na(epa),
           !is.na(rusher_player_id)) %>%
    group_by(rusher_player_id, rusher_player_name, posteam) %>%
    summarize(rushes = n(),
              avgepa = sum(epa)/sum(rushes),
              totepa = sum(epa),
              rushyd = sum(rushing_yards)) %>%
    arrange(-avgepa) %>%
    filter(rushes >= 10) %>%
    print(n = Inf)

# plot data
# geom_segment() replaces geom_col() - use geom_nfl_logos() instead of geom_point()
  day2rushplot = ggplot(data = day2rush, aes(x = avgepa, xend = 0,
                                         y = reorder(rusher_player_id, avgepa), yend = rusher_player_id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
    geom_segment(linewidth = 3.25, aes(color = posteam)) +
    scale_color_nfl(type = "primary") +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.06) +
    theme_minimal() +
    labs(title = "EPA Per Rush",
         subtitle = "2024 NFL Wild Card Weekend - Day 2",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "EPA/Rush") +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#F0F0F0"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.caption = element_text(size = 11),
          axis.title.y = element_blank(),
          axis.text.y  = element_nfl_headshot(size = 2.5),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.text.x = element_text(size = 14))

# view plot
  day2rushplot

# save plot
  ggsave("X post 39 - wc_d2_rush.png",
         width = 10.5, height = 6, dpi = "retina")
