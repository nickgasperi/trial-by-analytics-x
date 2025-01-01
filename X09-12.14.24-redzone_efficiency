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

# filter data to only runs and passes in the redzone
# summarize by player
# filter to only players with 18 or more RZ touches
  rzone = nfldata %>%
    filter((play_type == "run" | play_type == "pass"),
           week < 15,
           !is.na(epa),
           yardline_100 >= 80) %>%
    group_by(id, name, posteam) %>%
    summarize(plays = n(),
              epa = mean(epa)) %>%
    filter(plays >= 18) %>%
    arrange(-plays) %>%
    print(n = Inf)

# plot data
  rzplot = ggplot(data = rzone, aes(x = plays, y = epa)) +
    geom_nfl_logos(aes(team_abbr = posteam),
                   width = 0.035, alpha = 0.8) +
    geom_text_repel(box.padding = 0.3, aes(label = name)) +
    geom_vline(xintercept = mean(rzone$plays),
               linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean(rzone$epa),
               linetype = "dashed", color = "red") +
    labs(title = "2024 NFL Redzone Efficiency",
         subtitle = "Weeks 1-14 (min. 18 touches)",
         x = "Plays", y = "EPA Per Play",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18),
          axis.title = element_text(face = "bold.italic", size = 16),
          axis.text = element_text(size = 13),
          plot.caption = element_text(size = 12))

# view the plot
  rzplot

# save the plot
  ggsave("X post 9 - Redzone Touches and Efficiency.png",
         width = 14, height = 10, dpi = "retina")
