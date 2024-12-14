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

# explore
  colnames(nfldata)

  nfldata %>%
    pull(yards_gained) %>%
    summary()

  nfldata %>%
    pull(first_down) %>%
    summary()

  nfldata %>%
    pull(yardline_100) %>%
    summary()

# filter to call only rushing plays between the 20s (no qb kneels)
# group by player
# calc % of plays that are a)first downs & b)negative yds
# filter for players with minimum rush attempts
  rbdata = nfldata %>%
    filter(week <= 14,
           !is.na(yards_gained),
           !is.na(first_down_rush),
           !is.na(rusher_player_id),
           !is.na(yardline_100),
           yardline_100 %in% (20:80),
           qb_kneel == 0,
           play_type == "run") %>%
    group_by(rusher_player_id,
             rusher_player_name,
             posteam) %>%
    summarize(rushes = n(),
              fdrushrt = sum(first_down_rush)/sum(rushes),
              negrushrt = sum(yards_gained < 0)/sum(rushes),
              .groups = "drop") %>%
    filter(rushes >= 68) %>%
    arrange(-rushes) %>%
    print(n = Inf)
  
# plot data
  plotrb = ggplot(data = rbdata, aes(x = fdrushrt, y = negrushrt)) +
    geom_hline(yintercept = mean(rbdata$negrushrt),
               linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(rbdata$fdrushrt),
               linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE,
                color = "grey") +
    geom_nfl_logos(aes(team_abbr = posteam),
                       width = 0.045, alpha = 0.8) +
    geom_text_repel(box.padding = 0.7, aes(label = rusher_player_name)) +
    labs(title = "First Down Rate vs. Negative Play Rate",
         subtitle = "2024 NFL Weeks 1-14 | Rushes Between the 20s (min. 68 att.)",
         caption = "By Nick Gasperi | @tbanalysis | data @nflfastR",
         x = "First Down Rate",
         y = "Negative Play Rate") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(face = "bold.italic", size = 16),
          axis.text = element_text(size = 13),
          plot.caption = element_text(size = 12))

# view the plot
  plotrb

# save the plot
  ggsave("X post 8 - Fdown Rate and NegPlay Rate.png",
         width = 14, height = 10, dpi = "retina")

# to create second plot - use same code but add this line when filtering to creating 'rbdata'
   rusher_player_name != c("L.Jackson", "J.Hurts", "J.Daniels")
# also change plot title and subtitle
