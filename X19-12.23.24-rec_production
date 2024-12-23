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

# filter data to pass catchers
# change NA's to 0
# filter to receivers with at least 80 targets
  wrset = nfldata %>%
    filter(!is.na(receiver_player_id),
           !is.na(air_yards)) %>%
    mutate(yards_after_catch = ifelse(is.na(yards_after_catch), 0, yards_after_catch)) %>%
    group_by(receiver_player_id, receiver_player_name,
             posteam) %>%
    summarize(targets = n(),
              yac = sum(yards_after_catch)/sum(targets),
              yds = sum(yards_gained)/sum(targets)) %>%
    filter(targets >= 80) %>%
    print(n = Inf)

# plot the data
  wrplot1 = ggplot(data = wrset, aes(x = yac, y = yds)) +
    geom_hline(yintercept = mean(wrset$yds), linetype = "dashed", color = "black") +
    geom_vline(xintercept = mean(wrset$yac), linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_point(aes(color = posteam)) +
    scale_color_nfl(type = "primary") +
    geom_text_repel(aes(label = receiver_player_name, size = 8)) +
    labs(title = "YAC vs. Total Yards Per Target",
         subtitle = "2024 NFL Wk 1-16 | min. 80 targets",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "YAC",
         y = "Total Yards") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.caption = element_text(size = 13),
          axis.title = element_text(face = "bold.italic", size = 15),
          axis.text = element_text(size = 15))

# view the plot
  wrplot1

# save the plot
  ggsave("X post 19 - rec_production.png",
         width = 14, height = 10, dpi = "retina")  
