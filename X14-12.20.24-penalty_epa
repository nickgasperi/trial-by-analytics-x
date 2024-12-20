# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)
  library(ggrepel)

# load data
  nfldata92 = load_pbp(2024)

# filter data
  latedata1 = nfldata92 %>%
    filter(week < 16,
           yardline_100 >= 80,
           play_type == "run" | play_type == "pass",
           !is.na(posteam),
           !is.na(qtr),
           !is.na(touchdown),
           !is.na(epa)) %>%
    mutate(penalty_yards = ifelse(is.na(penalty_yards), 0, penalty_yards)) %>%
    group_by(posteam) %>%
    summarize(plays = n(),
              epa = sum(epa)/sum(plays),
              penalty = sum(penalty_yards)/sum(plays))%>%
    print(n = Inf)

# plot the data
  plot4th = ggplot(data = latedata1, aes(x = penalty, y = epa)) +
    geom_hline(yintercept = mean(latedata1$epa), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(latedata1$penalty), linetype = "dashed", color = "red") +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.055, alpha = 0.8) +
    labs(title = "Red Zone Penalty Rate vs. Effectiveness",
         subtitle = "2024 NFL Weeks 1-15 | Passing and Rushing Plays",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "Penalty Rate",
         y = "EPA Per Play") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.caption = element_text(size = 13),
          axis.title = element_text(face = "bold.italic", size = 15),
          axis.text = element_text(size = 15))
          
# view the plot
  plot4th

# save the plot
  ggsave("X post 14 - penaltyrate_epa.png",
       width = 14, height = 10, dpi = "retina")


