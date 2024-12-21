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
  epadata = nfldata %>%
    filter(play_type == "pass" | play_type == "run",
           id == "00-0034796" | id == "00-0034857",
           qb_kneel == 0, qb_spike == 0,
           !is.na(epa)) %>%
    group_by(play_id,
             id,
             name,
             posteam)%>%
    summarize(epa,
              .groups = "drop") %>%
    print(n = 15)

# add cumulative epa column
  epadata$cumepa = ave(epadata$epa, epadata$name, FUN = cumsum)

# establish last plays for headshot viz
  frame2 = epadata %>%
    filter(play_id == "4521" | play_id == "4612") %>%
    print(n = Inf)

# plot data
  plotepa = ggplot(data = epadata, aes(x = play_id, y = cumepa)) +
    geom_line(aes(color = name)) +
    scale_color_manual(values = c("red", "purple")) +
    labs(title = "Josh Allen vs. Lamar Jackson - Cumulative EPA",
         subtitle = "2024 NFL Wk 1-15 | Passing and Rushing Plays",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "",
         y = "Cumulative EPA") +
    geom_nfl_headshots(data = frame2, aes(player_gsis = id),
                       height = 0.09) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#F0F0F0"),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.caption = element_text(size = 13),
          axis.title = element_text(face = "bold", size = 15),
          axis.text = element_text(size = 15),
          legend.position = "none")

# view the plot
  plotepa

# save the plot
  ggsave("X post 16 - epa_mvprace.png",
         width = 14, height = 10, dpi = "retina")
