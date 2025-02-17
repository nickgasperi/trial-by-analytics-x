# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)

# load data
  nfldata2 = load_pbp(2024)

# filter data
  epadata2 = nfldata %>%
    filter(play_type == "pass" | play_type == "run",
           id == "00-0039910" | id == "00-0039918" | id == "00-0039851" | id == "00-0039732",
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
  epadata2$cumepa = ave(epadata2$epa, epadata2$name, FUN = cumsum)

# establish last plays for headshot viz
  frame4 = epadata2 %>%
    filter(play_id == "4563" | play_id == "4502" | play_id == "4766" | play_id == "5018") %>%
    print(n = Inf)

# plot data
  plotepa2 = ggplot(data = epadata2, aes(x = play_id, y = cumepa)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_line(aes(color = name)) +
    scale_color_manual(values = c("orange", "navy", "blue", "red")) +
    geom_nfl_headshots(data = frame4, aes(player_gsis = id),
                       height = 0.10) +
    labs(title = "Rookie QBs - Cumulative EPA",
         subtitle = "2024 NFL Wk 1-15 | Passing and Rushing Plays",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "",
         y = "Cumulative EPA") +
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
  plotepa2

# save the plot
  ggsave("X post 16.2 - epa_royrace.png",
         width = 14, height = 10, dpi = "retina")
