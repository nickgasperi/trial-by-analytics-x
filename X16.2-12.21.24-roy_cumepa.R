# load packages
library(tidyverse)  # data wrangling
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load NFL data from 2024 season
nfldata2 = load_pbp(2024)

# wrangle data into new tibble
# the code will be very similar to Plot 1's code - main difference is plotting 4 players instead of 2
epadata2 = nfldata %>%
  filter(play_type == "pass" | play_type == "run",
         id == "00-0039910" | id == "00-0039918" | id == "00-0039851" | id == "00-0039732",
         qb_kneel == 0, qb_spike == 0,
         !is.na(epa)) %>%       # exclude QB spikes and kneels as they do not offer value when gauging player efficiency
  group_by(play_id,
           id,
           name,
           posteam)%>%
  summarize(epa,
            .groups = "drop") %>%
  print(n = 15)

# add cumulative EPA column to existing tibble
epadata2$cumepa = ave(epadata2$epa, epadata2$name, FUN = cumsum)

# establish last plays for player image in ggplot
frame4 = epadata2 %>%
  filter(play_id == "4563" | play_id == "4502" | play_id == "4766" | play_id == "5018") %>%
  print(n = Inf)

# create cumulative EPA plot for top 4 rookie QBs
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

# save the plot to the device's local files
ggsave("X post 16.2 - epa_royrace.png",
       width = 14, height = 10, dpi = "retina")
