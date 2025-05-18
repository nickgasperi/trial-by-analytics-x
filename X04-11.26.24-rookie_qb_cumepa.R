# load packages
library(tidyverse)   # data wrangling
library(ggrepel)     # replaces geom_text()
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
data1 = load_pbp(2024)

# define group of players to include J.Daniels; C.Williams; D.Maye; B.Nix
qbrookie = c("00-0039910", "00-0039918", "00-0039851", "00-0039732")
  
# wrangle data into new tibble
data24rook = data1 %>%
  filter(week < 13,                     # include only passes from weeks 1-12
         !is.na(epa),
         passer_id %in% qbrookie) %>%    # include only players whose IDs are listed in line 12 above
  group_by(passer_id,                   
           passer,
           team = posteam,
           week) %>%               # group by week so EPA is summarized by game
  summarize(att = n(),
            epa = sum(epa),
            .groups = "drop") %>%
  arrange(passer,
          week) %>%
  print(n = Inf)

# add column to tibble to calculate running total for EPA
# tracks changes in cumulative EPA with each pass attempt
data24rook$cumepa = ave(data24rook$epa, data24rook$passer, FUN = cumsum)
  
# define week 6
# used later in ggplot() when plotting player headshots
week6 = data24rook %>%
  filter(week == 6)
  
# plot Cumulative EPA
plotqbrook1 = ggplot(data = data24rook,
                     aes(x = week, y = cumepa,
                         group = passer)) +
  geom_line(aes(color = passer))+
  scale_color_manual(values = c("orange", "navy", "blue", "red")) +   # apply team color to players' lines
  geom_point(aes(color = passer)) +                                    # apply team color to points
  geom_nfl_headshots(data = week6,
                     aes(player_gsis = passer_id),      # insert player headshots where week = 6 on x-axis
                     height = 0.12) +
  scale_x_continuous(n.breaks = 12) +                     # custom x and y axis label scaling
  scale_y_continuous(breaks = seq(-45, 110, by = 25)) +
  labs(title = "NFL Rookie QB Cumulative EPA By Week",
       subtitle = "2024 Weeks 1-12 (min. 250 att.)",
       x = "Week",
       y = "Cumulative EPA",
       caption = "By Nick Gasperi | Data @nflfastR") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 11),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold",
                                  size = 11))

# view plot
plotqbrook1

# save plot to local files
ggsave("X Post 4 - Rookie QB Cumulative EPA.png",
       width = 14, height = 10,
       dpi = "retina")