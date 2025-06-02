# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2024 NFL data
data24 = load_pbp(2024)

# wrangle data into new tibble
qbepa = data24 %>%
  filter(week < 12,
         play_type == "pass",
         !is.na(air_yards),
         !is.na(epa)) %>% 
  group_by(passer,
           team = posteam) %>%
  summarize(att = n(),
            epa = mean(epa),
            .groups = "drop") %>%
  filter(att > 158) %>%                 # include only QBs with more than 158 attempts
  arrange(att) %>%
  print(n = Inf)

# plot EPA Per Attempt
plotqbepa1 = ggplot(data = qbepa,
                    aes(x = epa, y = reorder(passer,epa))) +
  geom_col(aes(color = team,
               fill = team),
           width = .6) +
  scale_color_nfl(type = "secondary") +       # apply secondary team color to column borders
  scale_fill_nfl(alpha = .7) +                # apply team color to column
  geom_nfl_logos(aes(team_abbr = team),       # add team logo to end of each column
                 width = 0.033,
                 alpha = 0.75) +
  labs(title = "2024 QB EPA Per Attempt",
       subtitle = "Thru Week 11 (min. 158 att.)",
       x = "EPA Per Attempt",
       y = "",
       caption = "By Nick Gasperi | Data @nflfastR") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size =15),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold",
                                  size = 10))

# view plot
plotqbepa1

# save plot to local files
ggsave("X Post 3 - EPA Per Attempt.png",
       width = 14, height = 10,
       dpi = "retina")
