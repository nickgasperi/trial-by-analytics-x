# load packages
library(tidyverse)   # data wrangling
library(ggrepel)     # replaces geom_text
library(nflreadr)    
library(nflfastR)
library(nflplotR)
      
# load 2024 NFL data
pbp24 = load_pbp(2024)

# wrangle data into new tibble
wk11_24_pass_air_epa = pbp24 %>%
  filter(week == 11,
          play_type == "pass",
          !is.na(air_yards)) %>%    # include only passes from week 11   
  group_by(id, name) %>%
  summarize(team = last(posteam),
            att = n(),
            ydair = mean(air_yards),
            epa = mean(epa)) %>%
  filter(att > 15) %>%              # include only players with more than 15 pass attempts
  arrange(epa) %>%                  # use arrange to sort tibble
  print(n = Inf)                    # print all rows

# plot week 11 QB performance
plot221 = ggplot(data = wk11_24_pass_air_epa,
                 aes(x = ydair, y = epa)) +
  labs(title = "2024 Week 11 Passing Performance",
       subtitle = "Aggressiveness vs. Effectiveness",
       x = "Air Yards Per Attempt",
       y = "EPA Per Attempt") +
  geom_text_repel(aes(label = wk11_24_pass_air_epa$name)) +       # replaces geom_text
  geom_nfl_logos(aes(team_abbr = team), width = .05)+             # add player team logo to data points
  geom_hline(yintercept = mean(wk11_24_pass_air_epa$epa),
             linetype = "dashed") +                               # plot avg epa
  geom_vline(xintercept = mean(wk11_24_pass_air_epa$ydair),
             linetype = "dashed") +                               # plot avg air yards
  theme_bw() +
  theme(plot.title = element_text(face="bold", hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# view plot
plot221

# save plot to files
ggsave("X post 1 - wk11_passing.png",
       width = 14, height = 10, dpi = "retina")