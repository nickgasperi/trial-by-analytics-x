# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
data924 = load_pbp(2024)

# wrangle data into new tibble 
dataoff = data924 %>%
  filter(week < 13,
         !is.na(posteam),
         !is.na(yards_gained),
         !is.na(epa),
          (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarize(teamyds = sum(yards_gained),
            teamepa = sum(epa)) %>%
  print(n = Inf)

# plot Offensive Production
# use geom_vline and geom_hline to plot average on each axis
# use geom_smooth to add line of best fit
# use geom_nfl_logos to add team logo to each data point
plotrawoff = ggplot(data = dataoff, 
                    aes(x = teamyds, y = teamepa)) +
  geom_vline(xintercept = mean(dataoff$teamyds),
             linetype = "dashed",
             color = "red") +
  geom_hline(yintercept = mean(dataoff$teamepa),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  scale_y_continuous(breaks = seq(-100, 200,by = 50)) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = .07,
                 alpha = .8) +
  annotate("text",
           label = "Fake contenders?",
           x = 4100, y = -65,
           size = 5,
           color = "red",
           alpha = 0.5) +
  annotate("text",
           label = "Hopeless",
           x = 3450, y = -130,
           size = 5,
           color = "red") +
  annotate("text",
           label = "Has potential?",
           x = 3300, y = 15,
           size = 5,
           color = "green",
           alpha = 0.5) +
  annotate("text",
           label = "Talented & effective",
           x = 4100, y = 140,
           size = 5 ,
           color = "green") +
  annotate("text",
           label = "Ravens",
           x = 5050, y = 130,
           size = 5,
           color = "purple") +
  labs(title = "Raw Offensive Production ",
       subtitle = "2024 NFL Weeks 1-12",
       x = "Offensive Yards Gained",
       y = "Cumulative Offensive EPA",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 13),
        axis.title = element_text(face = "bold",
                                  size = 12))

# view plot
plotrawoff

# save plot to local files
ggsave("X Post 5 - Offensive Production.png",
       width = 14, height = 10,
       dpi = "retina")