# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata01 = load_pbp(2024)

# wrangle data into new tibble
nflwc24 = nfldata01 %>%
  filter(week == 19,
         play_type == "pass" | play_type == "run",
         qb_kneel == 0,
         qb_spike == 0,
         !is.na(posteam),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            epaper = sum(epa)/sum(plays)) %>%
  print(n = Inf)

# plot week 19 volume and efficiency 
wcplot11 = ggplot(data = nflwc24,
                  aes(x = plays, y = epaper)) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_hline(yintercept = mean(nflwc24$epaper),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(nflwc24$plays),
             linetype = "dashed",
             color = "red") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.075,
                 alpha = 0.8) +
  labs(title = "Offensive Volume vs. Efficiency",
       subtitle = "2024 NFL Wildcard Round",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Plays",
       y = "EPA Per Play") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic",
                                  size = 14),
        axis.text = element_text(size = 14))

# view plot
wcplot11

# save plot to local files
ggsave("X post 42 - wc_offense.png",
       width = 10.5, height = 7,
       dpi = "retina")
