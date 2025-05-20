# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data to rushing and passing plays
# filter epa to narrow list to bottom 15 player in epa/play
totalepa = nfldata %>%
  filter(week < 19,
         play_type == "pass",
         qb_spike == 0,
         !is.na(epa)) %>%
  group_by(id,
           name,
           posteam) %>%
  summarize(att = n(),
            epa = sum(epa)/sum(att),
            .groups = "drop") %>%
  filter(epa <= -0.06,
         att > 250) %>%
  arrange(epa) %>%
  print(n = Inf)

# create plot
avgepaplot = ggplot(data = totalepa,
                    aes(x = reorder(id, epa), y = epa)) +
  geom_col(position = "dodge",
           aes(color = posteam,
               fill = posteam),
           width = 0.5) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.065) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.8) +
  labs(title = "EPA Per Pass Attempt - Bottom 10",
       subtitle = "2024 NFL Regular Season | min. 200 att.",
       y = "EPA/Pass Att.",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 25),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 22),
        plot.caption = element_text(size = 14),
        plot.background = element_rect(fill = "white"),
        axis.title.y = element_text(face = "bold",
                                    size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.x = element_nfl_headshot(size = 2.5))

# view plot
avgepaplot

# save plot to local files
ggsave("X post 59 - epa_per_pass.png",
       width = 14, height = 10,
       dpi = "retina")
