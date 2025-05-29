# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load NFL data from 2016-2024 seasons
nfldata8 = load_pbp(2016:2024)

# filter data
passerdata = nfldata8 %>%
  filter(season_type == "REG",
         qb_spike == 0,
         !is.na(air_yards),
         !is.na(epa),
         !is.na(passer_player_id)) %>%
  group_by(season,
           passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            epaper = sum(epa)/sum(att),
            .groups = "drop") %>%
  filter(att >= 300) %>%
  arrange(-epaper) %>%
  print(n = 20)

# create extra column for future viz
passerdata$passeryr = paste(passerdata$passer_player_name, passerdata$season, sep = ", ")

# view updated tibble
passerdata

# define QBs to highlight in plot
currentqb = c("L.Jackson, 2024", "J.Goff, 2024", "J.Allen, 2024",
              "J.Burrow, 2024", "J.Daniels, 2024", "P.Mahomes, 2024",
              "B.Mayfield, 2024")

# plot data
passerplot2 = passerdata %>%
  mutate(text3 = ifelse(passeryr %in% currentqb, passeryr, "")) %>%
  mutate(alpha3 = ifelse(passeryr %in% currentqb, 1, 0.5)) %>%
  mutate(color3 = ifelse(passeryr %in% currentqb, posteam, "b/w")) %>%
  ggplot(aes(x = att, epaper)) +
  geom_hline(yintercept = mean(passerdata$epaper),
             linetype = "dashed") +
  geom_vline(xintercept = mean(passerdata$att),
             linetype = "dashed") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "darkgrey") +
  geom_point(aes(alpha = alpha3,
                 color = color3)) +
  geom_text_repel(box.padding = 0.1,
                  aes(label = text3,
                      color = posteam,
                      fontface = "bold.italic")) +
  scale_color_nfl(type = "primary") +
  labs(title = "Passing Volume vs. Efficiency",
       subtitle = "2016-2024 NFL Regular Seasons",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Pass Attempts",
       y = "EPA/Pass Attempt") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 14),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_text(face = "bold",
                                 size = 15))

# view plot
passerplot2

# save plot to local files
ggsave("X post 48 - reg_passers.png",
       width = 10.5, height = 7,
       dpi = "retina")
