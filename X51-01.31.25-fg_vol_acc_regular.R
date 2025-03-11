# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# load NFL data from 2016-2024 seasons
nfldata8 = load_pbp(2016:2024)


# wrangle data into new tibble
kickdata = nfldata8 %>%
  filter(season_type == "REG",
         !is.na(field_goal_result),
         !is.na(kicker_player_id)) %>%
  group_by(kicker_player_id, kicker_player_name, season, posteam) %>%
  mutate(field_goal_result = ifelse(field_goal_result == "made", 1, 0)) %>%     # change field goal result variable to binary
  summarize(att = n(),
            pctmade = sum(field_goal_result)/att) %>%
  filter(att >= 15) %>%
  arrange(att) %>%
  print(n = Inf)

# plot field goal attempts and accuracy
kickplot1 = kickdata %>%
  mutate(color4 = ifelse(season == 2024, posteam, "b/W")) %>%        # this changes color of all data points from 2016-2023 seasons to black & white
  mutate(label4 = ifelse(season == 2024, kicker_player_name, "")) %>%     # this removes player name label from data points from 2016-2023 seasons
  ggplot(aes(x = att, y = pctmade)) +
  geom_hline(yintercept = mean(kickdata$pctmade),
             linetype = "dashed") +
  geom_vline(xintercept = mean(kickdata$att),
             linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point(aes(color = color4)) +
  geom_text_repel(box.padding = 0.3, max.overlaps = Inf,
                  aes(label = label4, color = posteam, fontface = "bold")) +
  scale_color_nfl(type = "primary") +
  labs(title = "NFL Field Goal Volume and Accuracy",
       subtitle = "2016-2024 Reg. Seasons | 2024 labeled",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "FG Attempts", y = "Conversion %") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic", size = 15),
        axis.text = element_text(size = 15))

# view plot
kickplot1

# save plot to device's local files
ggsave("X post 51 - kicking_regular.png",
       width = 10.5, height = 7, dpi = "retina")
