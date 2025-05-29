# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data
data3 = load_pbp(2024)

# filer data for plot
# include passing yards to filter only to starting QBs
toandtd = data3 %>%
  mutate(fumble_lost = ifelse(is.na(fumble_lost), 0, fumble_lost)) %>%
  mutate(interception = ifelse(is.na(interception), 0, interception)) %>%
  mutate(touchdown = ifelse(is.na(touchdown), 0, touchdown)) %>%
  mutate(passing_yards = ifelse(is.na(passing_yards), 0, passing_yards)) %>%
  filter(!is.na(id),
         !is.na(posteam)) %>%
  group_by(id,
           name,
           posteam) %>%
  summarize(turnovers = sum(fumble_lost)+sum(interception),
            tds = sum(touchdown),
            passyd = sum(passing_yards)) %>%
  filter(passyd > 1800) %>%
  arrange(-passyd) %>%
  print(n = Inf)

# plot data
toandtdplot = ggplot(data = toandtd,
                     aes(x = turnovers, y = tds)) +
  scale_x_reverse() +
  geom_hline(yintercept = mean(toandtd$tds),
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = mean(toandtd$turnovers),
             linetype = "dashed",
             color = "black") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam,
                     width = 0.06,
                     alpha = 0.)) +
  geom_text_repel(box.padding = 0.7,
                  aes(label = toandtd$name,
                      color = posteam)) +
  scale_color_nfl(type = "primary") +
  labs(title = "QB Turnovers vs. Total Touchdowns",
       subtitle = "2024 NFL Reg Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Turnovers",
       y = "Touchdowns") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 18),
        axis.title = element_text(face = "bold.italic",
                                  size = 16),
        axis.text = element_text(size = 13),
        plot.caption = element_text(size = 12))

# view plot
toandtdplot

# save plot to local files
ggsave("X post 31 - turnovers_touchdowns.png",
       width = 10.5, height = 7,
       dpi = "retina")
