# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter and summarize data to include pass attempts, yac/att, epa/att
# group and filter to view only players with at least 10 att
wk16 = nfldata %>%
  filter(week == 16,
         play_type == "pass",
         !is.na(air_yards),
         !is.na(posteam),
         !is.na(passer_player_id)) %>%
  mutate(yards_after_catch = ifelse(is.na(yards_after_catch), 0, yards_after_catch)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            yds = sum(yards_gained),
            epa = sum(epa)/sum(att),
            yac = sum(yards_after_catch)/sum(att)) %>%
  filter(att >= 10) %>%
  print(n = Inf)

# plot YAC and EPA/attempt

plotwk16 = ggplot(data = wk16,
                  aes(x = yac, y = epa)) +
  geom_hline(yintercept = mean(wk16$epa),   # add horizontal average line
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = mean(wk16$yac),    # add vertical average line
             linetype = "dashed",
             color = "black") +
  geom_smooth(method = "lm",        # add trend line
              se = FALSE,
              color = "grey") +
  geom_point(aes(color = posteam,
                 size = wk16$att,
                 alpha = 0.85)) +
  scale_color_nfl(type = "primary") +
  geom_text_repel(aes(label = passer_player_name,
                      color = posteam,
                      fontface = "bold",
                      size = 30)) +
  labs(title = "YAC & EPA Per Attempt",
       subtitle = "2024 NFL Week 16 | size = att.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "YAC",
       y = "EPA") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 20),
        plot.caption = element_text(size = 13),
        axis.title = element_text(face = "bold.italic",
                                  size = 15),
        axis.text = element_text(size = 15))

# view plot
plotwk16

# save plot to local files
ggsave("X post 18 - wk16_passing.png",
       width = 14, height = 10,
       dpi = "retina")  
