# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(stringr)
library(ggbeeswarm)
library(ggrepel)

# load 2026 pbp data
data26 = load_pbp(2026)

# get epa per dropback and length of last name
passers26 = data26 %>%
  filter(!is.na(passer_player_id),
         qb_dropback == 1,
         qb_kneel == 0,
         qb_spike == 0,
         aborted_play == 0) %>%
  group_by(posteam,
           passer_player_id,
           passer_player_name) %>%
  summarize(dropbacks = n(),
            epa_dropback = sum(epa)/dropbacks,
            name_len = mean(str_length(passer_player_name))-2) %>%
  arrange(-epa_dropback) %>%
  filter(dropbacks >= 30) %>%
  print(width = Inf,
        n = Inf)

# test relationship
cor.test(passers26$name_len,
         passers26$epa_dropback)

# plot data
epa_name_plot = ggplot(data = passers26,
                       aes(x = name_len,
                           y = epa_dropback)) +
  geom_hline(yintercept = mean(passers26$epa_dropback),
             linetype = "dashed",
             color = "grey30",
             alpha = 0.6) +
  geom_vline(xintercept = mean(passers26$name_len),
             linetype = "dashed",
             color = "grey30",
             alpha = 0.6) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_beeswarm(aes(color = posteam,
                    size = dropbacks)) +
  geom_text_repel(segment.color = NA,
                  box.padding = 0.3,
                  size = 5,
                  aes(label = passer_player_name,
                      color = posteam)) +
  scale_color_nfl(type = "primary") +
  labs(title = "QB Last Name Length vs. EPA Per Dropback",
       subtitle = "2026 NFL Wk 1-2 | min. 30 dropbacks | size = # of dropbacks",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "# of Letters",
       y = "EPA/Dropback") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect("#F2F4F5",
                                       color = NA),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     face = "bold"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.text = element_text(size = 12))

# view plot
epa_name_plot

# save plot to local files
ggsave("X post 81 - qb_namelen_epa.png",
       width = 10.5, height = 7,
       dpi = "retina")
