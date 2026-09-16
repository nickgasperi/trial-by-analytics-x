# load packages
library(tidyverse)
library(gghighlight)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2026 week 1 pbp data
data26_1 = load_pbp(2026)

# filter to running plays & get running epa total by rusher
rush_data_26_1 = data26_1 %>%
  filter(!is.na(rusher_player_id),
         !is.na(yards_gained)) %>%
  group_by(play_id,
           rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(att = n(),
            epa = sum(epa),
            .groups = "drop")

# add cumulative rush att. and cumulative epa columns to tibble
rush_data_26_1$cum_att = ave(rush_data_26_1$att,
                            rush_data_26_1$rusher_player_id,
                            FUN = cumsum)

rush_data_26_1$cum_epa = ave(rush_data_26_1$epa,
                         rush_data_26_1$rusher_player_id,
                         FUN = cumsum)

# plot data
rush_26_1_plot = ggplot(data = rush_data_26_1,
                        aes(x = cum_att,
                            y = cum_epa,
                            group = rusher_player_id)) +
  geom_line(aes(color = posteam),
            linewidth = 1.0) +
  gghighlight(rusher_player_id %in% c("00-0038134",
                                      "00-0036275",
                                      "00-0032764",
                                      "00-0039139",
                                      "00-0036875"),
              label_key = rusher_player_name,
              label_params = list(size = 5),
              use_direct_label = TRUE,
              use_group_by = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  scale_color_nfl() +
  labs(title = "Cumulative Rushing EPA by Player",
       subtitle = "2026 NFL Week 1",
       x = "Rush Att.",
       y = "Cumulative EPA",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 17),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 13),
        axis.text.x = element_blank())

# view plot
rush_26_1_plot

# save plot to local files
ggsave("X post 77 - cumulative_rush_epa_wk1.png",
       width = 10.5, height = 7,
       dpi = "retina")
  