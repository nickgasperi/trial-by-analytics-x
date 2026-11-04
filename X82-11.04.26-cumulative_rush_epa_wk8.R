# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2026 week 1 pbp data
data26_8 = load_pbp(2026)

# get list of players with at least 60 rush attempts
rusher_list_8 = data26_8 %>%
  filter(week < 9,
         rush == 1,
         !is.na(rusher_player_id),
         !is.na(epa),
         sack == 0,
         qb_kneel == 0,
         aborted_play == 0) %>%
  group_by(rusher_player_id) %>%
  summarize(att = n(),
            .groups = "drop") %>%
  filter(att >= 60) %>%
  select(rusher_player_id)

# call running plays from rushers included in first tibble
# calculate cumulative attempts and cumulative epa
rush_data_26_8 = data26_8 %>%
  filter(rush == 1,
         rusher_player_id %in% rusher_list_8$rusher_player_id,
         !is.na(epa),
         sack == 0,
         qb_kneel == 0,
         aborted_play == 0) %>%
  group_by(game_id,
           play_id,
           rusher_player_id,
           rusher_player_name,
           posteam,
           epa) %>%
  summarize(att = n(),
            .groups = "drop") %>%
  arrange(rusher_player_id,
          game_id,
          play_id) %>%
  group_by(rusher_player_id) %>%
  mutate(cum_att = cumsum(att),
         cum_epa = cumsum(epa)) %>%
  ungroup()

# call out top 3 and bottom 3 rushers in terms of total epa
 # get epa totals by rusher
epa_totals = rush_data_26_8 %>%
  group_by(rusher_player_id) %>%
  summarize(tot_epa = sum(epa),
            .groups = "drop")

 # define top 3
epa_top3 = epa_totals %>%
  slice_max(tot_epa,
            n = 3,
            with_ties = FALSE) %>%
  pull(rusher_player_id)

 # define bottom 3
epa_bottom3 = epa_totals %>%
  slice_min(tot_epa,
            n = 3,
            with_ties = FALSE) %>%
  pull(rusher_player_id)

# plot
rush_26_8_plot = rush_data_26_8 %>%
  group_by(rusher_player_id) %>%
    mutate(namehighlight = ifelse(rusher_player_id %in% c(epa_top3, epa_bottom3) &
                                     cum_att == max(cum_att),
                                  rusher_player_name,
                                  NA)) %>%
  ungroup() %>%
  mutate(colorhighlight = ifelse(rusher_player_id %in% c(epa_top3, epa_bottom3),
                                posteam,
                                "grey")) %>%
  mutate(alphahighlight = ifelse(rusher_player_id %in% c(epa_top3, epa_bottom3),
                                 1.00,
                                 0.12)) %>%
  ggplot(aes(x = cum_att,
             y = cum_epa,
             group = rusher_player_id)) +
  geom_line(aes(color = colorhighlight,
                alpha = alphahighlight),
            linewidth = 1.0) +
  geom_nfl_logos(data = ~ filter(.x,
                                 !is.na(namehighlight)),
                 aes(team_abbr = posteam),
                 width = 0.05) +
  geom_text_repel(aes(label = namehighlight,
                      color = colorhighlight),
                  nudge_x = -11,
                  nudge_y = 3,
                  segment.colour = NA,
                  size = 5.0) +
  scale_color_nfl() +
  scale_alpha_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(n.breaks = 6) +
  labs(title = "Cumulative Rushing EPA by Player",
       subtitle = "2026 NFL Weeks 1-8 | min. 60 rush att.",
       x = "Rush Attempts",
       y = "Cumulative EPA",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F2F4F5",
                                       color = NA),
        plot.title = element_text(face = "bold",
                                  size = 20),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.title.y = element_text(margin = margin(t = 0,
                                                    r = 7,
                                                    b = 0,
                                                    l = 0)),
        axis.text = element_text(size = 13),
        axis.text.x = element_blank())

# view plot
rush_26_8_plot

# save plot to local files
ggsave("X post 82 - cumulative_rush_epa_wk8.png",
       width = 10.5, height = 7,
       dpi = "retina")
