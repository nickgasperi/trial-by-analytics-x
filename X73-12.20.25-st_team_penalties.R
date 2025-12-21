# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load data
nfldata25 = load_pbp(2025)

# wrangle data into tibble displaying total penalties committed and penalty yards suffered on kickoffs and punts
st1 = nfldata25 %>%
  filter(week < 16,
         !is.na(penalty_team),
         play_type %in% c("kickoff", "punt"),
         penalty == 1) %>%
  group_by(penalty_team) %>%
  summarize(penalties = n(),
            pen_yds = sum(penalty_yards,
                          na.rm = TRUE),
            .groups = "drop") %>%
  arrange(-penalties) %>%
  print(n = Inf)

# plot data
stplot1 = ggplot(data = st1,
                 aes(x = penalties,
                     y = pen_yds)) +
  geom_hline(yintercept = mean(st1$pen_yds),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_vline(xintercept = mean(st1$penalties),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "darkgrey",
              alpha = 0.60) +
  geom_nfl_logos(aes(team_abbr = penalty_team),
                     width = 0.06,
                     alpha = 0.80) +
  labs(title = "Penalties Committed vs. Penalty Yards Incurred",
       subtitle = "Kickoffs & Punts | 2025 NFL Wk 1-15",
       x = "# of Penalties",
       y = "Total Penalty Yards",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 18),
        plot.subtitle = element_text(face = "bold",
                                     size = 15),
        plot.caption = element_text(size = 12),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 13))

# view plot
stplot1

# save plot to local files
ggsave("X post 73 - st_team_penalties.png",
       width = 10.5, height = 7,
       dpi = "retina")