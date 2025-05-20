# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# wrangle data into new tibble
totalepa = nfldata %>%
  filter(week < 19,              # include only regular season plays
         play_type == "pass",    # include only passing plays
         qb_spike == 0,          # exclude spikes
         !is.na(epa)) %>%
  group_by(id,
           name,
           posteam) %>%
  summarize(att = n(),
            epa = sum(epa)/sum(att),
            .groups = "drop") %>%
  filter(epa <= -0.08,          # filter by epa/attempt to get bottom 10
         att >= 250) %>%         # players with at least 250 pass attempts
  arrange(epa) %>%
  print(n = Inf)

# plot epa per pass attempt
epaplotqb = ggplot(data = totalepa,
                    aes(x = reorder(id, epa), y = epa)) +      # reorder by epa/attempt
  geom_col(position = "dodge",
           aes(color = posteam,
               fill = posteam),
           width = 0.5) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06) +
  scale_color_nfl(type = "secondary") +                # apply secondary team colors to columns
  scale_fill_nfl(alpha = 0.8) +                     # fill column with primary team color
  labs(title = "EPA Per Pass Attempt - Bottom 10",
       subtitle = "2024 NFL Regular Season | min. 250 att.",
       y = "EPA/Pass Att.",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 19),
        plot.caption = element_text(size = 13),
        plot.background = element_rect(fill = "#F0F0F0"),
        axis.title.y = element_text(face = "bold",
                                    size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_nfl_headshot(size = 1.8))

# view plot
epaplotqb

# save plot to local files
ggsave("X post 59 - epa_per_pass.png",
       width = 10.5, height = 7,
       dpi = "retina")
