# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata4 = load_pbp(2024)

# define above .500 group by listing team abbr. for teams with a winning record
above500 = c("KC", "DET", "PHI", "MIN", "BUF", "PIT", "GB", "HOU", "BAL",
             "WAS", "LAC", "DEN", "TB", "LAR", "SEA")

# filter data
data339 = nfldata4 %>%
  filter(week < 16,
         qtr == 4,
         play_type == "run" | play_type == "pass",
         !is.na(posteam),
         !is.na(yards_gained),
         !is.na(qtr),
         !is.na(success)) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            ydsperp = sum(yards_gained)/sum(plays),
            sucrate = sum(success)/sum(plays))%>%
  print(n = Inf)

# plot the data
plot4th = data339 %>%
  mutate(color1 = ifelse(posteam %in% above500, NA, "b/w")) %>%
  ggplot(aes(x = ydsperp, y = sucrate)) +
  geom_hline(yintercept = mean(data339$sucrate),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(data339$ydsperp),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",        # add trend line
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam,
                     color = color1),
                 alpha = 0.9,
                 width = 0.065) +
  labs(title = "4th Quarter Yards Per Play vs. Success Rate",
       subtitle = "2024 NFL Weeks 1-15 (color = above .500 record)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Yards Per Play",
       y = "Success Rate") +
  scale_color_identity() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 20),
        plot.caption = element_text(size = 13),
        axis.title = element_text(face = "bold.italic",
                                  size = 15),
        axis.text = element_text(size = 13))

# view plot
plot4th

# save plot to local files
ggsave("X post 13 - yards_success_4th.png",
       width = 14, height = 10,
       dpi = "retina")
