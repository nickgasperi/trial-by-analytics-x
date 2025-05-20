# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
data888 = load_pbp(2024)

# declare playoff teams
playoffteam = data888 %>%
  select(posteam) %>%
  filter(posteam == "BAL" | posteam == "BUF" | posteam == "DEN" | posteam == "DET" |
         posteam == "GB" | posteam == "HOU" | posteam == "KC" | posteam == "LA" |
         posteam == "LAC" | posteam == "MIN" | posteam == "PHI" | posteam == "PIT" |
         posteam == "TB" | posteam == "WAS")

# filter data
# two pt conv. results are stored as characters - mutate to binary
# create two pt att per game and two pt success rate
usedata = data888 %>%
  mutate(two_point_conv_result = ifelse(two_point_conv_result == "success", 1, 0)) %>%
  filter(!is.na(two_point_attempt),
         !is.na(two_point_conv_result)) %>%
  group_by(posteam) %>%
  summarize(attempts = sum(two_point_attempt),
            attperg = sum(two_point_attempt)/17,
            successes = sum(two_point_conv_result),
            sucrate = sum(two_point_conv_result/sum(two_point_attempt))) %>%
  print(n = Inf)

# plot data
# select only playoff teams
twoptplot = ggplot(subset(usedata, posteam %in% playoffteam),
                   aes(x = attperg, y = sucrate)) +
  geom_hline(yintercept = mean(usedata$sucrate),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(usedata$attperg),
             linetype = "dashed",
             color = "red") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 alpha = 0.8,
                 width = 0.07) +
  geom_text_repel(box.padding = 1.0,
                  aes(label = posteam,
                      color = posteam,
                      fontface = "bold.italic",
                      size = 2)) +
  scale_color_nfl() +
  labs(title = "2024 NFL Playoff Teams | 2-Pt Conversions",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Attempts Per Game",
       y = "% Successful") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 20),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 16),
        axis.text = element_text(size = 14))

# view the plot
twoptplot

# save plot to local files
ggsave("X post 29 - twopt_conv.png",
       width = 10.5, height = 7,
       dpi = "retina")
