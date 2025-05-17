# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data
data5 = load_pbp(2024)

# define games won by Washington
wins1 = c(2, 3, 4, 5, 7, 8, 9, 13, 15)

# calculate yards per play before and after week 8
# run the same code again but flip the < 9 to > 8
data5 %>%
filter(!is.na(yards_gained),
       !is.na(posteam),
       !is.na(week),
       posteam == "WAS",
       play_type == "pass" | play_type == "run",
       week > 8) %>%
group_by(posteam) %>%
summarize(plays = n(),
          ydperp = sum(yards_gained)/sum(plays)) %>%
print(n = Inf)

# filter the data
data454 = data5 %>%
  filter(!is.na(yards_gained),
         !is.na(posteam),
         !is.na(week),
         posteam == "WAS",
         play_type == "pass" | play_type == "run",
         week < 16) %>%
  group_by(posteam,
           week) %>%
  summarize(plays = n(),
            ydperp = sum(yards_gained)/sum(plays)) %>%
  arrange(week) %>%
  print(n = Inf)

# mutate color scheme so wins will be in color and lossess in b/w
# plot with ggplot
# add yards per play horizontal lines w/ labels
# add logos and team colors
wasplot1 = data454 %>%
  mutate(color2 = ifelse(week %in% wins1, NA, "b/w")) %>%
  ggplot(aes(x = week, y = ydperp)) +
  annotate("segment", x = 1, xend = 8.5, y = 6.36, yend = 6.36, linetype = "dashed", color = "red") +
  annotate("text", label = "Avg = 6.36", x = 7, y = 6.25, fontface = "bold.italic", size = 6) +
  annotate("segment", x = 8.5, xend = 15, y = 5.22, yend = 5.22, linetype = "dashed", color = "red") +
  annotate("text", label = "Avg = 5.22", x = 10, y = 5.1, fontface = "bold.italic", size = 6) +
  geom_vline(xintercept = 8.5, color = "blue", linewidth = 1.5) +
  geom_nfl_logos(aes(team_abbr = posteam, color = color2), alpha = 0.85, width = .07) +
  scale_color_identity() +
  scale_x_continuous(n.breaks = 15) +
  labs(title = "Washington Yards Per Play By Week",
       subtitle = "2024 Wk 1-8 vs. Wk 9-15 (color = win)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Week", y = "Yards Per Play") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.caption = element_text(size = 13),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 15))

# view plot
wasplot1

# save plot to local files
ggsave("X post 15 - wasfirstandsecond.png",
       width = 14, height = 10, dpi = "retina")
