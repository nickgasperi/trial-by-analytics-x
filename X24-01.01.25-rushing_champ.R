# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data
nfl0 = load_pbp(2024)

# filter to call ids of each of the top 5 rush yd leaders
# call only rushing plays
nflrb = nfl0 %>%
  filter(week < 18,
         id == "00-0034844" | id == "00-0032764" | id == "00-0037840" | id == "00-0038542" | id == "00-0035700",
         play_type == "run",
         !is.na(yards_gained)) %>%
  group_by(play_id,
           id,
           name,
           posteam) %>%
  summarize(att = n(),
            yds = sum(yards_gained),
            .groups = "drop") %>%
  print(n = 20)

# add new column to tibble that calculates cumulative rush yds column
nflrb$cumyds = ave(nflrb$yds, nflrb$name, FUN = cumsum)

# establish last play for each player for geom_point label
frame3 = nflrb %>%
  filter(play_id == "4339" | play_id == "4450" | play_id == "4608" | play_id == "4682" | play_id == "5198") %>%
  print(n = Inf)

# plot data
# separate geom line and geom text for Saquon's data for emphasis
rbplot1 = ggplot(data = nflrb,
                 aes(x = play_id, y = cumyds)) +
  geom_hline(yintercept = 2000,
             linetype = "dashed",
             color = "black") +
  geom_line(aes(color = posteam)) +
  geom_line(data = filter(nflrb,
                          name == "S.Barkley"),
            linewidth = 1.3,
            aes(color = posteam)) +
  geom_point(data = frame3,
             aes(color = posteam)) +
  geom_text_repel(box.padding = 1.0,
                  data = filter(frame3, name == "B.Robinson" | name == "J.Jacobs" | name == "K.Williams" | name == "D.Henry"), 
                  aes(label = name,
                      color = posteam,
                      fontface = "bold")) +
  geom_text_repel(box.padding = 0.3,
                  data = filter(frame3,
                                name == "S.Barkley"),
                  aes(label = name,
                      color = posteam,
                      fontface = "bold.italic",
                      size = 2)) +
  scale_color_nfl(type = "primary") +
  labs(title = "2024 NFL Rushing Title Race",
       subtitle = "Thru Week 17",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Rushing Attempts",
       y = "Rushing Yards") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 19),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 17),
        plot.caption = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title = element_text(face = "bold",
                                  size = 12),
        axis.text.y = element_text(size = 11))

# view plot
rbplot1

# save plot to local files
ggsave("X post 24 - rushing_champ.png",
       width = 10.5, height = 7,
       dpi = "retina")
