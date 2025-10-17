# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(gghighlight)

# load 2025 data
nfldata25 = load_pbp(2025)


# Plot 1 - Passing Yards Leader -------------------------------------------

# tibble
passdata = nfldata25 %>%
  filter(!is.na(passer_player_id),
         !is.na(yards_gained)) %>%
  group_by(play_id,
           passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# add cumulative passing yds columns to existing tibble
passdata$cumyds = ave(passdata$yds, passdata$passer_player_id,
                      FUN = cumsum)
passdata$cumatt = ave(passdata$att, passdata$passer_player_id,
                      FUN = cumsum)

# check updated tibble for accuracy
passdata %>%
  filter(passer_player_name == "M.Stafford") %>%
  print(n = 15)

# plot data
passplot_a = ggplot(data = passdata,
                    aes(x = play_id,
                        y = cumyds,
                        group = passer_player_id)) +
  geom_line(aes(color = "blue"),
            linewidth = 1.0) +
  gghighlight(passer_player_id == "00-0026498",
              label_key = passer_player_name,
              use_direct_label = TRUE,
              unhighlighted_params = list(linewidth = 0.5),
              use_group_by = FALSE) +
  scale_color_identity() +
  labs(title = "NFL Passing Yards Leader",
       subtitle = "2025 Weeks 1-6",
       x = "",
       y = "Passing Yards",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 20),
        plot.subtitle = element_text(face = "bold",
                                     size = 18),
        plot.caption = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15))

# view plot
passplot_a

# save plot to local files
ggsave("X post 68.1 - pass_yd_leader.png",
       width = 10.5, height = 7,
       dpi = "retina") 

# Plot 2 - Rushing Yards Leader -------------------------------------------

# get rushing data
rushdata = nfldata25 %>%
  filter(!is.na(rusher_player_id),
         !is.na(yards_gained)) %>%
  group_by(play_id,
           rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(att = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# add cumulative rushing yds columns to existing tibble
rushdata$cumyds = ave(rushdata$yds, rushdata$rusher_player_id,
                      FUN = cumsum)
rushdata$cumatt = ave(rushdata$att, rushdata$rusher_player_id,
                      FUN = cumsum)

# check updated tibble for accuracy
rushdata %>%
  filter(rusher_player_name == "J.Taylor") %>%
  print(n = 15)
  
# plot data
rushplot_a = ggplot(data = rushdata,
                    aes(x = play_id,
                        y = cumyds,
                        group = rusher_player_id)) +
  geom_line(aes(color = "blue"),
            linewidth = 1.0) +
  gghighlight(rusher_player_id == "00-0036223",
              label_key = rusher_player_name,
              use_direct_label = TRUE,
              unhighlighted_params = list(linewidth = 0.5),
              use_group_by = FALSE) +
  scale_color_identity() +
  labs(title = "NFL Rushing Yards Leader",
       subtitle = "2025 Weeks 1-6",
       x = "",
       y = "Rushing Yards",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 20),
        plot.subtitle = element_text(face = "bold",
                                     size = 18),
        plot.caption = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15))

# view plot
rushplot_a

# save plot to local files
ggsave("X post 68.2 - rush_yd_leader.png",
       width = 10.5, height = 7,
       dpi = "retina")

# Plot 3 - Receiving Yards Leader -----------------------------------------

# get receiving data
recdata = nfldata25 %>%
  filter(!is.na(receiver_player_id),
         !is.na(yards_gained)) %>%
  group_by(play_id,
           receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(targets = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# add cumulative rec yds columns to existing tibble
recdata$cumyds = ave(recdata$yds, recdata$receiver_player_id,
                      FUN = cumsum)
recdata$cumtargets = ave(recdata$targets, recdata$receiver_player_id,
                      FUN = cumsum)

# check updated tibble for accuracy
recdata %>%
  filter(receiver_player_name == "J.Smith-Njigba") %>%
  print(n = Inf)

# plot data
recplot_a = ggplot(data = recdata,
                    aes(x = play_id,
                        y = cumyds,
                        group = receiver_player_id)) +
  geom_line(aes(color = "#69BE28"),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0038543",
              label_key = receiver_player_name,
              use_direct_label = TRUE,
              unhighlighted_params = list(linewidth = 0.5),
              use_group_by = FALSE) +
  scale_color_identity() +
  labs(title = "NFL Receiving Yards Leader",
       subtitle = "2025 Weeks 1-6",
       x = "",
       y = "Receiving Yards",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 20),
        plot.subtitle = element_text(face = "bold",
                                     size = 18),
        plot.caption = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15))

# view plot
recplot_a

# save plot to local files
ggsave("X post 68.3 - rec_yd_leader.png",
       width = 10.5, height = 7,
       dpi = "retina")
