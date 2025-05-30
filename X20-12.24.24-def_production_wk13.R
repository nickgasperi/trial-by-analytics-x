# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2024 NFL data
prepdata = load_pbp(2024)

# create tibble 1 for passing data
passdata = prepdata %>%
  filter(week > 12, 
         play_type == "pass",
         qb_kneel == 0,
         qb_spike == 0,   
         !is.na(air_yards),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(passes = n(),
            epapass = sum(epa)/sum(pass)) %>%
  arrange(defteam) %>%
  print(n = Inf)

# create tibble 2 for rushing data
rushdata = prepdata %>%
  filter(week > 12,  
         play_type == "run",
         qb_kneel == 0,
         !is.na(yards_gained),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(rushes = n(),
            eparush = sum(epa)/sum(rushes)) %>%
  arrange(defteam) %>%
  print(n = Inf)

# join tibble 1 and tibble 2
recentdata1 = rushdata %>%
  left_join(passdata, by = "defteam") %>%
  print(n = Inf)

# plot EPA Allowed
recentplot1 = ggplot(data = recentdata1,
                     aes(x = eparush, y = epapass)) +
  geom_hline(yintercept = mean(recentdata1$epapass),            # plot avg EPA/pass Allowed
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(recentdata1$eparush),            # plot avg EPA/rush Allowed
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",                                    # add trend line
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = defteam),                      # replace data points with team logos
                 width = 0.07,
                 alpha = 0.8) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(title = "EPA Per Rush vs. EPA Per Pass Allowed",
       subtitle = "2024 NFL Defenses | Wk 13-16",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA Per Rush",
       y = "EPA Per Pass") +
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
        axis.text = element_text(size = 15))

# view plot
recentplot1

# save plot to local files
ggsave("X post 20 - recent_def_production.png",
       width = 14, height = 10,
       dpi = "retina")