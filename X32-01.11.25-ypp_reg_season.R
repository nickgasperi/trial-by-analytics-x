# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)

# load data
  data3 = load_pbp(2024)

# calculate Off Yd/Play
# use renmae() to change 'posteam' to 'team' for future join
  offdata11 = data3 %>%
    filter(play_type == "run" | play_type == "pass",
           !is.na(posteam),
           !is.na(yards_gained),
           qb_kneel == 0, qb_spike == 0) %>%
    group_by(posteam) %>%
    summarize(offplays = n(),
              offyds = sum(yards_gained),
              offypp = offyds/offplays) %>%
    arrange(-offyds) %>%
    rename(team = posteam) %>%
    print(n = Inf)

# calculate Def Yd/Play
# use renmae() to change 'posteam' to 'team' for future join
  defdata11 = data3 %>%
    filter(play_type == "run" | play_type == "pass",
           !is.na(defteam),
           !is.na(yards_gained),
           qb_kneel == 0, qb_spike == 0) %>%
    group_by(defteam) %>%
    summarize(defplays = n(),
              defyds = sum(yards_gained),
              defypp = defyds/defplays) %>%
    arrange(defyds) %>%
    rename(team = defteam) %>%
    print(n = Inf)

# join tibbles
  teamyds1 = offdata11 %>%
    left_join(defdata11, by = "team") %>%
    print(n = Inf, width = Inf)

# set playoff teams for future viz
  playoffteams = c("BAL", "BUF", "DEN", "DET", "GB", "HOU", "KC",
                  "LA", "LAC","MIN", "PHI", "PIT", "TB", "WAS")

# plot data
  teamydplot1 = teamyds1 %>%
    mutate(color2 = ifelse(team %in% playoffteams, NA, "b/w")) %>%
    ggplot(aes(x = defypp, y = offypp)) +
    scale_x_reverse() +
    geom_hline(yintercept = mean(teamyds1$offypp), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(teamyds1$defypp), linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_nfl_logos(aes(team_abbr = team, color = color2), width = 0.07, alpha = 0.8) +
    scale_color_identity() +
    labs(title = "Opponent Yards Per Play vs. Offensive Yards Per Play",
         subtitle = "2024 NFL Reg Season",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
         x = "YPP Allowed",
         y = "Offensive YPP") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#F0F0F0"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18),
          axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(size = 13),
          plot.caption = element_text(size = 12))

# view plot
  teamydplot1

# save plot
  ggsave("X post 32 - ypp_regseason.png",
         width = 10.5, height = 7, dpi = "retina")
