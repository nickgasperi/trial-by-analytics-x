# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# Part 1: QB Pass TD Rate -------------------------------------------------

# calc QB touchdown rate
qbtdrate = nfldata %>%
  filter(season_type == "REG",
         !is.na(passer_player_id),
         !is.na(touchdown),
         passer_player_id != "00-0031280",    # exclude 2025 non-starters
         passer_player_id != "00-0039152",
         passer_player_id != "00-0031503",         
         play_type == "pass",
         qb_spike == 0) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            tds = sum(touchdown),
            tdrate = tds/att,
            .groups = "drop") %>%
  filter(att >= 300,                # include players with at least 300 pass attempts
         tdrate >= 0.0478) %>%      # include only top 10 td rates      
  arrange(-tdrate) %>%
  print(n = Inf)

# plot td rate
tdrateplot = ggplot(data = qbtdrate,
                    aes(x = reorder(passer_player_id, -tdrate), y = tdrate)) +
  geom_col(position = "dodge",
           aes(color = posteam,
               fill = posteam),
           width = 0.5) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.8) +
  labs(title = "Passing TD Rate - Top 10",
       subtitle = "2024 NFL Regular Season | min. 300 att.",
       y = "Pass TD/Att.",
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
        axis.title.y = element_text(face = "bold.italic",
                                    size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_nfl_headshot(size = 1.8))

# view plot
tdrateplot
  
# save plot to local files
ggsave("X post 61.1 - pass_td_rate.png",
       width = 10.5, height = 7,
       dpi = "retina")

# Part 2: Worst Passing Defenses ------------------------------------------

# calc EPA/pass and total pass TD allowed
passdef = nfldata %>%
  filter(season_type == "REG",
         !is.na(touchdown),
         !is.na(epa),
         play_type == "pass",
         qb_spike == 0) %>%
  group_by(defteam) %>%
  summarize(att = n(),
            epa_pass = sum(epa)/att,
            tds = sum(touchdown),
            tdrate = tds/att) %>%
  arrange(-tds) %>%
  print(n = Inf)

# plot epa/pass allowed vs td rate allowed
passdefplot = ggplot(data = passdef,
                     aes(x = epa_pass, y = tdrate)) +
  geom_hline(yintercept = mean(passdef$tdrate),        # plot avg TD Rate allowed
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(passdef$epa_pass),      # plot avg EPA/pass allowed
             linetype = "dashed",
             color = "red") +
  geom_nfl_logos(aes(team_abbr = defteam),             # replace data points with team logos
               width = 0.07,
               alpha = 0.8) +
  labs(title = "EPA Per Pass Allowed vs. Pass TD Rate Allowed",
       subtitle = "2024 NFL Regular Season Defenses",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Pass Att.",
       y = "TD/Pass Att.") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
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
passdefplot

# save plot to local files
ggsave("X post 61.2 - passing_defenses.png",
       width = 10.5, height = 7,
       dpi = "retina")
