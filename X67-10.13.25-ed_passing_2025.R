# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2025 pbp data
nfldata25 = load_pbp(2025)

# create tibble 1 for early down success rate and count of plays by team
ed_p_data = nfldata25 %>%
  filter(!is.na(success),
         !is.na(posteam), 
         play_type == "pass",
         down == 1 | down == 2,
         qb_spike == 0,
         sack == 0) %>%
  group_by(posteam) %>%
  summarize(early_att = n(),
            succ_rate = sum(success)/early_att) %>%
  arrange(posteam) %>%
  print(n = Inf)

# create tibble 2 to fetch count of total early down plays by team
tot_ed_data = nfldata25 %>%
  filter(!is.na(posteam),
         play_type == "run" | play_type == "pass",
         sack == 0,
         qb_kneel == 0,
         qb_spike == 0) %>%
  group_by(posteam) %>%
  summarize(tot_plays = n()) %>%
  arrange(posteam) %>%
  print(n = Inf)

# join tibbles
all_ed_data = ed_p_data %>%
  left_join(tot_ed_data, by = "posteam") %>%
  print(n = Inf)

# add column to calc early down pass rate
all_ed_data$ed_pass_rate = all_ed_data$early_att/all_ed_data$tot_plays

# view final tibble
all_ed_data %>%
  print(n = Inf)

# plot early down pass rate vs. success rate
ed_pass_plot = ggplot(data = all_ed_data,
                      aes(x = ed_pass_rate,
                          y = succ_rate)) +
  geom_vline(xintercept = mean(all_ed_data$ed_pass_rate),
             linetype = "dashed",
             color = "red",
             alpha = 0.5) +
  geom_hline(yintercept = mean(all_ed_data$succ_rate),
             linetype = "dashed",
             color = "red",
             alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "darkgrey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06,
                 alpha = 0.70) +
  labs(title = "Early Down Passing - Frequency vs. Success Rate",
       subtitle = "2025 NFL Wk 1-6 | 1st & 2nd Down",
       x = "Pass Rate",
       y = "Success Rate",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 13),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_text(size = 14))


# view plot
ed_pass_plot

# save plot to local files
ggsave("X post 67 - ed_passing_2025.png",
       width = 10.5, height = 7,
       dpi = "retina")  