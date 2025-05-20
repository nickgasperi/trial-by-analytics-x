# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data
# change play type to binary variable to calculate pass rate
latedown = nfldata %>%
  filter(week == 20,
         play_type == "run" | play_type == "pass",
         down == 3 | down == 4,
         qb_spike == 0, qb_kneel == 0,
         !is.na(success)) %>%
  mutate(play_type = recode(play_type,
                            "run" = 0, "pass" = 1)) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            sucrate = sum(success)/sum(plays),
            passrate = sum(play_type)/sum(plays)) %>%
  print(n = Inf)

# plot data
latedownp1 = ggplot(data = latedown,
                    aes(x = passrate, y = sucrate)) +
  geom_hline(yintercept = mean(latedown$sucrate),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(latedown$passrate),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",       # add trend line
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.08,
                 alpha = 0.8) +
  labs(title = "Late Down Pass Rate vs. Success Rate",
       subtitle = "2024 NFL Divisional Round",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Pass Rate",
       y = "Success Rate") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_text(size = 15))

# view plot
latedownp1

# save plot to local files
ggsave("X post 44 - latedown_pass_succ.png",
       width = 10.5, height = 7,
       dpi = "retina")
