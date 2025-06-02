# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
pbpall = load_pbp(2024)

# create 2 separate tibbles - 1 for rushes and 1 for passes
# after each is created, combine them into a dataframe with left_join()


# filter to include only RUSH plays from week 1 to week 12
# also filter out bad data with !is.na()
# group by team, then add total epa and total yards
# inside summarize(), create epa/play and yards/play
pbp2412rush = pbpall %>%
  filter(week < 13,
         !is.na(posteam),
         !is.na(defteam),
         !is.na(epa),
         !is.na(yards_gained),
         rush == 1) %>%
  group_by(defteam) %>%
  summarize(rushsnaps = n(),
            rushepa = sum(epa),
            rushperplayepa = (rushepa/rushsnaps),
            rushyd = sum(yards_gained),
            rushperplayyd = (rushyd/rushsnaps)) %>%
  arrange(rushperplayepa) %>%
  print(n = Inf)

## repeat the same process, but for PASS plays
pbp2412pass = pbpall %>%
  filter(week < 13,
         !is.na(posteam),
         !is.na(defteam),
         !is.na(epa),
         !is.na(yards_gained),
         pass == 1) %>%
  group_by(defteam) %>%
  summarize(passsnaps = n(),
            passepa = sum(epa),
            passperplayepa = (passepa/passsnaps),
            passyd = sum(yards_gained),
            passperplayyd = (passyd/passsnaps)) %>%
  arrange(passperplayepa) %>%
  print(n = Inf)

# combine these two tibbles using left join
# view data frame
pbp2412_rp = pbp2412rush %>%
  left_join(pbp2412pass, by = "defteam")

# view new tibble
view(pbp2412_rp)

# create a the plot for epa with the dataframe you just created
plot_rpepa_2412 = ggplot(data = pbp2412_rp,
                         aes(x = rushperplayepa, y = passperplayepa)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_hline(yintercept = mean(pbp2412_rp$passperplayepa),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(pbp2412_rp$rushperplayepa),
             linetype = "dashed",
             color = "red") +
  geom_nfl_logos(aes(team_abbr = defteam),
                 width = .06,
                 alpha = .8) +
  annotate("text",
           label = "Flores Zone",
           x = -0.27, y = 0,
           size = 6.5,
           color = "green",
           fontface = "bold.italic") +
  annotate("text",
           label = "Ravens",
           x = -0.2, y = 0.17,
           size = 6.5,
           color = "purple") + 
  annotate("text",
           label = "Solid",
           x = -0.17, y = -0.07,
           size = 6.5,
           color = "green") +
  annotate("text",
           label = "Maybe Next Year",
           x = 0.02, y = 0.25,
           size = 6.5,
           color = "red") + 
  labs(title = "2024 NFL Defenses Weeks 1-12",
       subtitle = "EPA Per Play Allowed - Rushing vs. Passing",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Rush Attempt Allowed",
       y = "EPA/Pass Attempt Allowed") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(face = "bold",
                                     hjust = 0.5,
                                     size = 14),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 12),
        plot.caption = element_text(size = 10))

# view plot
plot_rpepa_2412

# export the plot
# use the Files tab in the bottom right pane of RStudio to open and save image to desired file location
ggsave("X Post 6 pt1 - epa allowed wk1-12.png",
       width = 14, height = 10,
       dpi = "retina")

# recreate the plot, but use yards/play this time
plot_rpyd_2412 = ggplot(data = pbp2412_rp,
                         aes(x = rushperplayyd, y = passperplayyd)) +
  labs(title = "2024 NFL Defenses Weeks 1-12",
       subtitle = "Yards Per Play Allowed - Rushing vs. Passing",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Yards/Rush Attempt Allowed",
       y = "Yards/Pass Attempt Allowed") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_hline(yintercept = mean(pbp2412_rp$passperplayyd),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(pbp2412_rp$rushperplayyd),
             linetype = "dashed",
             color = "red") +
  geom_nfl_logos(aes(team_abbr = defteam),
                 width = .06,
                 alpha = .8) +
  annotate("text",
           label = "Great",
           x = 3.5, y = 5.2,
           size = 6.5,
           color = "green",
           fontface = "bold") +
  annotate("text",
           label = "Solid",
           x = 3.92, y = 5.15,
           size = 6.5,
           color = "green") +
  annotate("text",
           label = "Vulnerable",
           x = 3.3, y = 6,
           size = 6.5,
           color = "orange") +
  annotate("text",
           label = "Serviceable",
           x = 4.5, y = 5.4,
           size = 6.5,
           color = "orange") +
  annotate("text",
           label = "Unreliable",
           x = 4.8, y = 6.7,
           size = 6.5,
           color = "red") +
  annotate("text",
           label = "Fire Doug",
           x = 3.93, y = 7.06,
           size = 6.5,
           color = "red",
           fontface = "bold") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(face = "bold",
                                     hjust = 0.5,
                                     size = 14),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 12),
        plot.caption = element_text(size = 10))

# view plot
plot_rpyd_2412

# save plot to local files
ggsave("X Post 6 pt2 - yds allowed wk1-12.png",
       width = 14, height = 10,
       dpi = "retina")
