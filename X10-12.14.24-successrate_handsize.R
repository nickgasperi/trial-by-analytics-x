# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# create hand size tibble
handsize2024 = tibble(
  passer_player_name = c("J.Burrow", "J.Goff", "D.Carr", "B.Mayfield", "P.Mahomes",
               "B.Purdy", "G.Smith", "J.Daniels", "S.Darnold",
               "L.Jackson", "K.Murray", "D.Jones",
               "D.Watson", "J.Hurts", "G.Minshew", "B.Young",
               "C.Williams", "K.Cousins", "T.Tagovailoa", "C.Stroud",
               "T.Lawrence", "J.Herbert", "D.Prescott", "M.Stafford",
               "J.Allen", "A.Rodgers", "R.Wilson", "A.Richardson",
               "J.Love", "W.Levis", "J.Winston", "C.Rush", "B.Nix",
               "D.Maye"),
  handsize = c(9, 9, 9.125, 9.25, 9.25,
               9.25, 9.25, 9.375, 9.375,
               9.5, 9.5, 9.75,
               9.75, 9.75, 10.125, 9.75,
               9.875, 9.875, 10, 10,
               10, 10, 10, 10,
               10.125, 10.125, 10.25, 10.5,
               10.5, 10.625, 9.375, 9.125, 10.125,
               9.125))

# load 2024 NFL data
data93 = load_pbp(2024)

# filter data to only passing plays
# calculate success rate per attempt with summarize()
# filter to only QBs who have thrown at least 200 times
qbsuccrate = data93 %>%
  filter(week < 15,
         play_type == "pass",
         !is.na(success),
         !is.na(passer_player_id),
         !is.na(air_yards)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            sucrate = sum(success)/sum(att)) %>%
  filter(att > 200) %>%
  arrange(att) %>%
  print(n = Inf)

# combine 'qbsuccrate' with previously stored hand size data
hsizedata = qbsuccrate %>%
  left_join(handsize2024, by = "passer_player_name")

# view new dataset
hsizedata %>%
  print(n = Inf)

# the correlation between success rate and hand size is -0.44
cor.test(hsizedata$sucrate, hsizedata$handsize)

# create plot
hsizeplot = ggplot(data = hsizedata,
                   aes(x = sucrate, y = handsize)) +
  geom_nfl_logos(aes(team_abbr = posteam,
                     width = 0.04,
                     alpha = .99)) +
  geom_text_repel(box.padding = 0.3,
                  aes(label = passer_player_name)) +
  geom_hline(yintercept = mean(hsizedata$handsize),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(hsizedata$sucrate),
             linetype = "dashed",
             color = "red") +
  labs(title = "NFL QB Success Rate vs. Hand Size",
       subtitle = "2024 Weeks 1-14 | Passing Plays (min. 200 att.)",
       x = "Success Rate (%)",
       y = "Hand Size (in.)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
  annotate("text",
           label = "Hang In There",
           x = 0.39, y = 9.3,
           size = 7,
           color = "red",
           fontface = "italic") +
  annotate("text",
           label = "Firm Handshake",
           x = 0.39, y = 10.3,
           size = 6,
           color = "red",
           fontface = "italic",
           alpha = 0.65) +
  annotate("text", label = "Congrats",
           x = 0.53, y = 10.25,
           size = 7,
           color = "green",
           fontface = "italic") +
  annotate("text", label = "Happy For You",
           x = 0.55, y = 9.12,
           size = 6,
           color = "green",
           fontface = "italic",
           alpha = 0.65) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 18),
        axis.title = element_text(face = "bold.italic",
                                  size = 16),
        axis.text = element_text(size = 13),
        plot.caption = element_text(size = 12))

# view plot
hsizeplot

# save plot to local files
ggsave("X post 10 - successrate_handsize.png",
       width = 14, height = 10,
       dpi = "retina")
