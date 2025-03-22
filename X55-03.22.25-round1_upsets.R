library(tidyverse)        # data wrangling
library(ggrepel)

# define winning teams in upset games
upsetR1 = c('1142', '1140', '1136', '1134',
            '1133', '1116', '1106')

# define losing teams in upset games
lostR1 = c('1137', '1123', '1119', '1118', '1115',
           '1111', '1110')

# plot power rankings
# geom points and text colored green for winners and red for losers
upsetR1a = kenbart2025 %>%
  mutate(color3 = ifelse(`TEAM NO` %in% upsetR1, "darkgreen", ifelse(`TEAM NO` %in% lostR1, "red", "grey"))) %>%
  mutate(label3 = ifelse(`TEAM NO` %in% upsetR1, TEAM, ifelse(`TEAM NO` %in% lostR1, TEAM, ""))) %>%
  ggplot(aes(x = BARTHAG, y = `KADJ EM`)) +
  geom_hline(yintercept = mean(kenbart2025$`KADJ EM`),
             linetype = "dashed") +
  geom_vline(xintercept = mean(kenbart2025$BARTHAG),
             linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE,
              color = "grey") +
  geom_point(aes(color = color3)) +
  geom_text_repel(max.overlaps = 14,
                  aes(label = label3,
                      color = color3),
                  fontface = "bold",
                  size = 5) +
  scale_color_identity() +
  labs(title = "2025 NCAA MBB Tournament - Round 1 Upsets",
       subtitle = "Barttorvik Power Rating vs. Kenpom Net Adj. Efficiency",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold", size = 18),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic", size = 15),
        axis.text = element_text(size = 15))

# view plot
upsetR1a

# save the plot to the device's local files
ggsave("MM2025 - R1 Upsets.png",
       width = 14, height = 10, dpi = "retina") 
