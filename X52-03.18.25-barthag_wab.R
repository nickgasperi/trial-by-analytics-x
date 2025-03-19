# load packages
library(tidyverse)    # data wrangling
library(readxl)       # to import data
library(ggrepel)      # replaces geom_text

# convert dataframe to tibble
kenbart1 = as_tibble(kenbart)


testwab = kenbart1 %>%
  mutate(color1 = ifelse(ROUND == 1, "blue", ifelse(YEAR == 2025, "green", "grey"))) %>%
  mutate(size1 = ifelse(ROUND == 1, 1.5, ifelse(YEAR == 2025, 1.5, 1))) %>%
  mutate(label1 = ifelse(YEAR == 2025, TEAM, "")) %>%
  ggplot(aes(x = BARTHAG, y = WAB)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "red") +
  geom_point(aes(color = color1,
                 size = size1)) +
  geom_text_repel(box.padding = 0.3,
                  max.overlaps = 64,
                  aes(label = label1)) +
  scale_color_identity() +
  labs(title = "2008-2025 NCAA Tournaments - Power Rating vs. Wins Above Bubble",
       subtitle = "blue = prev. champs | green = 2025 teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 17, face = "bold"),
        plot.caption = element_text(size = 11),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15))

# view plot
testwab

# save the plot to the device's local files
ggsave("MM2025 - BARTHAG v. WAB.png",
       width = 14, height = 10, dpi = "retina")  
