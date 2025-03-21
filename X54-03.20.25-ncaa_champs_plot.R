# load packages
library(tidyverse)    # data wrangling
library(readxl)       # to import data
library(ggrepel)      # replaces geom_text

# plot data
champsplot1 = kenbart1 %>%
  mutate(color1 = ifelse(ROUND == 1, "blue", "grey")) %>%     # highlights only national champs
  mutate(size1 = ifelse(ROUND == 1, 1.5, 1)) %>%              # increases point size for only champs
  mutate(label1 = ifelse(ROUND == 1, YEAR, "")) %>%           # applies season label for onyl champs
  ggplot(aes(x = `3PT%`, y = `FT%`)) +
  geom_hline(yintercept = mean(`FT%`),
             linetype = "dashed") +
  geom_vline(xintercept = mean(`3PT%`),
             linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE,
              color = "red") +
  geom_point(aes(color = color1,
                 size = size1)) +
  geom_text_repel(box.padding = 1.2,
                  max.overlaps = Inf,
                  size = 7,
                  aes(label = label1,
                      fontface = "bold.italic")) +
  scale_color_identity() +
  labs(title = "NCAA National Champions - 3PT % vs. FT %",
       subtitle = "2008-2025 Tournaments",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "3PT %", y = "Free Throw %") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 11),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15))

# view the plot
champsplot1

# save the plot to the device's local files
ggsave("NCAA Champs - 3Pt v. FT.png",
       width = 14, height = 10, dpi = "retina")  
