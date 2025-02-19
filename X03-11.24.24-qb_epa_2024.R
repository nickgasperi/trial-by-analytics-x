# load packages
library(tidyverse)
library(ggplot2)
library(nflfastR)
library(nflplotR)

# load data
data24 = load_pbp(2024)

# filter data
qbepa = data24%>%
  filter(week < 12,
         play_type == "pass",
         !is.na(air_yards),
         !is.na(epa)) %>% 
  group_by(passer,
           team = posteam) %>%
  summarize(att = n(),
            epa = mean(epa),
            .groups = "drop") %>%
  filter(att > 158) %>%
  arrange(att) %>%
  print(n = Inf)

# plot data
plotqbepa1 = ggplot(data = qbepa, aes(x = epa, y = reorder(passer,epa)))+
  geom_col(aes(color = team, fill = team), width = .6)+
  scale_color_nfl(type = "secondary")+
  scale_fill_nfl(alpha = .7)+
  geom_nfl_logos(aes(team_abbr = team), width = 0.033, alpha = 0.75)+
  labs(title = "2024 QB EPA Per Attempt",
       subtitle = "Thru Week 11 (min. 158 att.)",
       x = "EPA Per Attempt",
       y = "",
       caption = "By Nick Gasperi | Data @nflfastR")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size =15),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold", size = 10))

# view plot
plotqbepa1


