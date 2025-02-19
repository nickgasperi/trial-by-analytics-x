# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(ggplot2)

# load 2024 NFL data
data924 = load_pbp(2024)

# filter data to include passing and rushing plays from week 1 to week 12
dataoff = data924 %>%
  filter(week < 13,
         !is.na(posteam),
         !is.na(yards_gained),
         !is.na(epa),
          (rush == 1 | pass == 1)) %>%
  group_by(posteam) %>%
  summarize(teamyds = sum(yards_gained),
            teamepa = sum(epa)) %>%
  print(n = Inf)

# use ggplot to create the plot
# use geom_vline and geom_hline to plot average on each axis
# use geom_smooth to add line of best fit
# use geom_nfl_logos to add team logo to each data point
plotrawoff = ggplot(data = dataoff, aes(x = teamyds, y = teamepa))+
  geom_vline(xintercept = mean(dataoff$teamyds), linetype = "dashed", color = "red")+
  geom_hline(yintercept = mean(dataoff$teamepa), linetype = "dashed", color = "red")+
  geom_smooth(method = "lm", se = FALSE, color = "grey")+
  scale_y_continuous(breaks = seq(-100, 200, by = 50))+
  geom_nfl_logos(aes(team_abbr = posteam), width = .07, alpha = .8)+
  annotate("text", x = 4100, y = -65, label = "Fake contenders?", size = 5, color = "red", alpha = 0.5)+
  annotate("text", x = 3450, y = -130, label = "Hopeless", size = 5, color = "red")+
  annotate("text", x = 3300, y = 15, label = "Has potential?", size = 5, color = "green", alpha = 0.5)+
  annotate("text", x = 4100, y = 140, label = "Talented & effective", size = 5 , color = "green")+
  annotate("text", x = 5050, y = 130, label = "Ravens", size = 5, color = "purple")+
  labs(title = "Raw Offensive Production ", subtitle = "2024 NFL Weeks 1-12",
       x = "Offensive Yards Gained", y = "Cumulative Offensive EPA",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"), size = 12)

# view plot
plotrawoff