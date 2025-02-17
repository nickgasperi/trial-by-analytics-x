##load packages
  library(ggplot2)
  library(ggrepel)
  library(nflreadr)
  library(nflfastR)
  library(nflplotR)

## clear cache when starting new R session  
  nflreadr::clear_cache()

## load NFL data from 2018 to 2024
pbpallpass = load_pbp(2018:2024)

## filter data to include only passing plays
setpass1 = pbpallpass %>%
  filter(play_type == "pass", !is.na(air_yards))
  
## filter data to include only passes thrown by Patrick Maomes in the regular season
## group by passer and season
## summarize to create data points - best practice to add Mahomes' playerid to protect from duplicate names
## drop groups at end to avoid warning message
mahomesallpass = setpass1 %>%
  filter(passer == "P.Mahomes",
         season_type == "REG"
         )%>%
  group_by(passer, season)%>%
  summarize(playergsid = "00-0033873",
            ratiotdint = (sum(touchdown)/sum(interception)),
            td = sum(touchdown),
            int = sum(interception),
            .groups = "drop")
            
## use ggplot to create plot
## use geom_nfl_headshots to add player image to each data point
## define color as red within geom_line to match team color
## use theme() command to format plot title elements
mahomesratioplot = ggplot(data = mahomesallpass, 
                          aes(x = season, y = ratiotdint))+
  geom_nfl_headshots(aes(player_gsis = "00-0033873"), height = .1)+
  labs(title = "Mahomes Touchdown to Interception Ratio By Year",
       subtitle = "2018 - 2024",
       x = "Season",
       y = "TD:Int Ratio")+
  geom_line(color = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 12))
## view the plot
mahomesratioplot



