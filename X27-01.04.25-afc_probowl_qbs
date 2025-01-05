# load packages
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(nflfastR)
  library(nflplotR)
  library(nflreadr)

# load data
  nfldata1 = load_pbp(2022:2024)

# pull qb seasons
  afcqb1 = nfldata1 %>%
    filter(season == 2024 & id == "00-0034857" | season == 2024 & id == "00-0034796" | season == 2024 & id == "00-0036442" |
           season == 2023 & id == "00-0033873" | season == 2023 & id == "00-0034796" | season == 2023 & id == "00-0035289" |
           season == 2023 & id == "00-0039163" | season == 2023 & id == "00-0036212" | season == 2022 & id == "00-0033873" |
           season == 2022 & id == "00-0036442" | season == 2022 & id == "00-0034857" | season == 2022 & id == "00-0036971" |
           season == 2022 & id == "00-0031280" | season == 2022 & id == "00-0035993",
           season_type == "REG",
           play_type == "pass",
           qb_spike == 0,
           !is.na(epa),
           !is.na(air_yards)
           ) %>%
    group_by(id,
             name,
             posteam,
             season) %>%
    summarize(att = n(),
              passyds = sum(yards_gained),
              totepa = sum(epa),
              epa = sum(epa)/sum(att),
              ypa = sum(yards_gained)/sum(att),
              tds = sum(touchdown),
              tdrate = sum(touchdown)/sum(att),
              .groups = "drop") %>%
    print(n = Inf)

# create extra column to combine name and season
  afcqb1$qbyr = paste(afcqb1$name, afcqb1$season, sep = ", ")

# view new tibble
  afcqb1

# create plots
# only one plot is included below - to create more, just change variables on axes
# then change axis titles, plot title, etc.
  afcqbplot1 = ggplot(data = afcqb1, aes(x = epa, y = passyds)) +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.06, alpha = 0.8) +
    geom_text_repel(box.padding = 1.0, aes(label = qbyr, color = posteam, fontface = "bold.italic")) +
    scale_color_nfl(type = "primary") +
    labs(title = "NFL AFC Pro Bowl Quarterbacks | 2022-2024",
         subtitle = "EPA Per Attempt vs. Total Passing Yards",
         x = "EPA", y = "Passing Yards",
         caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#F0F0F0"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 17),
          plot.caption = element_text(size = 11),
          axis.title = element_text(face = "bold", size = 15),
          axis.text = element_text(size = 13)
          )
  
# view plot
  afcqbplot1

# save plot
  ggsave("X post 27 - probowl_qb_afc.png",
         width = 10.5, height = 7, dpi = "retina")
