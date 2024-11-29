## load packages
  library(ggplot2)
  library(ggrepel)
  library(nflreadr)
  library(nflfastR)
  library(nflplotR)

## clear cache when starting new R session  
  nflreadr::clear_cache()

## load 2024 NFL data
  data1 = load_pbp(2024)

## before filtering, define group of players to include J.Daniels; C.Williams; D.Maye; B.Nix
  qbrookie = c("00-0039910", "00-0039918", "00-0039851", "00-0039732")
  
## filter data to include weeks 1-12 with the group you just defined
  data24rook = data1 %>%
    filter(week < 13,
           !is.na(epa),
           passer_id %in% qbrookie)%>%
    group_by(passer_id, passer, team = posteam, week)%>%
    summarize(att = n(),
              epa = sum(epa),
              .groups = "drop")%>%
    arrange(passer, week)%>%
    print(n = Inf)
  
## add EPA running total column to the tibble you just created
  data24rook$cumepa = ave(data24rook$epa, data24rook$passer, FUN = cumsum)
  
## define week 6 (used later for plotting headshots)
  week6 = data24rook %>% filter(week == 6)
  
## use ggplot to create the plot
## use 'group = passer' inside ggplot aesthetics to summarize
## use aesthetics inside geom_line and scale_color_manual to attach team color to each line
## use aesthetics inside geom_point to change data point colors
## use prev. created 'week6' inside _headshots command to place player image where x = 6
## manually adjust axis ranges
## use theme() command to format plot title and axis title elements
  plotqbrook1 = ggplot(data = data24rook, aes(x = week, y = cumepa, group = passer))+
    geom_line(aes(color = passer))+
    scale_color_manual(values = c("orange", "navy", "blue", "red"))+
    geom_point(aes(color = passer))+
    geom_nfl_headshots(data = week6, aes(player_gsis = passer_id),
                       height = 0.12)+
    scale_x_continuous(n.breaks = 12)+
    scale_y_continuous(breaks = seq(-45, 110, by = 25))+
    labs(title = "NFL Rookie QB Cumulative EPA By Week",
         subtitle = "2024 Weeks 1-12 (min. 250 att.)",
         x = "Week", y = "Cumulative EPA",
         caption = "By Nick Gasperi | Data @nflfastR")+
    theme_bw()+
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 11),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold", size = 11))
## view the plot
  plotqbrook1
