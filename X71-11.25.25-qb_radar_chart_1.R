# install packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(radarchart)
library(fmsb)
library(scales)

# load 2025 pbp data
nfldata25 = load_pbp(2025)

# load tibble with 2025 passing data
trialdata25 = nfldata25 %>%
  filter(!is.na(passer_player_name),
         !is.na(air_yards)) %>%
  group_by(passer_player_name) %>%
  summarize(att = n(),
            passyards = sum(yards_gained),
            adot = mean(air_yards),
            passtds = sum(touchdown),
            comppct = sum(complete_pass)/sum(att)) %>%
  filter(att > 150) %>%
  print(n = 11)

# transform tibble to dataframe, then make first column values the row labels)
as.data.frame(trialdata25)
trialdata25_a = column_to_rownames(trialdata25,
                                   var = "passer_player_name")

# view new dataframe
head(trialdata25_a)

# add 2 rows to dataset - one for max values and another for min values of each column
# radarchart() uses these to define limits of charting area
trialdata25_b = rbind(max = apply(trialdata25_a,
                                2,
                                max),
                    min = apply(trialdata25_a,
                                2,
                                min),
                    trialdata25_a)

# view updated dataframe
head(trialdata25_b)

# rename columns for plot
trialdata25_b = trialdata25_b %>%
  rename('Attempts' = att,
         'Pass Yards' = passyards,
         'ADOT' = adot,
         'Pass TDs' = passtds,
         'Comp %' = comppct)

# select 3 players to start with in a new dataframe, then rerun the radarchart with the new filtered dataframe
trialdata25_select = trialdata25_b[c("max",
                                     "min",
                                     "A.Rodgers",
                                     "B.Mayfield",
                                     "B.Nix"), ]
# view dataframe
trialdata25_select

# the order for png() differs from ggsave()
## write png() then create the image then close graphics with dev.off() to write the file
png("X Post 71 - qb_radar_chart_1.png",
    width = 7, height = 7,
    units = "in",
    res = 300)

# chart data
radarchart(trialdata25_select,
           pcol = c("black", "red3", "orange2"),
           plwd = 2,
           plty = 1,
           pfcol = alpha(c("black", "red3", "orange2"), 0.15),
           cglcol = "grey",
           cglty = 1)

# add legend to chart
legend(x = "bottom",
       horiz = TRUE,
       legend = rownames(trialdata25_select)[-c(1,2)],
       col = c("black", "red3", "orange2"),
       lwd = 4,
       lty = 1,
       bty = "n",
       text.col = "grey60")

mtext("QB Base Stat Comparison",
      side = 3,
      adj = 0,
      line = 2,
      cex = 1.4,
      font = 2)
mtext("2025 NFL Wk 1-12",
      side = 3,
      adj = 0,
      line = 1,
      cex = 1.1,
      font = 2)
mtext("By Nick Gasperi | @tbanalysis | data @nflfastR",
      side = 1,
      adj = 1,
      line = 3,
      cex = 0.95,
      font = 1)

# close graphics -> write file
dev.off()