# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(radarchart)
library(fmsb)
library(scales)

# load 2025 data
nfldata25 = load_pbp(2025)

# generate 3 tibbles - 1 for passing stats, 1 for cpoe, and 1 for rushing epa - join 2nd and 3rd tibbles to 1st before charting visual
# cpoe requires separate tibble due to NAs - need to use slightly more filtered dataset to calculate
# create passing tibble first, as this will define the group that will appear on the chart
# despite focusing on only 2 QBs, charting group needs to include all qualified QBs to define max/min values (edges) of chart
# convert final tibble to dataframe prior to charting

# tibble 1 - passing ----------------------------------------------------------
# generate passing stats tibble
# 'qualified' QBS = at least 140 attempts
qb_pass_25 = nfldata25 %>%
  filter(week < 13,
         qb_spike == 0,
         !is.na(passer_player_id),
         !is.na(air_yards)) %>%
  group_by(passer_player_id,
           passer_player_name) %>%
  summarize(att = n(),
            epa_att = sum(epa)/sum(att),
            suc_rate = sum(success)/sum(att),
            td_rate = sum(touchdown)/sum(att),
            adot = sum(air_yards)/sum(att)) %>%
  filter(att >= 140) %>%
  print(n = Inf)


# tibble 2 - cpoe ----------------------------------------------------------
# generate tibble for cpoe only
# using !is.na(cpoe) will filter further than tibble 1, but same final filter() works to get same qb list
qb_cpoe_25 = nfldata25 %>%
  filter(week < 13,
         qb_spike == 0,
         !is.na(passer_player_id),
         !is.na(cpoe)) %>%
  group_by(passer_player_id,
           passer_player_name) %>%
  summarize(att = n(),
            CPOE = sum(cpoe)/att) %>%
  filter(att >= 140) %>%
  print(n = Inf)

# new tibble to drop att column
qb_cpoe_25_a = qb_cpoe_25 %>%
  select(passer_player_id,
         passer_player_name,
         CPOE)

# join tibbles 1 & 2
qb_passing_25 = qb_pass25 %>%
  left_join(qb_cpoe_25_a, by = c("passer_player_id",
                                 "passer_player_name"))

# view updated df
# the left_join() function auto converted to df
# i will convert back to tibble bc I am unsure if joining a future tibble to an existing df would present an issue - i always default to tibbles, though this may be overkill
qb_passing_25

# convert to tibble
qb_passing_25_t = as.tibble(qb_passing_25)

# view tibble
qb_passing_25_t

# tibble 3 - rushing ----------------------------------------------------------
# generate rushing stats tibble
# going to define the previous list of qbs separately, then use %in% to reference that list in the rush tibble
# this avoids generating a big list of rushers we don't need - not a huge concern here with likely a few hundred rushers, but best practice
qb_list_a = qb_passing_25_t$passer_player_id

qb_rush_25 = nfldata25 %>%
  filter(week < 13,
         qb_kneel == 0,
         !is.na(rusher_player_id),
         !is.na(epa),
         rusher_player_id %in% qb_list_a) %>%
  group_by(rusher_player_id,
           rusher_player_name) %>%
  summarize(tot_rush_epa = sum(epa)) %>%
  print(n = Inf)

# drop the rush attempts col
qb_rush_25_t = qb_rush_25 %>%
  select(rusher_player_id,
         rusher_player_name,
         tot_rush_epa)

# join tibbles ----------------------------------------------------------
# to join the 2 tibbles, the columns with matching data must also have matching col titles
# so rename cols from each original tibble, then join
pass_join = qb_passing_25_t %>%
  rename(qb_id = passer_player_id,
         qb_name = passer_player_name)

rush_join = qb_rush_25_t %>%
  rename(qb_id = rusher_player_id,
         qb_name = rusher_player_name)

# join
# drop the qb_id since radar charts only allow for one label
# also drop att bc it won't be included in the chart
qb_chart_data = pass_join %>%
  left_join(rush_join,
            by = "qb_name") %>%
  select(qb_name,
         epa_att,
         suc_rate,
         td_rate,
         adot,
         CPOE,
         tot_rush_epa)

# view tibble
qb_chart_data


# chart data --------------------------------------------------------------
# need to first convert tibble to df, then add max/min values before charting
qb_chart_data_df = as.data.frame(qb_chart_data)

# view df
qb_chart_data_df

# move qb_name to row title
qb_chart_data_df = column_to_rownames(qb_chart_data_df,
                                   var = "qb_name")

qb_chart_data_final = rbind(max = apply(qb_chart_data_df,
                                  2,
                                  max),
                            min = apply(qb_chart_data_df,
                                  2,
                                  min),
                            qb_chart_data_df)

# view df
qb_chart_data_final

# select figures for charting
qb_chart_data_select = qb_chart_data_final[c("max",
                                             "min",
                                             "M.Stafford",
                                             "D.Maye"), ]

# rename columns
qb_chart_data_select_a = qb_chart_data_select %>%
  rename("EPA/Att." = epa_att,
         "Success Rate" = suc_rate,
         "TD Rate" = td_rate,
         "ADOT" = adot,
         "CPOE" = CPOE,
         "Tot. Rush EPA" = tot_rush_epa)

# write png file
png("X Post 72 - qb_radar_chart_2.png",
    width = 7, height = 7,
    units = "in",
    res = 300)

# chart data
radarchart(qb_chart_data_select_a,
           pcol = c("#003594", "#C60C30"),
           plwd = 2,
           plty = 1,
           pfcol = alpha(c("#003594", "#C60C30"),
                         0.20),
           cglcol = "grey",
           cglty = 1)

# add legend to chart
legend(x = "bottomleft",
       legend = rownames(qb_chart_data_select_a)[-c(1,2)],
       col = c("#003594", "#C60C30"),
       lwd = 4,
       lty = 1,
       bty = "n",
       text.col = "grey40")

mtext("2025 MVP Favorites Comparison",
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