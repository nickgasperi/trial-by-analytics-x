# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(gt)
library(gtExtras)
install.packages(sdvplot)

# load data
data26 = load_pbp(2026)

# get adot for all qualifying qbs in 2026 for X post caption remark
adot_list = data26 %>%
  filter(!is.na(passer_player_id),
         !is.na(air_yards),
         qb_spike == 0,
         qb_kneel == 0,
         aborted_play == 0) %>%
  group_by(passer_player_id,
           passer_player_name) %>%
  summarize(att = n(),
            adot = mean(air_yards)) %>%
  filter(att >= 75) %>%
  arrange(-adot) %>%
  print(n = Inf)

# get Mahomes dropbacks from week 9
pm_data_9 = data26 %>%
  filter(play_id != 1995,
         week == 9,
         posteam == "KC",
         passer_player_name == "P.Mahomes",
         qb_kneel == 0,
         qb_spike == 0,
         aborted_play == 0,
         !is.na(air_yards)) %>%
  select(week,
         posteam,
         passer_player_id,
         passer_player_name,
         drive,
         pass_attempt,
         complete_pass,
         passing_yards,
         air_yards,
         touchdown,
         interception,
         epa,
         cpoe,
         play_id) %>%
  mutate(inj_status = ifelse(play_id < 1995,
                            "Before",
                            "After")) %>%
  print(n = Inf)

# create Before vs After injury tibble
pm_summary_9 = pm_data_9 %>%
  group_by(inj_status) %>%
  summarize("Drives" = n_distinct(drive),
            "Comp %" = (sum(complete_pass)/n()) * 100,
            "Pass Att" = n(),
            "Pass Yds" = sum(passing_yards,
                             na.rm = TRUE),
            "Pass Y/A" = sum(passing_yards, na.rm = TRUE) / n(),
            "TDs" = sum(touchdown,
                      na.rm = TRUE),
            "Int" = sum(interception,
                      na.rm = TRUE),

            "ADOT" = mean(air_yards,
                        na.rm = TRUE),
            "EPA/db" = mean(epa,
                          na.rm = TRUE),
            "CPOE" = mean(cpoe,
                          na.rm = TRUE)
            )%>%
  print(n = Inf)

# transpose tibble before creating table with gt
pm_tbl_9 = pm_summary_9 %>%
  pivot_longer(cols = -inj_status,
               names_to = "Stat",
               values_to = "value") %>%
  pivot_wider(names_from = inj_status,
              values_from = value) %>%
  select(Stat, Before, After)

# create table
gt_pm_9 = pm_tbl_9 %>%
  gt(rowname_col = "Stat") %>%
  fmt_integer(columns = c("Before", "After"),
              rows = c("Drives", "Pass Att", "Pass Yds", "TDs", "Int")) %>%
  fmt_number(columns = c("Before", "After"),
              rows = Stat %in% c("Comp %", "Pass Y/A"),
              decimals = 1) %>%
  fmt_number(columns = c("Before", "After"),
              rows = Stat %in% c("ADOT", "EPA/db", "CPOE"),
              decimals = 2) %>%
  tab_header(title = md("**Patrick Mahomes: Before vs. After Injury**"),
             subtitle = md("**2026 NFL Week 9 Passing Performance**")) %>%
  tab_style(style = list(cell_fill(color = "lightgreen"),
                         cell_text(weight = "bold")),
            locations = cells_body(columns = "After",
                                   row = Stat != "int" & After > Before)) %>%
  tab_style(style = list(cell_fill(color = "lightgreen"),
                         cell_text(weight = "bold")),
            locations = cells_body(columns = "Before",
                                   row = Stat != "int" & Before > After)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub()) %>%
  tab_style(style = cell_text(size = px(16)),
            locations = cells_title(groups = "subtitle")) %>%
  tab_style(style = cell_text(size = px(13)),
            locations =cells_footnotes()) %>%
  tab_footnote(footnote = md("**Injured with 1:39 remaining in 2nd Qtr*")) %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | Data @nflfastR")) %>%
  cols_align(align = "center") %>%
  opt_align_table_header(align = "center") %>%
  tab_options(page.margin.left = px(100),
              page.margin.right = px(50))

# view table
gt_pm_9

# save table to local files
gtsave(gt_pm_9,
       filename = "X83-11.09.26-mahomes-wk9-injury-before-after.png",
       vwidth = 1000,
       vheight = 800,
       expand = 30)
