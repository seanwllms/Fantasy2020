library(tidyverse)
library(readxl)
library(lubridate)

#Load coefficients for marginal stats, as well as replacement values
load("coefs.rda")
replacement_hitters <- readxl::read_xlsx("./replacement/replacement_hitters.xlsx", sheet = "replacement_hitters") %>%
  mutate(Position = case_when(
    Position == "UTIL" ~ "DH",
    TRUE ~ toupper(Position))) %>% 
  rename(R = Runs) %>% 
  rename_at(vars(-Position), function(x) paste0(x, "_repl"))

draft <- read_xlsx("./old_draft_analysis/draftpicks.xlsx") %>% 
  mutate(drafted = ymd(drafted),
          kept = ifelse(drafted < mdy("1/1/2019"), TRUE, FALSE))
#########################
##### READ DRAFT REZ ####
#########################


hitters2019 <- read_csv("./old_draft_analysis/hitters.csv") %>% 
  select(Name, PA, R, HR, RBI, SB, AVG)

pitchers2019 <- read_csv("./old_draft_analysis/pitchers.csv") %>%
  mutate(WHIP = (H+BB)/IP) %>% 
  select(Name, IP, W, SV, SO, ERA, WHIP)


#########################
##### HITTERS  HERE #####
#########################

#merge in 2019 results for each pick, merge in replacment levels.
hitter_results <- left_join(draft, hitters2019, by =c("player" ="Name")) %>% 
  left_join(replacement_hitters, by = c("position" = "Position")) %>%
  filter(!(player %in% pitchers2019$Name)) %>% 
  mutate(
    mrg_r = R - R_repl,
    mrg_hr = HR - HR_repl,
    mrg_rbi = RBI - RBI_repl,
    mrg_sb = SB - SB_repl,
    mrg_avg = AVG - AVG_repl,
    marginal_points = mrg_r * coefs$estimate[5] +
                      mrg_hr * coefs$estimate[3] +
                      mrg_sb * coefs$estimate[7] +  
                      mrg_rbi * coefs$estimate[6] +
                      mrg_avg * coefs$estimate[1]/15,
    dollar_value = marginal_points*4680/1700 + 2,
    equity = dollar_value - salary
  )

hitter_value_targets_cost <- hitter_results %>% 
  filter(!kept) %>% 
  group_by(salary) %>% 
  summarise(mean_equity = mean(equity, na.rm = TRUE),
            equity_sd = sd(equity,  na.rm = TRUE)) 


hitter_value_graph_cost <- hitter_results %>% 
  filter(!kept) %>% 
  ggplot(aes(x=factor(salary), y = equity)) +
  geom_point() +
  gemo

hitter_value_graph_cost

hitter_value_targets_day <- hitter_results %>% 
  mutate(drafted = lubridate::ymd(drafted)) %>% 
  filter(!kept & position != "B") %>% 
  ggplot(aes(x=drafted, y = equity)) +
  geom_point() +
  geom_smooth()

hitter_value_targets_day

#########################
##### PITCHERS HERE #####
#########################

replacement_pitchers <- readxl::read_xlsx("./replacement/replacement_pitchers.xlsx",
                                         sheet="replacement") %>% 
  select(-IP) %>% 
  mutate(Position = "P") %>% 
  rename_at(vars(-Position), function(x) paste0(x, "_repl"))


#merge in 2019 results for each pick, merge in replacment levels.
pitcher_results <- left_join(draft, pitchers2019, by =c("player" ="Name")) %>% 
  left_join(replacement_pitchers, by = c("position" = "Position")) %>%
  filter(player %in% pitchers2019$Name) %>% 
  mutate(
    mrg_era = ERA - ERA_repl,
    mrg_whip = WHIP - WHIP_repl,
    mrg_k = SO - K_repl,
    mrg_w = W - W_repl,
    mrg_sv = SV - SV_repl,
    marginal_points = mrg_era * coefs$estimate[2]*(IP/1464) +
      mrg_whip * coefs$estimate[10]*(IP/1464) +
      mrg_k * coefs$estimate[4] +  
      mrg_w * coefs$estimate[9] +
      mrg_sv * coefs$estimate[8],
    dollar_value = marginal_points*4680/1700 + 3,
    equity = dollar_value - salary
  )

pitcher_value_targets <- pitcher_results %>% 
  filter(!kept) %>% 
  group_by(salary) %>% 
  summarise(mean_equity = mean(equity, na.rm = TRUE),
            equity_sd = sd(equity,  na.rm = TRUE)) 


pitcher_value_graph <- pitcher_results %>% 
  filter(!kept) %>% 
  ggplot(aes(x=factor(salary), y = equity)) +
  geom_boxplot()

pitcher_value_graph

pitcher_value_graph_day <- pitcher_results %>% 
  filter(!kept & position != "B") %>% 
  mutate(drafted = lubridate::ymd(drafted)) %>% 
  ggplot(aes(x=drafted, y = equity)) +
  geom_point() +
  geom_smooth(span = 5)

pitcher_value_graph_day


scatterplot_hit <- ggplot(hitter_results, aes(x=salary,y=equity)) +
  geom_point() +
  geom_smooth(span = 3)

scatterplot_hit

scatterplot_pitch <- ggplot(pitcher_results, aes(x=salary,y=equity)) +
  geom_point() +
  geom_smooth()

scatterplot_pitch
