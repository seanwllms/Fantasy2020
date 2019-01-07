batter_positions <- c("C1","C2","1B","2B","SS","3B","CI","MI","OF1","OF2","OF3","OF4","OF5","OF6","DH")
pitcher_positions <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")

#read in replacement level hitters
replacement_hitters <- readxl::read_xlsx("./replacement/replacement_hitters.xlsx") %>% 
  rename(R = Runs)


replacement_hitters$Position <- c("C",
                                  "1B",
                                  "2B",
                                  "SS",
                                  "3B",
                                  "MI",
                                  "CI",
                                  "OF",
                                  "DH"
)

replacement_hitters$AB <- c(400)

#isolate repeated positions
replacement_catcher <- filter(replacement_hitters, Position == "C")
replacement_of <- filter(replacement_hitters, Position == "OF")

merge_repeats <- function(position, times_appears) {
  
  for (i in seq(1,times_appears)) {
   data_to_merge <- get(position)
   pos_number <- data_to_merge %>% pull(Position) %>% paste0(i)
   data_to_merge <- mutate(data_to_merge, Position = pos_number) 

   replacement_hitters <<- rbind(replacement_hitters, data_to_merge) 
  }
}

merge_repeats("replacement_of", 6)

merge_repeats("replacement_catcher",2)


#read in replacement level pitchers
replacement_pitcher <- readxl::read_xlsx("./replacement/replacement_pitchers.xlsx",
                                         sheet="replacement")


#####################################################################
#############MERGE IN PROJECTIONS FOR EACH PLAYER AND TEAM###########
#####################################################################

#create function to grab replacement level hitter stat based on position
get_replacement_hitter <- function(position, stat) {
  
  quo_stat <- enquo(stat)
  

  replacement <- filter(replacement_hitters, Position == position) %>% 
    select(!!quo_stat)

  pull(replacement, !!quo_stat)
}

#create function to grab replacement level pitcher stat based on position
get_replacement_pitcher <- function(stat) {
  
  quo_stat <- enquo(stat)
  
  
  replacement <- replacement_pitcher %>% 
    select(!!quo_stat)
  
  pull(replacement, !!quo_stat)
}



for (team in teams) {
      
      temp <- get(team)
            
      #separate hitters and pitchers
      hitters <- filter(temp, roster_spot %in% batter_positions)
      pitchers <- filter(temp, roster_spot %in% pitcher_positions)
      
      #merge in projections
      hitters <- left_join(hitters, hitter_projections, by = "Name")
      pitchers <- left_join(pitchers, pitcher_projections, by = "Name")
      
      open_hitters <- hitters %>% filter(Name == "") %>% 
        pull(roster_spot)
      
      hitters <- hitters %>% 
        mutate(R = ifelse(roster_spot %in% open_hitters, 
                          get_replacement_hitter(roster_spot, R),
                          R), 
               HR = ifelse(roster_spot %in% open_hitters, 
                          get_replacement_hitter(roster_spot, HR),
                          HR), 
               RBI = ifelse(roster_spot %in% open_hitters, 
                           get_replacement_hitter(roster_spot, RBI),
                           RBI), 
               SB = ifelse(roster_spot %in% open_hitters, 
                           get_replacement_hitter(roster_spot, SB),
                           SB), 
               AVG = ifelse(roster_spot %in% open_hitters, 
                           get_replacement_hitter(roster_spot, AVG),
                           AVG), 
               AB = ifelse(roster_spot %in% open_hitters, 
                           get_replacement_hitter(roster_spot, AB),
                           AB))
      
      pitchers <- pitchers %>%
        mutate(ERA = ifelse(Name=="", get_replacement_pitcher(ERA), ERA),
               WHIP = ifelse(Name=="", get_replacement_pitcher(WHIP), WHIP),
               SV = ifelse(Name=="", get_replacement_pitcher(SV), SV),
               W = ifelse(Name=="", get_replacement_pitcher(W), W),
               K = ifelse(Name=="", get_replacement_pitcher(K), K),
               IP = ifelse(Name=="", get_replacement_pitcher(IP), IP))

      
      #merge hitters and pitchers
      temp <- bind_rows(hitters, pitchers) %>% 
        map_at(c("R", "RBI", "HR", "SB", "K", "SV", "W", "IP"), round, 1) %>% 
        map_at(c("ERA", "WHIP"), round, 2) %>% 
        map_at(c("AVG"), round, 3) %>% 
        tbl_df()
      
      assign(team, temp) 
      
      remove(hitters, pitchers, temp, undrafted.pitchers, undrafted.hitters)
      
}

