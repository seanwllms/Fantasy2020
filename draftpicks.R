#read csv of draft picks
draftpicks <- readxl::read_xlsx("draftpicks.xlsx", sheet = "draftpicks")

#########################################################################
## Create draft function to add a player to the team in the draft. ######
#########################################################################
draft <- function(team, player, salary, pos) {
      
      #create vector of repetitive positions
      specialcases <- c("OF1","OF2","OF3","OF4","OF5","OF6",
                        "P1","P2","P3","P4","P5","P6","P7","P8","P9","P10",
                        "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10")
      
      #Create temporary vector for team Name
      temp <- get(team)
      
      #assign values to relevant positions
      if (!(pos == "OF" | pos == "P" | pos == "B" | pos =="C")) {
            temp[pos,"salary"] <- salary
            temp[pos,"Name"] <- player
      }
      
      #Handle pitchers, outfielders, bench players
      else {
            if (pos == "OF") {
                  for (i in 1:6) {
                        outfield_number <- paste(pos, i, sep = "")
                        if (temp[outfield_number,"Name"] == "") {
                              temp[outfield_number,"salary"] <- salary
                              temp[outfield_number,"Name"] <- player
                              break
                        }
                  }
            }
            
            else if (pos == "P") {
                  for (i in 1:10) {
                        pitcher_number <- paste(pos, i, sep = "")
                        if (temp[pitcher_number,"Name"] == "") {
                              temp[pitcher_number,"salary"] <- salary
                              temp[pitcher_number,"Name"] <- player
                              break
                        }
                  }
            }
            
            else if (pos == "B") {
                  for (i in 1:10) {
                        bench_number <- paste(pos, i, sep = "")
                        if (temp[bench_number,"Name"] == "") {
                              temp[bench_number,"salary"] <- salary
                              temp[bench_number,"Name"] <- player
                              break
                        }
                  }
            }
            
            else if (pos == "C") {
                  for (i in 1:2) {
                        catcher_number <- paste(pos, i, sep = "")
                        if (temp[catcher_number,"Name"] == "") {
                              temp[catcher_number,"salary"] <- salary
                              temp[catcher_number,"Name"] <- player
                              break
                        }
                  }
            }
      } 
      

      assign(team, temp, env = .GlobalEnv)      
}

#######################################################
###############Mark Drafted Function###################
#######################################################

hitter_projections <- hitter_projections %>% 
      mutate(status = ifelse(Name %in% draftpicks$player, "drafted", ""))

pitcher_projections <- pitcher_projections %>% 
      mutate(status = ifelse(Name %in% draftpicks$player, "drafted", ""))

drafterrors <- select(draftpicks, player, team) %>%
      mutate(error = ifelse(player %in% hitter_projections$Name | player %in% pitcher_projections$Name,
             "matched", "not matched")
      ) %>%
      filter(error == "not matched")


#draft everything in the draft csv
for (pick in 1:nrow(draftpicks)) {
      
      this_pick <- filter(draftpicks, row_number() == pick)
      pick_team <- pull(this_pick, team)
      pick_player <- pull(this_pick, player)
      pick_salary <- pull(this_pick, salary)
      pick_position <- pull(this_pick, position)
      
      #run draft function on that row
      draft(pick_team,
            pick_player,
            pick_salary, 
            pick_position)
      
      #remove leftover variable that isn't needed
      remove(a)
}
