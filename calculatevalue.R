
library(tidyverse)
###Load the coefficients data frame
load("coefs.rda")

###############################################################
################HITTER STUFF LIVES HERE#########################
################################################################

#Import and clean data on replacement levels

#read in league wide csv
replacement_hitters <- readxl::read_xlsx("./replacement/replacement_hitters.xlsx", sheet = "replacement_hitters")

replacement_hitters$Position <- c("catcher",
                                  "first_base",
                                  "second_base",
                                  "shortstop",
                                  "third_base",
                                  "middle_infield",
                                  "corner_infield",
                                  "outfield",
                                  "dh"
                                  )

# replacement_hitters <- replacement_hitters %>% 
#   rename(Runs.repl = Runs,
#          HR.Repl = HR,
#          RBI.repl =RBI,
#          SB.repl = SB,
#          AVG.repl = AVG)

#names(replacement_hitters)[2:6] <- sapply(names(replacement_hitters[c(2:6)]), paste, ".repl", sep="")

#rename columns
names(replacement_hitters) <- c("position",
                                "runs",
                                "hr",
                                "rbi",
                                "sb",
                                "avg")

#make lists of file names
projections <- c("steamer", "depthcharts", "fans", "zips", "atc", "thebat")


  
getfiles <- function(proj) {

  #build list of paths to files
  folder <- paste0("./", proj, "/")
  
  filelist <- list.files(folder)
  filelist <- filelist[filelist != "pitcher.csv"]
  filelist <- filelist[filelist != "pitchers.csv"]

  if (length(filelist) > 0) {
    filelist <- map_chr(filelist, function(x) paste0(folder, x))
  } 
  #for each path, read csv file and clean it up
  dfs <- map(filelist, function(x) {
    if (!str_detect(x, "pitchers"))
    read_csv(x) %>%
      mutate(proj=proj) %>% 
      select(Name, Team, AB, PA, R,HR, RBI, SB, AVG, OBP, proj, playerid) %>% 
      mutate( 
        HR_pa = HR/PA,
        R_pa = R/PA,
        RBI_pa = RBI/PA,
        SB_pa = SB/PA,
        playerid = as.character(playerid)
      )
  })
  keep(dfs, function(x) !is.null(x))
}

#build nested list with all of the data frames.
hitter_data_frames <- map(projections, getfiles) %>% 
  keep(function(x) length(x) > 1) 


#build list of all projection systems successfully read
proj_systems <- map_chr(hitter_data_frames, function(x){
                  pluck(x, 1) %>% 
                    pull(proj) %>% 
                    unique() 
                }) 
                
#assign names in df list
names(hitter_data_frames) <- proj_systems

####################################
############   PECOTA   ############
####################################
#read in PECOTA data and rname variables to line up

if (file.exists("./pecota/pecota_bat1_2019-02-04_60742.csv")) {
  pecotahit<- read_csv("./pecota/pecota_bat1_2019-02-04_60742.csv") %>% 
  mutate(Name = paste(FIRSTNAME, LASTNAME, sep=" ")) %>% 
  rename(Team = TEAM, 
         BPID=ID) %>% 
  select(Name, BPID, Team, AB, PA, R,HR, RBI, SB, AVG, OBP) %>% 
  mutate(Team = "pecotaflag")

  #crosswalk PECOTA to BP
  if (file.exists("./pecota/crosswalk.rda")) {
    load("./pecota/crosswalk.rda")
  } else {
    crosswalk <- read_csv(url("http://crunchtimebaseball.com/master.csv")) %>%
      rename(Name = fg_name, playerid=fg_id, BPID = bp_id) %>% 
      select(Name, playerid, BPID)
    save(crosswalk, file="./pecota/crosswalk.rda")
  }
  
  pecotahit <- left_join(pecotahit, crosswalk) %>% 
    select(-BPID) %>% 
    mutate(HR_pa = HR/PA,
           R_pa = R/PA,
           RBI_pa = RBI/PA,
           SB_pa = SB/PA,
           playerid = as.character(playerid),
           proj="pecota") %>% 
    filter(!is.na(playerid))
}


####################################
##### AGGREGAGE PROJECTIONS ########
####################################


#create vector of positions.
positions <- c("first_base",
               "second_base",
               "third_base",
               "catcher",
               "dh",
               "outfield",
               "shortstop")


#loop through vector and average projections across each system
for (pos in 1:7) {
      
      position_name <- positions[pos]
      
      raw_pos_data <- data.frame()
      for (system in seq_along(proj_systems)) {
        raw_pos_data <- bind_rows(raw_pos_data, hitter_data_frames[[system]][[pos]])
      }
      
      # #merge all of the projection systems
      # raw_pos_data <- bind_rows(
      #   hitter_data_frames[[1]][[pos]],
      #   hitter_data_frames[[2]][[pos]],
      #   hitter_data_frames[[3]][[pos]],
      #   #pecotahit,
      #   hitter_data_frames[[4]][[pos]],
      #   hitter_data_frames[[5]][[pos]]
      # ) 
      
      if (exists("pecotahit")) {
        raw_pos_data <- bind_rows(raw_pos_data, pecotahit)
      }
      
      raw_pos_data <- raw_pos_data %>% 
        group_by(playerid) %>% 
        mutate(count = n()) %>%
        ungroup() %>% 
        filter(count > 1 | proj !="pecota") %>% 
        select(-count)
      
      #grab the plate appearances for the depth charts projections
      at_bats <- filter(raw_pos_data, proj=="depthcharts") %>%
            mutate(depthpa = PA) %>%
            select(playerid, depthpa)
      
      #average across projection systems
      temp <- group_by(raw_pos_data, playerid) %>%
            summarise(AB = mean(AB),
                  PA = mean(PA),
                   R_ab = mean(R_pa),
                   HR_ab = mean(HR_pa),
                   RBI_ab = mean(RBI_pa),
                   SB_ab = mean(SB_pa),
                   AVG = mean(AVG),
                   OBP = mean(OBP)) %>%
            
            #merge in the PA projections
            left_join(at_bats) %>%
            #use depth charts PA if available
            mutate(PA = ifelse(is.na(depthpa), PA, depthpa)) %>%
            #multiply rate based projections by PA
            mutate(R = R_ab*PA, 
                   HR = HR_ab*PA,
                   RBI = RBI_ab*PA,
                   SB = SB_ab*PA) %>%
            select(playerid, PA, AB, R, HR, RBI, SB, AVG, OBP)
      
      #join averaged data with all names in all 3 projection systems
      results <- select(raw_pos_data, Name, Team, playerid) %>% distinct %>%
            left_join(temp) %>% 
        filter(Team != "pecotaflag" | is.na(Team))
      
      replacement <- filter(replacement_hitters, position==position_name)
      names(replacement)[2:6] <- sapply(names(replacement)[2:6], paste, ".repl", sep="")
      
      results <- cbind(results, replacement)
      
      #make name for position projection df      
      dfname <- paste(position_name, "_proj", sep="")
      
      #save it
      assign(dfname, results) 
      
      #remove temp variables
      remove("temp", "raw_pos_data", "at_bats", "results", "dfname")
}


#build all positional projections into a list
hitter_projections <- list(first_base_proj,
                    second_base_proj,
                    third_base_proj,
                    catcher_proj,
                    dh_proj,
                    outfield_proj,
                    shortstop_proj)

#convert coefficients frame to a normal data frame
coefs.for.calc <- as.numeric(coefs$estimate)
names(coefs.for.calc) <- coefs$Category

#create function to calculate value for a position
calculate.value <- function(df) {
      
      mutate(df, 
             marginal.hr = HR - hr.repl, 
             marginal.runs = R - runs.repl,
             marginal.rbi = RBI - rbi.repl,
             marginal.sb = SB - sb.repl,
             marginal.avg = AVG - avg.repl,
             marginal.runs.points = marginal.runs * coefs.for.calc[["r"]],
             marginal.hr.points = marginal.hr * coefs.for.calc[["hr"]],
             marginal.rbi.points = marginal.rbi * coefs.for.calc[["rbi"]],
             marginal.sb.points = marginal.sb * coefs.for.calc[["sb"]],
             marginal.avg.points = marginal.avg * coefs.for.calc[["avg"]]/15,
             marginal.total.points = (marginal.runs.points +
                                    marginal.hr.points +
                                    marginal.rbi.points +
                                    marginal.avg.points +
                                    marginal.sb.points)*1.2,#this is an adjustment upwards;
                                                            #in 2016 calculation, top 270 players
                                                            #result in only 735.5 marginal points.
                                                            #this means we adjust upwards to make 855
                                                            #marginal points (sum(1:18)*5)
             
             #total of 4680 dollars exist in the league. 1700 marginal points exist. Therefore, marginal
             #point is worth 4680/1700
             dollar.value = marginal.total.points*(4680/1700)
      )      
}

#calculate values for all of the positions
hitter_projections <- map(hitter_projections, calculate.value)

#merge projections for different positions together.
hitter_projections <- do.call(rbind, hitter_projections)

#Calculate player's strongest position
best_position <- hitter_projections %>%
      group_by(playerid) %>%
      mutate(max_value = max(dollar.value),
             is_best = max_value==dollar.value) %>%
      filter(is_best) %>%
      mutate(bestpos = position) %>%
      select(playerid, bestpos)

hitter_projections <- hitter_projections %>%
      left_join(best_position, by=c("playerid")) %>%
      filter(position == bestpos) %>%
      arrange(desc(dollar.value)) %>%
      select(Name, Team, position, playerid, PA, AB, R, HR, RBI, SB, AVG, marginal.total.points, dollar.value) %>%
      mutate(AB=round(AB),PA = round(PA), R = round(R), HR=round(HR), RBI=round(RBI), SB=round(SB), AVG =round(AVG, 3),
             marginal.total.points = round(marginal.total.points, 2),
             dollar.value = round(dollar.value, 2)) %>%
      filter(PA > 1)



################################################################
################PITCHER STUFF LIVES HERE########################
################################################################
library(stringr)
#read in files for all of the systems other than ZIPS (which doesn't do saves)
projection_systems <- c("depthcharts", 
                        "steamer",
                        "fans",
                        "atc",
                        "zips",
                        "thebat")

#read in the list of pitcher projections and set list item names
pitcher_proj <- map_chr(projection_systems, function(x) paste("./", x, "/pitchers.csv", sep="")) 
names(pitcher_proj) <- projection_systems


pitcher_proj <- pitcher_proj %>% 
      map(function(x) {if (file.exists(x)) read_csv(x) %>% mutate(proj=x)
        }) 

#mutate zips data frame to deal with missing saves
if (!is.null(pitcher_proj[["zips"]])) {
  pitcher_proj[["zips"]] <- mutate(pitcher_proj[["zips"]], SV = NA)
} 


pitcher_proj <- pitcher_proj %>% 
      keep(function(x) !is.null(x)) %>%
      map(select, Name, playerid, Team, IP, ERA, WHIP, SO, SV, W, proj) %>% 
      map(rename, K = SO) %>% 
      map(mutate, proj = str_remove(proj, "./"),
                  proj = str_remove(proj, "/pitchers.csv"),
                  playerid = as.character(playerid))

      
####################################
############   PECOTA   ############
####################################
if (file.exists("./pecota/pecota_pit1_2019-02-04_60742.csv")) {
  #read in PECOTA data and rename variables to line up
  pecotapitch <- read_csv("./pecota/pecota_pit1_2019-02-04_60742.csv") %>% 
    mutate(Name = paste(FIRSTNAME, LASTNAME, sep=" ")) %>% 
    rename(Team = TEAM, K = SO, BPID=ID) %>% 
    select(Name, BPID, Team, IP, ERA, WHIP, K, SV, W) %>% 
    mutate(Team = "pecotaflag",
           proj = "pecota") 
  
  pecotapitch <- left_join(pecotapitch, crosswalk) %>% 
    select(-BPID) %>% 
    filter(!is.na(playerid)) %>% 
    select(-name)
  
  pitcher_proj[["pecota"]] <- pecotapitch
}


#group everything together
pitcher_proj <- bind_rows(pitcher_proj)

#get vector of innings pitched
innings <- filter(pitcher_proj, proj=="depthcharts") %>%
      mutate(depthip = IP) %>%
      select(Name, Team, playerid, depthip)

#spread per ip numbers across depth charts IP
pitcher_proj <- mutate(pitcher_proj,
                       K_IP = K/IP,
                       SV_IP = SV/IP,
                       W_IP = W/IP) %>%
      group_by(playerid) %>%
      summarise(ERA = mean(ERA),
                WHIP = mean(WHIP),
                K_IP = mean(K/IP),
                SV_IP = mean(SV/IP, na.rm=TRUE),
                W_IP = mean(W/IP))

pitcher_proj <- left_join(innings, pitcher_proj) %>%
      mutate(IP = depthip,
             ERA = round(ERA, 2),
             WHIP = round(WHIP, 2),
             K = round(K_IP*IP, 0),
             SV = round(SV_IP*IP, 0),
             W = round(W_IP*IP, 0), 
             position = "pitcher") %>%
      select(Name, Team, IP, W, ERA, SV, K, WHIP, playerid, position)

#create replacement pitcher values
#these are the mean projections for the 170th through 190th best players

replacement.pitcher <- readxl::read_xlsx("./replacement/replacement_pitchers.xlsx",
                                         sheet="replacement") %>% 
  select(-IP) 

new_colnames <- map_chr(names(replacement.pitcher), paste0, ".repl")

names(replacement.pitcher) <- new_colnames

#calculate marginal values and points
pitcher_projections <- pitcher_proj %>%
      mutate(
            marginal.ERA = ERA - replacement.pitcher[["ERA.repl"]],
            marginal.WHIP = WHIP - replacement.pitcher[["WHIP.repl"]],
            marginal.W = W - replacement.pitcher[["W.repl"]],
            marginal.SV = SV - replacement.pitcher[["SV.repl"]],
            marginal.K = K - replacement.pitcher[["K.repl"]],
            ERA.points = (marginal.ERA *coefs.for.calc[["era"]])*(IP/1464),
            WHIP.points = (marginal.WHIP*coefs.for.calc[["whip"]])*(IP/1464),
            W.points = marginal.W*coefs.for.calc[["w"]],
            SV.points = marginal.SV*coefs.for.calc[["sv"]],
            K.points = marginal.K*coefs.for.calc[["k"]],
            marginal.total.points =  1.16*(ERA.points + WHIP.points + W.points + SV.points + K.points),
            dollar.value = marginal.total.points*(4680/1700)
      ) %>%
      
      #sort by dollar value
      arrange(desc(dollar.value)) %>%
      
      #select relevant columns
      select(Name,Team,position,playerid,IP,ERA,WHIP,SV,W,K,marginal.total.points,dollar.value) %>%
      
      #round points and dollars columns
      mutate(marginal.total.points = round(marginal.total.points, 2), dollar.value = round(dollar.value, 2)) %>%
      
      #select only pithcers with at least 1 IP
      filter(IP > 1)

save(hitter_projections, pitcher_projections, file = "projections.rda")



########################################
########### CALIBRATE VALUES ###########
########################################




#function to calculate number of pitchers above replacement level
# calc_positive_pitchers <- function() {
#   positive_pitchers <- filter(pitcher_projections, dollar.value >= 1) %>%
#   summarise(players = n()) %>%
#   pull(players)
# 
#   positive_pitchers
# }
# 
# #Adjust marginal points
# while (abs(calc_positive_pitchers()-NUMBER_OF_PITCHERS*NUMBER_OF_TEAMS) > 5) {
# 
#   if (calc_positive_pitchers() > NUMBER_OF_PITCHERS*NUMBER_OF_TEAMS) {
#     adjusted_pitcher_projections <- mutate(pitcher_projections,
#                                   marginal.total.points =marginal.total.points - .1,
#                                   dollar.value = marginal.total.points*(4680/1700))
#   } else {
#     adjusted_pitcher_projections <- mutate(pitcher_projections,
#                                            marginal.total.points = marginal.total.points + .1,
#                                            dollar.value = marginal.total.points*(4680/1700))
#   }
# 
#   pitcher_projections <- adjusted_pitcher_projections
#   remove(adjusted_pitcher_projections)
# }
# 
# 
# 
# #function to calculate number of hitters above replacement level
# calc_positive_hitters <- function(pos) {
# 
#   #get list of acceptable positions for the roster spot
#   acceptable_positions <- POSITION_ELIGIBILITY %>%
#     filter(roster_spot == !!pos) %>%
#     pull(position)
# 
#   positive_hitters <- filter(hitter_projections, dollar.value > 1, position %in% acceptable_positions) %>%
#     summarise(players = n()) %>%
#     pull(players)
# 
#   positive_hitters
# }
# 
# for (position in HITTER_SPOTS) {
#   
#   print(paste0(position, ": ", as.character(calc_positive_hitters(position))))
#   
# }
# 
# 
# 
# 
# 
# ###########################################################
# ############calculate total dollars in forecast############
# ###########################################################
# calc_positive_pitchers()
# 
# hitter_points <- filter(hitter_projections, dollar.value > 0) %>%
#   summarise(marginal.total.points = sum(marginal.total.points),
#             players = n())
# 
# hitter_points
# 
# sum(1:18)
# 
# hitterbucks <- pull(hitter_points, marginal.total.points) * 4680/1700
# pitcher_bucks <- pitcher_projections %>% 
#   filter(dollar.value>=1) %>% 
#   summarise(bucks = sum(dollar.value))
# 
# 
# alternate_hitter_projections <- hitter_projections %>% 
#   mutate(dollar.value = dollar.value/1.3)
# 
# alternate_pitcher_projections <- pitcher_projections %>% 
#   mutate(dollar.value = dollar.value/1.3)
# 
