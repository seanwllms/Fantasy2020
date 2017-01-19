setwd("/home/sean/Documents/rstuff/fantasy2017")
library(dplyr)
library(purrr)
library(readr)

###Load the coefficients data frame
load("coefs.rda")

###############################################################
################HITTER STUFF LIVES HERE#########################
################################################################

#Import and clean data on replacement levels

#read in league wide csv
replacement_hitters <- read.csv("replacement_hitters.csv", stringsAsFactors = FALSE)

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

names(replacement_hitters)[2:6] <- sapply(names(replacement_hitters[c(2:6)]), paste, ".repl", sep="")

#rename columns
names(replacement_hitters) <- c("position",
                                "runs",
                                "hr",
                                "rbi",
                                "sb",
                                "avg")

#make lists of file names
filelocs_steam <- sapply("./steamer/", paste, list.files("./steamer"), sep="")[c(1:6,8)]
filelocs_depth <- sapply("./depthcharts/", paste, list.files("./depthcharts"), sep="")[c(1:6,8)]
filelocs_fans <- sapply("./fans/", paste, list.files("./fans"), sep="")[c(1:6,8)]

files <- list(fans=filelocs_fans, depth=filelocs_depth, steam=filelocs_steam)


#read in hitterdata
hitterdata <- at_depth(files, 2, read_csv) %>%
      at_depth(2, select, 1, Team, AB, PA, R,HR, RBI, SB, AVG, OBP, playerid) %>%
      at_depth(2, setNames, c("name", "Team", "AB", "PA", "R","HR", "RBI", "SB", "AVG", "OBP", "playerid")) %>%
      at_depth(2, mutate, 
               HR_pa = HR/PA,
               R_pa = R/PA,
               RBI_pa = RBI/PA,
               SB_pa = SB/PA,
               playerid = as.character(playerid))

#create variable for each projection system
hitterdata$fans <- map(hitterdata$fans, mutate, proj="fans")
hitterdata$steam <- map(hitterdata$steam, mutate, proj="steamer")
hitterdata$depth <- map(hitterdata$depth, mutate, proj="depthcharts")

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
      
      #merge all of the projection systems
      raw_pos_data <- bind_rows(
            hitterdata[[1]][[pos]],
            hitterdata[[2]][[pos]],
            hitterdata[[3]][[pos]]
      )
      
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
      results <- select(raw_pos_data, name, Team, playerid) %>% distinct %>%
            left_join(temp)
      
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



#create vector of positions.
positions <- c("first_base",
               "second_base",
               "third_base",
               "catcher",
               "dh",
               "outfield",
               "shortstop")

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
hitter_projections <- lapply(hitter_projections, calculate.value)

#merge projections for different positions together.
hitter_projections <- do.call(rbind, hitter_projections)

#get player's strongest position
hitter_projections <- hitter_projections %>%
      group_by(playerid) %>%
      mutate(times.appears = n(), max.points = max(dollar.value)) %>%
      filter(position != "dh" | times.appears==1) %>%
      filter(dollar.value==max.points) %>%
      ungroup() %>%
      arrange(desc(dollar.value)) %>%
      select(name, Team, position, playerid, PA, AB, R, HR, RBI, SB, AVG, marginal.total.points, dollar.value) %>%
      mutate( marginal.total.points = round(marginal.total.points, 2),
              dollar.value = round(dollar.value, 2)) %>%
      filter(PA > 1)


################################################################
################PITCHER STUFF LIVES HERE########################
################################################################

#read in projections
pitcher_projections <- read.csv("./steamer/pitchers.csv", stringsAsFactors=FALSE)

#keep only relevant columns
pitcher_projections <- select(pitcher_projections,Name,Team,W,ERA,SV,IP,SO,WHIP,playerid) %>%
      mutate(position = "pitcher")

names(pitcher_projections)[c(1, 7)] <- c("name", "K")

#create replacement pitcher values
#these are the mean projections for the 170th through 190th best players
replacement.pitcher <- c(3.761429,1.284286,5.523810,2.952381,88.714286)
names(replacement.pitcher) <- c("ERA.repl","WHIP.repl","W.repl","SV.repl","K.repl")

#calculate marginal values and points
pitcher_projections <- pitcher_projections %>%
      mutate(
            marginal.ERA = ERA - replacement.pitcher["ERA.repl"],
            marginal.WHIP = WHIP - replacement.pitcher["WHIP.repl"],
            marginal.W = W - replacement.pitcher["W.repl"],
            marginal.SV = SV - replacement.pitcher["SV.repl"],
            marginal.K = K - replacement.pitcher["K.repl"],
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
      select(name,Team,position,playerid,IP,ERA,WHIP,SV,W,K,marginal.total.points,dollar.value) %>%
      
      #round points and dollars columns
      mutate(marginal.total.points = round(marginal.total.points, 2), dollar.value = round(dollar.value, 2)) %>%
      
      #select only pithcers with at least 1 IP
      filter(IP > 1)

save(hitter_projections, pitcher_projections, file = "projections.rda")
