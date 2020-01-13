library(tidyverse)

#Load parameters file
source("parameters.R")

#load in coefficients file
if (!file.exists("coefs.rda")) {
  source("historyanalysis.R")
} else {
  source("coefs.rda")
}

#load hitter and pitcher projections
if (!file.exists("projections.rda")) {
  source("calculatevalue.R")
} else{
  source("projections.rda")
}

#Build league
source("leaguesetup.R")

#run draft
source("draftpicks.R")

#merge in projections
source("mergeinprojections.R")
  
#calculate standings
source("calculatestandings.R")

#write to .csv
source("csvwriter.R")

standings.output


