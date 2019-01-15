

#################################
### Roster Parameters Go Here ###
#################################

NUMBER_OF_PITCHERS <- 10
NUMBER_OF_HITTERS <- 15

NUMBER_OF_TEAMS <-  18

START_OF_AUCTION <- lubridate::mdy("1/1/2019")


HITTER_SPOTS <- c("C","1B","2B","SS","3B","CI","MI","OF","DH")

POSITION_ELIGIBILITY <- readxl::read_xlsx("position_eligibility.xlsx")
