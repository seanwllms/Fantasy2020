standings <- data.frame()

#Calculate current standings
for (team in teams) {
      
      temp <- get(team) %>%
            summarise(R = sum(R, na.rm=TRUE),
                      HR = sum(HR, na.rm=TRUE),
                      RBI = sum(RBI, na.rm=TRUE),
                      SB = sum(SB, na.rm=TRUE),
                      AVG = sum(AVG*AB, na.rm=TRUE)/sum(AB, na.rm=TRUE),
                      ERA = sum(IP*ERA, na.rm=TRUE)/sum(IP, na.rm=TRUE),
                      WHIP = sum(IP*WHIP, na.rm=TRUE)/sum(IP, na.rm=TRUE),
                      K = sum(K, na.rm=TRUE),
                      SV =sum(SV, na.rm=TRUE),
                      W = sum(W, na.rm=TRUE),
                      spent = sum(salary, na.rm=TRUE),
                      left = 260-sum(salary, na.rm=TRUE), 
                      picks.left = sum(salary == 0 & Name =="")
            ) %>%
            mutate(team_name = team) %>%
            select(team_name, spent, left, picks.left, R, HR, RBI, SB, AVG, ERA, WHIP, K, SV, W)
      
      #add results to standings      
      standings <- rbind(standings, temp) 
      
      remove(temp)
}

#load coeficients for calculating standings
load("standingscoefs.rda")

#calculate points
stats <- c("R", "HR", "RBI", "SB", "AVG", "ERA", "WHIP", "K","SV","W")


calc_points <- function(baseball_stat, team) {
  
  stat_amount <- filter(standings, team_name == team) %>% pull(stat) 
  
  standings_data <- filter(coefs.standings, Category == tolower(stat)) 
  
  intercept <- pull(standings_data, yint)
  coef <- pull(standings_data, coef)
  
  value <- intercept + coef * stat_amount
  value
}


map(stats, function(x) {
  label = paste0(x, "_points")
  standings <<- mutate(standings, !!label := calc_points(x, team_name))
})



for (stat in stats) {
      # column_name <- paste(stat,"_points", sep="")
      # standings[column.name] <- coefs.standings[coefs.standings$Category==tolower(stat),2] +
      #                           coefs.standings[coefs.standings$Category==tolower(stat),3]*
      #                           standings[stat]
       
      column_name <- paste(stat,"_points", sep="")
      
      stat_amount <- pull(standings, stat) 
      
      standings_data <- filter(coefs.standings, Category == tolower(stat)) 
      
      intercept <- pull(standings_data, yint)
      coef <- pull(standings_data, coef)
      
      standings <- mutate(standings,
                          !!column_name := intercept + coef * stat_amount)
}

points.var.names <- paste(stats, "_points", sep="")

#create function to fix impossible numbers
rational.points <- function(vector) {
      column <- vector %>%
            sapply(max, 1) %>%
            sapply(min, 18) %>%
            sapply(round, 1)
      column
}

standings[points.var.names] <- lapply(standings[points.var.names], rational.points)

standings <- mutate(standings, total_points = 
                          R_points +
                          HR_points+
                          RBI_points+
                          SB_points+
                          AVG_points+
                          ERA_points+
                          WHIP_points+
                          K_points+
                          SV_points+
                          W_points) %>%
      mutate(total_points = round(total_points,1)) %>%
      arrange(desc(total_points))



bench_bucks <- draftpicks %>% 
  filter(position == "B", salary >0) %>% 
  group_by(team) %>% 
  summarise(bench_dollars = sum(salary)) 


standings <- standings %>% 
  left_join(bench_bucks, by=c("team_name" = "team")) %>% 
  mutate(
    bench_dollars = ifelse(is.na(bench_dollars), 0, bench_dollars),
    spent = spent + bench_dollars,
    left = left - bench_dollars)

