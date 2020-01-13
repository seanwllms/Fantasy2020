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

#function to calculate points based on historical regression coefficients and intercepts
calc_points <- function(baseball_stat, team) {
  
  #grab total for team
  stat_amount <- filter(standings, team_name == team) %>% pull(baseball_stat) 
  
  #get coefficients
  standings_data <- filter(coefs.standings, Category == tolower(baseball_stat)) 
  
  #grab values
  intercept <- pull(standings_data, yint)
  coef <- pull(standings_data, coef)
  
  #calculate value
  value <- intercept + coef * stat_amount
  value
}

#calculate points for each team and each statistical category
walk(stats, function(x) {
  label = paste0(x, "_points")
  standings <<- mutate(standings, !!label := calc_points(x, team_name)) 
})


#limit points to 1 to 18, and round to nearest tenth
rational_points <- function(x) {
  y <- pmax(x,1)
  y <- pmin(y,18)
  y <- round(y, 1)
  y
}

standings <- standings %>% 
  mutate_at(vars(matches("_points")), rational_points) 
  

#calculate and round total points
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

