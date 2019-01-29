#write projections files to csv
write.csv(pitcher_projections, file = "./results/pitcher_projections.csv")
write.csv(hitter_projections, file = "./results/hitter_projections.csv")
write.csv(marmaduke, file = "./results/marmaduke.csv")
write.csv(pasadena, file = "./results/pasadena.csv")


#create file for best remaining players
hitterpitcher <- bind_rows(hitter_projections, pitcher_projections) %>%
      arrange(desc(dollar.value)) %>%
      select(Name, Team, position, marginal.total.points, dollar.value, status)

hitterpitcher <- filter(hitterpitcher, status != "drafted" & dollar.value > -5)

write.csv(hitterpitcher, "./results/bestremaining.csv")


#write out draft errors to csv
write.csv(drafterrors, "./results/drafterrors.csv")

#write standings output to file
standings.output <- select(standings,
                           team_name, spent, left, picks.left,
                           total_points, R_points, HR_points, RBI_points, 
                           SB_points, AVG_points, ERA_points, WHIP_points, K_points, SV_points, W_points)



write.csv(standings.output, file="./results/standings.csv")

