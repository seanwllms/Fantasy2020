#set up file
library(tidyverse)
library(readxl)

#read in results from pre-2014
results <- read_xlsx("./history/historicalresults.xlsx") %>% 
  mutate(Category = tolower(Category))

#fill in results for years that it is missing
results[1:540, 4] <- c(rep(2010, 18), rep(2011, 18), rep(2012, 18))

#reorder columns to match 2014 and 2015
results <- select(results, Category, Value, Year, Points)

sb <- filter(results, Category == "sb")

######################################
#####read in the 2014 results#########
######################################
standings.2014 <- read_csv("./history/results2014.csv") %>% 
      select(R, HR, RBI, SB, Avg, W, K, Sv, ERA, WHIP) %>% 
  rename_all(.funs = tolower) %>% 
  gather("Category", "Value") %>%
  mutate(Value = as.numeric(Value)) %>% 
  group_by(Category) %>% 
  arrange(desc(Value)) %>%
  mutate(Points = min_rank(Value)) %>% 
  mutate(Points = ifelse(Category %in% c("era", "whip"), 
                         19-Points, 
                         Points), 
         Year = 2014)

######################################
#####read in the 2015 results#########
######################################
standings.2015 <- read_csv("./history/results2015.csv") %>% 
  select(R, HR, RBI, SB, Avg, W, K, Sv, ERA, WHIP) %>% 
  rename_all(.funs = tolower) %>% 
  gather("Category", "Value") %>% 
  group_by(Category) %>% 
  arrange(desc(Value)) %>%
  mutate(Points = min_rank(Value)) %>% 
  mutate(Points = ifelse(Category %in% c("era", "whip"), 
                         19-Points, 
                         Points), 
         Year = 2015)

######################################
##### Merge All Results Together######
######################################
all_results <- bind_rows(results, standings.2014, standings.2015) %>%
      #filter out rows from 2013 that seem to be outliers
      filter(!(Year==2013 & Category %in% c("r", "rbi", "hr"))) %>% 
  arrange(Category, Year, -Value) 

save(all_results, file="historicalresults.rda")

######################################
#####Graphs and Analysis go Here######
######################################
library(ggplot2)
library(broom)

catplot <- all_results %>% 
  mutate(LastYear = ifelse(Year >2018, "2019", "Earlier Years")) %>% 
  ggplot(aes(x=Value, y=Points)) +
  geom_point(aes(color = LastYear)) +
  facet_wrap(~ Category, ncol=2, scales="free_x") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank())

ggsave("catplot.png", catplot, width=6, height = 8)

###Regression Time####

#filter out 1, 2, 17 and 18 point recipients (skew results)
regress <- results %>% filter(Points > 2 & Points < 17)

#run the regression for each category
regress <- regress %>% group_by(Category) %>%
      do(regresults = lm(Points ~ Value + factor(Year), data=.))

#organize regression results in to tidy df for calculating value 
coefs <- tidy(regress, regresults) %>%
      filter(term == "Value") %>%
      select(Category, estimate)

save(coefs, file="coefs.rda")

#organize regression results in to tidy df for calculating standings
coefs.standings <- tidy(regress, regresults) %>% select(Category, term, estimate)  %>% 
  filter(!str_detect(term, "factor")) %>% 
  spread(term, estimate)


names(coefs.standings)[2:3] <- c("yint", "coef")

save(coefs.standings, file="standingscoefs.rda")
