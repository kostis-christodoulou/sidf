library(tidyverse)
library(skimr)
library(mosaic)
library(lubridate)
library(janitor)


uber <- read_csv(here::here('data', 'uber.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(period_start = lubridate::dmy_hm(period_start)
  )

# Plot cancellation over time

uber %>% 
  ggplot(aes(x= period_start, y=cancellation_rate, colour = group)) + 
  geom_point() +

  # plot target cancellation of 4%
  geom_hline(yintercept = 4,
             colour = "#001e62",
             size = 1.1,
             linetype = "dashed")+

  theme_bw()+
  labs(title = "Cancellation Rate for groups A & B",
       subtitle = "Group A: 5 min wait-time, B: 2 min wait time",
       x = NULL, 
       y = "Cancellation Rate")


uber %>% 
  ggplot(aes(x=cancellation_rate, y = group)) + 
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", 
               colour = "red", 
               linewidth = 2, 
               size = 2)+
  
  # plot target cancellation of 4%
  geom_vline(xintercept = 4,
             colour = "#001e62",
             linetype = "dashed")+
  
  
  
  theme_bw()+
  labs(title = "Cancellation Rate for groups A & B",
       subtitle = "Group A: 5 min wait-time, B: 2 min wait time",
       y = NULL, 
       x = "Cancellation Rate")



uber %>% 
  ggplot(aes(x=cancellation_rate, y = group)) + 
  geom_point() +

  
  # stat_summary() takes a function (here mean_se()) and runs it on
  # each of the groups to get the average and standard error. It then
  # plots those with geom_pointrange. The fun.args part of this lets us pass an
  # argument to mean_se() so that we can multiply the standard error by 1.96,
  # giving us the 95% confidence interval.
  
  stat_summary(geom = "pointrange", 
               fun.data = "mean_se", 
               colour = "red", 
               linewidth = 2, 
               size = 2,
               fun.args = list(mult = 1.96)) +

  # plot target cancellation of 4%
  geom_vline(xintercept = 4,
             colour = "#001e62",
             linetype = "dashed")+
  
  theme_bw()+
  labs(title = "Cancellation Rate for groups A & B",
       subtitle = "Group A: 5 min wait-time, B: 2 min wait time",
       y = NULL, 
       x = "Cancellation Rate")




# Summary Statistics ------------------------------------------------------


options(digits = 5)
# summary statistics of cancellation rate 
favstats(cancellation_rate ~ group, data = uber)


# Hypothesis Testing ------------------------------------------------------


# 1 sample t-test. is group A cancellation rate  = 4%
t.test(cancellation_rate ~ 1, # ~1 run a 1-sample t-test
       
       # pick only group A
       data = uber %>% 
         filter(group == 'A - 5 min'), 
       
       # Null hypothesis is that population mean = 4 percent
       mu = 4)

# 1 sample t-test. is group B cancellation rate = 4%
t.test(cancellation_rate ~ 1, # ~1 run a 1-sample t-test
       
       # pick only group B
       data = uber %>% 
         filter(group == 'B - 2min'), 
       
       # Null hypothesis is that population mean = 4 percent
       mu = 4)

# two-sample t-test/ Is the mean cancellation rate between the groups the same or not?
t.test(cancellation_rate ~ group, data = uber)
