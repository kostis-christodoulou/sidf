library(tidyverse)
library(lubridate)
library(here)
library(mosaic)
library(ggridges)
library(viridis)
options(digits=4)

# read CSV file. 
bottling_lines <- read_csv(here::here('data', 'bottling_lines.csv'))

# Get a glimpse of the data- which vairables do we have, etc
glimpse(bottling_lines)

# Count how many samples in each bottling line machine
bottling_lines %>% 
  count(machine,sort=TRUE)

# Create a new variable, delta, which is the difference
# between actual net_content and target
bottling_lines <- bottling_lines %>% 
  
  mutate(delta = net_content - target,
         
         # also express difference as % of target
         delta_percent = delta / target)

# Summary statistics for each machine
mosaic::favstats(net_content ~ machine, data = bottling_lines)


# which bottles are rejected and cannot be sold? 
# Lower control limit LCL = 2% below target
bottling_lines <- bottling_lines %>% 
  
  mutate(lcl = 0.98 * target,
         rejected = net_content < lcl
         ) %>% 
  group_by(machine) %>% 
  mutate(mean_content = mean(net_content),
         sd_content = sd(net_content)) %>% 
  ungroup()


# count how many are rejected in each machine
bottling_lines %>% 
  
  group_by(machine, rejected) %>% 
  summarise(count = n()) %>% 
  
  mutate(percent = count / sum(count))


#**************************************
# Plot boxplots of net_content, faceted by machine
bottling_lines %>%
  ggplot()+
  aes(x = net_content, colour=machine) +
  geom_boxplot() +
  facet_wrap(~machine, 
             ncol=1,
             scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Net content (ml) by machine",
       x = NULL,
       y = NULL)+
  NULL



# Plot histogram of net_content, faceted by machine
bottling_lines %>%
  ggplot()+
  aes(x = net_content, fill = machine) +
  geom_histogram() +
  facet_wrap(~machine, 
             ncol=1, 
             scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Net content (ml) by machine",
       x = NULL,
       y = NULL)+
  NULL


# Plot density plot of net_content, faceted by machine
bottling_lines %>%
  ggplot()+
  aes(x = net_content, colour=machine) +
  geom_density() +
  facet_wrap(~machine, 
             ncol = 1,
             scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Net content (ml) by machine",
       x = NULL,
       y = NULL)+
  NULL



# Plot ECDF of net_content, faceted by machine
bottling_lines %>%
  ggplot()+
  aes(x = net_content, colour=machine) +
  stat_ecdf(geom = "step", pad = FALSE) +

  # add line for mean net_content 
  geom_vline(aes(xintercept = mean_content, 
                 colour = machine),
             size = 1.3)+ 
  # add annotation for mean
  geom_text(aes(x=mean_content, 
                label=paste0("\nMean = ",round(mean_content , digits = 2)), 
                y=0.5), 
            angle=90) +
  
  
  # add line for mean-2*SDs
  geom_vline(aes(xintercept=mean_content - 2*sd_content),
             colour = "orange",
             linetype = "dashed")+
  # add annotation for  mean-2*SDs
  geom_text(aes(x=mean_content - 2*sd_content, 
                label=paste0("\nMean - 2*SD= ",round(mean_content - 2*sd_content, digits = 2)), 
                y=0.5), colour="orange", 
            angle=90) +
  
  
  # add line for mean+2*SDs
  geom_vline(aes(xintercept=mean_content + 2*sd_content),
             colour = "orange",
             linetype = "dashed")+
  # add annotation for  mean+3*SDs
  geom_text(aes(x=mean_content + 2*sd_content, 
                label=paste0("\nMean + 2*SD= ",round(mean_content + 2*sd_content, digits = 2)), 
                y=0.5), colour="orange", 
            angle=90) +
  
  # add line for mean-3*SDs
  geom_vline(aes(xintercept=mean_content - 3*sd_content),
             colour = "tomato",
             linetype = "dashed")+
  # add annotation for  mean-3*SDs
  geom_text(aes(x=mean_content - 3*sd_content, 
                label=paste0("\nMean - 3*SD= ",round(mean_content - 3*sd_content, digits = 2)), 
                y=0.5), colour="tomato", 
            angle=90) +
  
  
  # add line for mean+3*SDs
  geom_vline(aes(xintercept=mean_content + 3*sd_content),
             colour = "tomato",
             linetype = "dashed")+
  # add annotation for  mean+3*SDs
  geom_text(aes(x=mean_content + 3*sd_content, 
                label=paste0("\nMean + 3*SD= ",round(mean_content + 3*sd_content, digits = 2)), 
                y=0.5), colour="tomato", 
            size = 4,
            angle=90) +
  
  # format y-axis as a % 
  scale_y_continuous(labels = scales::percent)+
  
  # facet wrap to give a different graph for each machine
  facet_wrap(~machine, 
             ncol=1,
             scales = "free")+
  
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Net content (ml) by machine",
       x = NULL,
       y = NULL)+
    NULL



# Summary stats of net content by machine
favstats(net_content ~ machine, data = bottling_lines)


bottling_lines %>% 
  ggplot()+
  aes(x = net_content, y = machine, colour=machine) +
 
  geom_point(position = "jitter", size = 0.8, alpha = 0.50) + 
  
  # we can also superimpose  the CI for mean content as a big orange dot
  stat_summary(#geom = "pointrange",
    fun.data = "mean_se",
    colour = "orange",
    linewidth = 2,
    size = 1,
    fun.args = list(mult = 1.96)) +

   facet_wrap(~machine, 
              scales = "free")+
  
  # add labels, on x= y axes and title
  labs(x= "Net Content (ml)", 
       y = NULL,
       title="Mean Content by Machine- bias to overfilling?") + 
  
  theme_bw()+
  theme(legend.position = 'none')+
  NULL


