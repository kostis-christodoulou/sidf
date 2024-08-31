library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)

# read NASA temperature data. The tabular data of temperature anomalies can be found here
# https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt
nasa_data <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")

# manipulate and tidy the data
global_warming_data <- nasa_data %>%
  
  # pick the first 13 columns
  select(1:13) %>% 
  
  # reshape into longer. tidy format
  pivot_longer(cols = 2:13,
               names_to = "month",
              values_to = "delta") %>%
  
  # add full date as YYYY-MM-DD
  # and add 'month' and 'year' in our dataframe
  mutate(date = ymd(paste(as.character(Year), month, "1")),
         month = month(date, label=TRUE),
         year = year(date)) %>% 
  
  # pick the variables we want
  select(date, year, month, delta) %>% 
  
  #create new variable 'interval', and assign values based on criteria below:
  mutate(interval = case_when(
    year %in% c(1881:1920) ~ "1881-1920",
    year %in% c(1921:1950) ~ "1921-1950",
    year %in% c(1951:1980) ~ "1951-1980",
    year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"
  ))

# explore resulting dataframe
glimpse(global_warming_data)

# summary statistics of delta vs. interval
mosaic::favstats(delta ~ interval, data = global_warming_data)

# Hokeystick plot  ----------------------------------------------
ggplot(global_warming_data, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  geom_hline(yintercept = 0, 
             colour = "orange",
             size = 1.5,
             linetype = "dashed")+
  labs (
    title = "Monthly Temperature Anomalies",
    subtitle = "degrees C compared to the 1951-1980 mean",
    caption = "Source: https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt",
    y     = "Temperature Anomaly (in degrees Celsius)",
    x = NULL
  )


# Distribution of temperature anomalies by interval -----------------------
ggplot(global_warming_data, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +   #density plot with transparency set to 20%
  theme_bw() +                #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    subtitle = "degrees C compared to the 1951-1980 mean",
    caption = "Source: https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt",
    x     = "Temperature Anomaly (in degrees Celsius)",
    y = NULL
  )+
  facet_wrap(~ interval, ncol=1)+
  theme(
      axis.text.y = element_blank(),  # Remove y-axis text labels
      axis.ticks.y = element_blank(),  # Remove y-axis tick marks
      legend.position = "none"  # remove legends
      )+
  NULL


# ECDF for delta by interval ----------------------------------------------
global_warming_data %>% 
  ggplot()+
  aes(x=delta, colour=interval)+
  stat_ecdf()+
  theme_bw() +               
  geom_vline (xintercept = 1.5, 
              colour = "red", 
              size = 2,
              linetype = "dashed")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = seq(-2, 4, by = 0.5),
                     labels = scales::number)+
  labs (
    title = "Cumulative Density Function for Monthly Temperature Anomalies",
    subtitle = "degrees C compared to the 1951-1980 mean",
    caption = "Source: https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt",
    x     = "Temperature Anomaly (in degrees Celsius)",
    y = NULL
  )+
  NULL

