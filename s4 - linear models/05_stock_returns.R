library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(here)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
library(janitor)
library(ggrepel)
library(plotly)

# DJIA Wikipedia page-- also contains a table with the 30 stocks that make up the DJIA
djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers 
# plus SPY. an ETF that tracks the SP500

tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF

# define starting- ending dates
end_date = Sys.Date()# today

# go back 5 years
start_year = lubridate::year(end_date-5*365)
start_date = paste0(start_year,"-01-01")

# Download stock prices off yahoo finance
myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = start_date,
         to   = end_date) %>% # today
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame


#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

# histogram for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns))+
  geom_histogram()+
  facet_wrap(~symbol)+
  theme_bw()+
  scale_x_continuous(
    breaks = seq(-0.5, 0.9, by = 0.25),
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = paste0(start_date," to ", end_date),
    x=NULL,
    y=NULL
  )


# boxplot for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ungroup() %>% 

  # arrange the stocks according to montly return, rather than alphabetically
  mutate(symbol=fct_reorder(symbol, monthly_returns)) %>% 
  ggplot(aes(x= monthly_returns, y=symbol))+
  geom_boxplot()+
  theme_bw()+
  scale_x_continuous(
    breaks = seq(-0.5, 0.9, by = 0.1),
    labels = scales::percent_format(accuracy=.1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = paste0(start_date," to ", end_date),
    x=NULL,
    y=NULL  )


# ECDF for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns))+
  stat_ecdf()+
  facet_wrap(~symbol, scales = "free")+
  theme_bw()+
  scale_x_continuous(
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = paste0(start_date," to ", end_date),
    x=NULL,
    y=NULL  )+
  theme(legend.position="none")


# Create a table that summarises monthly returns for each of the stocks and
# for the SPY, the SP500 ETF 
# min, max, median, mean, SD, and CI for mean
monthly_summaries <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(
    min = min(monthly_returns),
    max = max(monthly_returns), 
    mean_return = mean(monthly_returns),
    sd_return = sd(monthly_returns),
    count = n(),
    se_return = sd_return / sqrt(count),
    t_critical = qt (0.975, count - 1),
    lower_95 = mean_return - t_critical * se_return,
    upper_95 = mean_return + t_critical * se_return,
  ) %>% 
  # sort the table
  arrange(desc(mean_return))




#visualise CIs for all stocks 
my_colours <- c("grey60","tomato")

# Create a plot of mean monthly returns with 95% confidence intervals
monthly_summaries %>% 
  # Reorder the 'symbol' factor based on 'mean_return'
  mutate(
    symbol = fct_reorder(symbol, mean_return),  # Reorder symbols by mean return-- otherwise they will show in alphabetic order
    contain_zero = ifelse(lower_95 > 0, TRUE, FALSE)  # Check if lower CI is greater than 0
  ) %>% 
  # Initialize ggplot
  ggplot() +
  aes(x = mean_return, y = symbol, colour = contain_zero) +  # Set aesthetics for the plot
  scale_color_manual(values = my_colours) +  # Manually set colors based on 'contain_zero'
  
  # Add points to the plot
  geom_point() +
  
  # Add error bars representing the 95% confidence intervals
  geom_errorbar(width = 0.5, 
                aes(xmin = lower_95, xmax = upper_95)) + 
  
  # Format the x-axis to display percentages
  scale_x_continuous(labels = scales::percent) +
  
  # Add labels and titles to the plot
  labs(
    x = NULL,  # No label for x-axis
    y = NULL,  # No label for y-axis
    title = "95% CI for Mean Monthly Return of the 30 DJIA Stocks",  # Main title
    subtitle = paste0(start_date, " to ", end_date)  # Subtitle with date range
  ) +
  
  # Use a clean theme for the plot
  theme_bw() +
  
  # Remove the legend from the plot
  theme(legend.position = "none") +
  
  # End the pipeline
  NULL

# Visualise Risk-Return tradeoff

by_year_monthly <- myStocks_returns_monthly %>% 
  mutate(year = year(date),
         month=month(date),
         month_name = month(date, label=TRUE)
  )




my_colours <- c("grey30","tomato")

by_year_monthly %>% 
  group_by(year,symbol) %>% 
  summarise(mean_return = mean(monthly_returns, na.rm=TRUE),
            sd_return = sd(monthly_returns, na.rm=TRUE),
  ) %>% 
  mutate(sp500 = ifelse(symbol == "SPY", TRUE, FALSE)) %>% 
  
  ggplot(aes(x=sd_return, y = mean_return))+
  geom_point(aes(color = sp500))+
  geom_text_repel(aes(label = symbol, color = sp500), size = 3)+
  theme_bw()+
  scale_colour_manual(values = my_colours)+
  facet_wrap(~year,
             ncol = 2,
             scales="free")+
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Risk-Return tradeoff for DJIA stocks",
    subtitle = paste0("Monthly returns ",start_date," to ", end_date),
    x = "Risk (SD of monthly returns)",
    y = "Mean Return" )+
  NULL


# Correlation-Scatterplot matrix of monthly returns for a few stocks, and the SPY

library(GGally)

myStocks_returns_monthly %>% 
  # choose a few stocks
  filter(symbol %in% c("SPY", "AAPL","BA","DIS","PG", "INTC", "GS", "MCD", "MRK", "MSFT")) %>% 
  
  # table must be in wide format--  each column should be the returns of a stock
  pivot_wider(
    names_from = symbol,
    values_from = monthly_returns
  ) %>% 
  select(-date) %>% 
  
  ggpairs() + 
  
  theme_bw()


# Rolling, 6-month calculation of SD
library(zoo)
window <- 6

rolling_sd <- myStocks_returns_monthly %>% 
  filter(symbol %in% c("AAPL", "INTC", "BA", "MCD", "SPY")) %>% 
  group_by(symbol) %>% 
  
  
  mutate(
    rolling_sd = zoo::rollapply(monthly_returns, 
                                FUN = sd, # use function sd, to calculate standard deviation
                                width = window,
                                #By default, NA are removed, so we use the fill
                                fill = NA)
  ) %>% 
  ungroup()


rolling_sd %>% 
  mutate(symbol = fct_reorder(symbol, rolling_sd)) %>% 
  ggplot(aes(x=date, y = rolling_sd, group=symbol, colour=symbol))+
  geom_line()+
  theme_bw()+
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks="6 months", 
               date_labels = "%b-%Y")+
  labs(title = "Rolling 6-month risk (SD) calculation",
       x = "",
       y = "")+
  facet_wrap(~symbol, 
             scales = "free",
             ncol=1)+
  theme(legend.position = "none")+
  NULL



# Calculate total return index since the starting date
total_returns <- myStocks_returns_daily %>% 
  # Group the data by stock symbol
  group_by(symbol) %>% 
  
  # Calculate the cumulative total return for each stock
  mutate(total_return = 1000 * cumprod(1 + daily_returns)) %>% 
  
  # Ungroup the data to return to a regular data frame
  ungroup()

# Plot total return for selected stocks
total_returns %>% 
  # Filter for specific stock symbols of interest
  filter(symbol %in% c("AAPL", "MSFT", "GS", "PG", "AMZN", "V", "MCD", "MRK", "DIS", "INTC", "MMM", "BA")) %>% 
  
  # Group the filtered data by stock symbol again
  group_by(symbol) %>% 
  
  # Remove rows with missing daily returns
  drop_na(daily_returns) %>% 
  
  # Calculate the cumulative total return for each stock again
  mutate(total_return = 1000 * cumprod(1 + daily_returns)) %>% 
  
  # Ungroup the data to return to a regular data frame
  ungroup() %>% 
  
  # Reorder the symbols based on total return in descending order
  mutate(symbol = fct_reorder(symbol, -total_return, mean)) %>% 
  
  # Initialize ggplot for visualization
  ggplot() +
  aes(x = date, y = total_return, group = symbol) +  # Set aesthetics for the plot
  
  # Add lines for each stock's total return
  geom_line(alpha = 0.65) + 
  
  # Use a clean theme for the plot
  theme_bw() + 
  
  # Format the y-axis to display dollar amounts
  scale_y_continuous(labels = scales::dollar) + 
  
  # Remove the legend from the plot
  theme(legend.position = "none") + 
  
  # Create a separate facet for each stock symbol with free y-scales in three columns
  facet_wrap(~symbol, 
             scales = "free_y", 
             ncol = 3) + 
  
  # Add titles and subtitles to the plot
  labs(
    x = NULL,  # No label for x-axis
    y = NULL,  # No label for y-axis
    title = "Total Return, Jan 2020 - Now",  # Main title
    subtitle = paste0("Growth of $1000 invested on ", start_date)  # Subtitle with investment start date
  ) + 
  
  # Ensure the title is top-left aligned
  theme(plot.title.position = "plot") + 
  
  # End the pipeline
  NULL








