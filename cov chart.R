library(tidyverse)
library(readr)
Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

Confirmed %>%
  rename(Country = `Country/Region`) %>%
  select(-c(`Province/State`)) %>% 
  gather(day, Confirmed, 4:{dim(Confirmed)[2]-1}) %>%
  mutate(Date = as.Date(day, format = "%m/%d/%y")) %>%
  group_by(Country, Date) %>%
  summarise(Lat = mean(Lat), Long = mean(Long), Confirmed = sum(Confirmed)) %>%
  filter(Country %in% c("Switzerland", "Italy", "China", "France", "Poland", "United Kingdom")) %>%
  group_by(Country) %>%
  ggplot(aes(x=Date, y=Confirmed, colour=Country)) + geom_line() + theme_minimal() +
  scale_y_log10()
