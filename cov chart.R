library(tidyverse)
library(readr)

Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
Deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
Recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
isoalpha3 <- read_csv('https://gist.githubusercontent.com/tadast/8827699/raw/7255fdfbf292c592b75cf5f7a19c16ea59735f74/countries_codes_and_coordinates.csv')%>%
  select(Country, `Alpha-3 code`)%>%
  rename(iso_alpha3 = `Alpha-3 code`)

tidy_CSSE <- function(data, value_col = "Confirmed"){
  data%>%
    rename('admin1' = 'Province/State',
           'admin0' = 'Country/Region')%>%
    pivot_longer(cols = contains("/"), names_to = 'date', values_to = value_col)%>%
    select(-Lat, -Long)%>%
    mutate(date = as.Date(date, format = "%m/%d/%y"))
}

tidy_CSSE_admin0 <- function(data, value_col = "Confirmed"){
  sum_col <- paste0(value_col)
  new_col <- paste0(enquo(value_col))[2]

  data %>%
    group_by(admin0, date)%>%
    summarise(!!new_col := sum(!!sym(sum_col)))%>%
    left_join(isoalpha3, by = c('admin0' = 'Country'))%>%
    mutate(unique_id = paste0(iso_alpha3, date))%>%
    ungroup()
  }


confirmed_tidy <- tidy_CSSE_admin0(tidy_CSSE(Confirmed, value_col = 'Confirmed'), value_col = 'Confirmed')

deaths_tidy <- tidy_CSSE_admin0(tidy_CSSE(Deaths, value_col = 'Deaths'), value_col = 'Deaths')

recovered_tidy <- tidy_CSSE_admin0(tidy_CSSE(Recovered, value_col = 'Recovered'), value_col = 'Recovered')

all_data_admin0 <- left_join(confirmed_tidy,deaths_tidy, by = c('unique_id' = 'unique_id'))%>%
  left_join(recovered_tidy, by = c('unique_id' = 'unique_id'))%>%
  select(-contains("."))%>%
  select(admin0, iso_alpha3, date, Confirmed, Deaths, Recovered)




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
