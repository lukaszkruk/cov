library(tidyverse)

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
    pivot_longer(cols = contains("/"), names_to = 'date_raw', values_to = value_col)%>%
    select(-Lat, -Long)%>%
    mutate(Date = as.Date(date_raw, format = "%m/%d/%y"))
}

tidy_CSSE_admin0 <- function(data, value_col = "Confirmed"){
  sum_col <- paste0(value_col)
  new_col <- paste0(enquo(value_col))[2]

  data %>%
    group_by(admin0, Date)%>%
    summarise(!!new_col := sum(!!sym(sum_col)))%>%
    left_join(isoalpha3, by = c('admin0' = 'Country'))%>%
    mutate(unique_id = paste0(iso_alpha3, Date))%>%
    ungroup()
  }


confirmed_tidy <- tidy_CSSE_admin0(tidy_CSSE(Confirmed, value_col = 'Confirmed'), value_col = 'Confirmed')

deaths_tidy <- tidy_CSSE_admin0(tidy_CSSE(Deaths, value_col = 'Deaths'), value_col = 'Deaths')

recovered_tidy <- tidy_CSSE_admin0(tidy_CSSE(Recovered, value_col = 'Recovered'), value_col = 'Recovered')

all_data_admin0 <- left_join(confirmed_tidy,deaths_tidy, by = c('unique_id' = 'unique_id'))%>%
  left_join(recovered_tidy, by = c('unique_id' = 'unique_id'))%>%
  select(-contains("."))%>%
  select(admin0, iso_alpha3, Date, Confirmed, Deaths, Recovered) %>%
  group_by(admin0) %>%
  mutate(Active = (Confirmed - (Recovered + Deaths)),
         Day = row_number(dplyr::na_if(Confirmed, 0)),
         `Daily percentage change` = ifelse(Confirmed > 5,
                                            (Confirmed/lag(Confirmed)-1)*100, 
                                            NA
                                            ),
         `Daily change of daily percentage change` = `Daily percentage change` - lag(`Daily percentage change`)
         ) 



chart_list = c("Switzerland", "Italy", "China", "France", "Poland", "United Kingdom", "Spain")

all_data_admin0 %>%
  filter(admin0 %in% chart_list) %>%
  ggplot(aes(x=Date, y=Confirmed, colour=admin0)) + geom_line() + theme_minimal() + 
  scale_y_log10(labels = scales::comma) + ylab("Confirmed [log]") +
  labs(title = "Confirmed cases by date", colour = "Country") 
  
ggsave(filename = "confirmed by date.png")

all_data_admin0 %>%
  filter(admin0 %in% chart_list) %>%
  ggplot(aes(x=Day, y=Confirmed, colour=admin0)) + geom_line() + theme_minimal() + 
#  scale_y_log10(labels = scales::comma) +
  labs(title = "Confirmed cases by day since first case")

ggsave(filename = "confirmed by day.png")
