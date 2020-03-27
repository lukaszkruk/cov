some covid-19 charts
================

## Parameters

All data points where confirmed cases are below the `threshold` are
discarded, which makes for cleaner charts. This is because the number of
cases tends to exhibit nice exponential behaviour from 10 or so cases
upwards. `chart_list` defines which countries to chart. Duh.

``` r
threshold = 10
chart_list = c("Switzerland", "Italy", "China", "Armenia")
```

## Load data

``` r
library(tidyverse)


Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
Deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
Recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
isoalpha3 <- read_csv('https://gist.githubusercontent.com/tadast/8827699/raw/7255fdfbf292c592b75cf5f7a19c16ea59735f74/countries_codes_and_coordinates.csv')%>%
  select(Country, `Alpha-3 code`) %>%
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
```

    ## Warning: Using `as.character()` on a quosure is deprecated as of rlang 0.3.0.
    ## Please use `as_label()` or `as_name()` instead.
    ## This warning is displayed once per session.

``` r
deaths_tidy <- tidy_CSSE_admin0(tidy_CSSE(Deaths, value_col = 'Deaths'), value_col = 'Deaths')
recovered_tidy <- tidy_CSSE_admin0(tidy_CSSE(Recovered, value_col = 'Recovered'), value_col = 'Recovered')
```

## Process

``` r
  left_join(confirmed_tidy,deaths_tidy, by = c('unique_id' = 'unique_id')) %>%
  left_join(recovered_tidy, by = c('unique_id' = 'unique_id')) %>%
  select(admin0 = admin0.x, Date=Date.x, Confirmed, Deaths, Recovered) %>%
  replace_na(list(Recovered = 0, Deaths = 0)) %>%
  group_by(admin0) %>%
  filter(Confirmed > threshold) %>%
  mutate(Active = Confirmed - (Recovered + Deaths),
         Day = row_number(1:n()),
         Mortality = Deaths/Confirmed*100,
         `New cases daily percentage change` = (Confirmed/lag(Confirmed)-1)*100,
         `Active cases daily percentage change` = (Active/lag(Active)-1)*100,
         `Daily change of daily percentage change` = 
           `Active cases daily percentage change` - lag(`Active cases daily percentage change`)
         ) ->
  all_data_admin0
```

## Chart

``` r
all_data_admin0 %>%
  filter(admin0 %in% chart_list) %>%
  ggplot() + geom_line(alpha=.6) + theme_minimal() ->
  covplot

covplot + aes(x=Date, y=Active, colour=admin0) +
  scale_y_log10(labels = scales::comma) + 
  labs(title = "Active cases by date, log scale", colour = "Country") 
```

![](readme_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
covplot + aes(x=Date, y=Active, colour=admin0) +
  aes(x=Day, y=Active, colour=admin0) +
  scale_y_continuous(labels = scales::comma) + labs(title = paste("Active cases by day, since the day", threshold, "cases were first recorded"))
```

![](readme_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
covplot + 
  aes(x=Date, y=`New cases daily percentage change`, colour=admin0)+ geom_smooth(se=F) +
  ylim(0, 50) +
  labs(title = "Number of new cases compared to previous day, per cent", 
       caption = "Anything above zero means new cases are being recorded. 
       Daily change of 26% means doubling of case number every 3 days.")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_path).

    ## Warning: Removed 22 rows containing missing values (geom_smooth).

![](readme_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
covplot + 
  aes(x=Date, y=`Daily change of daily percentage change`, colour=admin0) + 
  geom_smooth(se=F) + ylim(-10, 10) +
  labs(title = "Change of speed of increase of active cases", 
     caption = "Positive values mean that active cases are increasing at an increasing rate. 
     Negative values mean that active cases are increasing at a decreasing rate. 
     Well, not really. The math is off for now.")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 51 rows containing non-finite values (stat_smooth).

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : span too small. fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 18342

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.025

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 9.1506

    ## Warning: Removed 17 rows containing missing values (geom_path).

    ## Warning: Removed 11 rows containing missing values (geom_smooth).

![](readme_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

## To do

  - switch data source to ECDC or JHU?

  - add country population, compute cases per capita

  - compute and chart moving window weekly (3-day?) averages, because
    these charts are a mess

  - labels
    
      - explicitly add x-label for the most recent date
      - title, description, data source, etc
      - do something about verical grids and x-labels, show weeks more
        clearly

  - add key events and a visual link to possible case rate-of-change
    change

  - add shiny interface
    
      - selector for countries
      - selector for linear and log y-scales
      - selector for x-axis metric - date vs “days since first case”
      - add a “timeshift” tool so you can move one country to compare
        how many days behind another country it is

#### Done

  - add a “days since first case” metric as an alternative x-axis to
    actual date
  - add deaths and recovered cases, compute active cases
