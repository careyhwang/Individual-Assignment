---
author: Hanrui Wang
date: "2021-10-08"
description: Data from Fred
math: true
title: Changes of CPI and Its Components
---


## Data Processing


### Find [CPI components at FRED](https://fredaccount.stlouisfed.org/public/datalist/843).

```{r,load_movies, warning=FALSE, message=FALSE}
movies <- read_csv(here::here("data", "movies.csv"))
```

### Data Cleaning

1. Generate a vector of components, and then pass it to `tidyquant::tq_get(get = "economic.data", from =  "2000-01-01")` to get all data since January 1, 2000

1. Since the data downloaded is an index with various starting dates, we have to calculate the yearly, or 12-month change, using the `lag` function, and specifically, `year_change = value/lag(value, 12) - 1`, which means comparing the current month's value with that 12 months ago lag(value, 12).
1. Order components so the higher the yearly change, the earlier does that component appear.
1. Make sure that the **All Items** CPI (CPIAUCSL) appears first.
1. Add a `geom_smooth()` for each component to get a sense of the overall trend.
1. Colour the points according to whether yearly change was positive or negative. 

```{r}
url <- "https://fredaccount.stlouisfed.org/public/datalist/843"
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")
economic.data <- map(tables, . %>% 
             html_table(fill=TRUE)%>% 
             janitor::clean_names())
#economic.data, component is "list"
  # list of CPI component
component <- economic.data[[2]]
CPI.components <- component[2]
CPI.vector <- as.vector(unlist(CPI.components, use.names=FALSE))
k<-component[,1:2]
names(k)[2]<-"symbol"

# Pass symbols to tq_get to get economic data
FRED_data_m <- CPI.vector %>%
  tidyquant::tq_get(get = "economic.data", from =  "2000-01-01")

FRED_data<-left_join(k,FRED_data_m,by="symbol")
FRED_data$title<-gsub("Consumer Price Index for All Urban Consumers: ","",FRED_data$title)
FRED_data$title<-gsub("in U.S. City Average","",FRED_data$title)

FRED_data$year_change = FRED_data$price/(lag(FRED_data$price,12))-1
FRED_data<-FRED_data %>% 
  filter(year(date) %in% c(2016,2017,2018,2019,2020,2021)) %>% 
  mutate(z= if_else(year_change > 0,1,0)) %>% 
  group_by(title) %>% 
  mutate(m=max(year_change)) %>% 
  arrange(desc(m))

```
![](CPI.jpg)





