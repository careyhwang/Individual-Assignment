---
aliases:
- migrate-from-jekyl
author: Hanrui Wang
categories:
- RMarkdown
tags:
- RMarkdown
date: "2021-10-06"
description: Excess rentals in TfL bike sharing
image: kk.jpg
series:
- Themes Guide
title: TfL Data Analysis
---

Latest TfL data on how many bikes were hired every single day can be got from [London DataStore](https://data.london.gov.uk).

<!--more-->

## Data Cleaning

```{r, get_tfl_data, cache=TRUE}
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week
bike <- bike0 %>% 
  clean_names() %>% 
  rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day, label = TRUE),
          week = isoweek(day))
```

Create a facet grid that plots bikes hired by month and year.

![](tfl_distributions_monthly.png) 


## Excess Rentals in TfL Bike Sharing
### Absolute_Monthly_Change

```{r tfl_absolute_monthly_change_our_coding}
bikerentals<-bike %>% 
  group_by(year,month) %>% 
  summarise(monthlyave=mean(bikes_hired))
bikeave<- bike %>% 
  group_by(month) %>% 
  summarise(totalave=mean(bikes_hired))
bikerentals <-bikerentals %>% 
  filter(year %in% c(2016,2017,2018,2019,2020,2021))
bikerentals$monthlyave<-bikerentals$monthlyave-3015.56
chart<-left_join(bikerentals,bikeave,by="month") 
ggplot(chart, aes(month, monthlyave,group=1)) + facet_wrap(~year) + 
    geom_line(size = 1) + 
    geom_line(aes(month, totalave),colour='#1827e7', size = 1) + 
    theme(legend.position = 'none', strip.background = element_blank(), panel.background = element_blank())+
  
    geom_ribbon(aes(ymin = monthlyave, ymax = pmin(monthlyave, totalave), fill = "positive")) + 
    geom_ribbon(aes(ymin = totalave, ymax = pmin(monthlyave, totalave), fill = "negative")) +
    scale_fill_manual(values=c("#eab5b7", "#cbebce")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 
  
   labs (
    title = "Monthly changes in TfL bike rentals",
    subtitle = "Change from monthly average shown in blue and calculated between 2016 - present",
    x     = "month",
    y = "Bike rentals"
  )+
  NULL
```

![](wan.jpg) 


### Tfl_absolute_weekly_change

looks at percentage changes from the expected level of weekly rentals. The two grey shaded rectangles correspond to Q2 (weeks 14-26) and Q4 (weeks 40-52).



Tables aren't part of the core Markdown spec, but Hugo supports supports them out-of-the-box.

   Name | Age
--------|------
    Bob | 27
  Alice | 23

#### Inline Markdown within tables

| Italics   | Bold     | Code   |
| --------  | -------- | ------ |
| *italics* | **bold** | `code` |








#### Nested list

* Fruit
  * Apple
  * Orange
  * Banana
* Dairy
  * Milk
  * Cheese


