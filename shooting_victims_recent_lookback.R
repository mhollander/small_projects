library(ggplot2)
library(tidyverse)
library(lubridate)

shooting_victims <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*,+ST_Y(the_geom)+AS+lat,+ST_X(the_geom)+AS+lng+FROM+shootings&filename=shootings&format=csv&skipfields=cartodb_id") %>%
  filter(!is.na(fatal)) %>% 
  complete(date_ = seq(min(date_), 
                       max(date_), by = '1 day'), fatal)


rolling_victims <- function(df) {
  df %>% 
    group_by(date_) %>% 
    summarize(fs = sum(fatal == 1 & !is.na(objectid), na.rm = T),
              nfs = sum(fatal == 0 & !is.na(objectid), na.rm = T)) %>% 
    replace_na(list(fs = 0, nfs = 0)) %>% 
    arrange(date_) %>% 
    mutate(across(.cols = c(fs, nfs), .fns = list(~RcppRoll::roll_sum(.x, n = 7, fill = NA),
                                                  ~RcppRoll::roll_sum(.x, n = 30, fill = NA),
                                                  ~RcppRoll::roll_sum(.x, n = 90, fill = NA)))) %>% 
    rename(fs_7_day = fs_1,
           fs_30_day = fs_2,
           fs_90_day = fs_3,
           nfs_7_day = nfs_1,
           nfs_30_day = nfs_2,
           nfs_90_day = nfs_3,
    ) %>% 
    pivot_longer(names_to = "name", values_to = "value",  cols = -date_) %>% 
    mutate(type = if_else(grepl("^fs", name), "Fatal", "Non-Fatal"))
}

shooting_victims %>% 
  rolling_victims() %>% 
  group_by(name) %>% 
  mutate(prepandemic_avg = mean(value[date_ <= "2020-03-11"], na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = as.Date(date_), y = value, color = name)) + 
  geom_point() +
  geom_hline(data = . %>% distinct(prepandemic_avg, name, .keep_all = T), aes(yintercept = prepandemic_avg, color = name)) +
  geom_vline(aes(xintercept = ymd("2020-03-11")), color = "red", linetype = "dashed", alpha = .5) +
  annotate("text", x = ymd("2020-03-11"), y = Inf, angle = 90, label = "COVID-19 Pandemic Declared", vjust = "left", hjust = "right", alpha = .5) + 
  facet_wrap(~type, scales = "free_y") +
  labs(title = "Fatal and Non-Fatal Shootings in Philadelphia",
       subtitle = "1-, 7-, 30-, and 90-day running totals",
       x = "", y = "") 


shooting_victims %>% 
  rolling_victims() %>% 
  filter(!grepl("90", name)) %>% 
  ggplot(aes(x = date_, y = value, color = name)) + 
  geom_point() + 
  facet_wrap(~type, scales = "free_y") +
  labs(title = "Fatal and Non-Fatal Shootings in Philadelphia",
       subtitle = "1-, 7-, and 30-day running totals",
       x = "", y = "")
  
  
shooting_victims %>%
  rolling_victims() %>% 
  filter(name %in% c("fs", "nfs")) %>% 
  mutate(no_shooting = if_else(value == 0, "No Shooting", "Shooting")) %>% 
  #filter(name == "fs") %>% 
  ggplot(aes(x = date_, y = no_shooting)) + 
  geom_point(size = .05, ) + 
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  labs(title = "Days Without a Shooting in Philadelphia",
       x = "", y = "")
