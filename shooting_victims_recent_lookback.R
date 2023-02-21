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
    mutate(type = if_else(grepl("^fs", name), "Fatal", "Non-Fatal"),
           name = factor(name, levels = c("fs", "fs_7_day", "fs_30_day", "fs_90_day", "nfs", "nfs_7_day", "nfs_30_day", "nfs_90_day")))
}

rolling_shooting_victims_plot <- function(df, ...) {
  
  
  df <- df %>% 
    group_by(name) %>%   mutate(prepandemic_avg = mean(value[date_ <= "2020-03-11"], na.rm = T)) %>% 
    ungroup() 
  
  max_y <- df %>% pull(value) %>% max(na.rm = T)
  max_avg <- df %>% distinct(prepandemic_avg) %>% pull() %>% max(na.rm = T)
  
  df %>%
    ggplot(aes(x = as.Date(date_), y = value, color = name)) +
    geom_point(alpha = .5, size = .8) +
    geom_hline(data = . %>% distinct(prepandemic_avg, name, .keep_all = T), 
               aes(yintercept = prepandemic_avg, color = name)) +
    geom_vline(aes(xintercept = ymd("2020-03-11")), color = "red", linetype = "dashed", alpha = .5) +
    annotate("text", x = ymd("2020-02-15"), y = Inf, angle = 90, label = "COVID-19 Pandemic Declared", vjust = "left", hjust = "right", alpha = .5) +
    annotate("text", x = ymd("2016-01-01"), y = .8*max_y, alpha = .5, label = "Pre-pandemic average", vjust = "top", hjust = "left") +
    annotate("curve", x = ymd("2016-10-01"), xend = ymd("2019-01-01"), y = .77*max_y, yend = max_avg * 1.01,
             arrow = arrow(length = unit(0.02, "npc")), curvature = -.35, alpha = .5) +
    scale_color_discrete(labels = c("fs" = "Daily", "nfs" = "Daily", "fs_7_day" = "7 day", "fs_30_day" = "30 day",
                                    "fs_90_day" = "90 day", "nfs_7_day" = "7 day", "nfs_30_day" = "30 day",
                                    "nfs_90_day" = "90 day")) + 
    scale_x_date(expand = c(.01, .01)) +
    labs(...,
         x = "", y = "", color = "Shooting Count: ") +
    theme_minimal() + 
    theme(plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 17, face = "bold"),
          axis.text = element_text(size = 16),
          axis.ticks = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"))
}

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^fs", name)), 
                              title = "Fatal Shootings in Philadelphia",
                              subtitle = "1-, 7-, 30-, and 90-day running totals") 
ggsave("Fatal_90_day.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^fs", name),
                                       !grepl("90", name)), 
                              title = "Fatal Shootings in Philadelphia",
                              subtitle = "1-, 7-, and 30- day running totals")

ggsave("Fatal_30_day.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^fs", name),
                                       !grepl("90|30", name)), 
                              title = "Fatal Shootings in Philadelphia",
                              subtitle = "1- and 7-day running totals") 
ggsave("Fatal_7_day.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^fs", name),
                                       !grepl("90|30|7", name)), 
                              title = "Fatal Shootings in Philadelphia",
                              subtitle = "Single Day Counts") + 
  theme(legend.position = "none")

ggsave("Fatal_daily.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^nfs", name)), 
                              title = "Non-Fatal Shootings in Philadelphia",
                              subtitle = "1-, 7-, 30-, and 90-day running totals") 
ggsave("non-fatal_90_day.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^nfs", name),
                                       !grepl("90", name)), 
                              title = "Non-Fatal Shootings in Philadelphia",
                              subtitle = "1-, 7-, and 30- day running totals")

ggsave("non-fatal_30_day.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^nfs", name),
                                       !grepl("90|30", name)), 
                              title = "Non-Fatal Shootings in Philadelphia",
                              subtitle = "1- and 7-day running totals") 
ggsave("non-fatal_7_day.png", width = 9, height = 7, bg = "white")

rolling_shooting_victims_plot(shooting_victims %>%
                                rolling_victims() %>% 
                                filter(grepl("^nfs", name),
                                       !grepl("90|30|7", name)), 
                              title = "Non-Fatal Shootings in Philadelphia",
                              subtitle = "Single Day Counts") + 
  theme(legend.position = "none")

ggsave("non-fatal_daily.png", width = 9, height = 7, bg = "white")



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
