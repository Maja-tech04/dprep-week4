# Exercise 6

library(tidyverse)

dir.create("temp", showWarnings = FALSE)

video_view <- read_csv("video_view.csv")
creators <- read_csv("creators.csv")

creator_week4 <- video_view %>%
  group_by(creator_id) %>%
  summarise(
    videos_n = n(),
    impressions_total = sum(impressions_n, na.rm = TRUE),
    watched_total = sum(watched_n, na.rm = TRUE),
    avg_watch_rate = mean(watch_rate, na.rm = TRUE) 
  )