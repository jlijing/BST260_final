library(tidyverse)
library(excessmort)
library(MASS)

weekly_death_counts <- puerto_rico_counts |>
  mutate(year = year(date), data = as.Date(date)) |>
  mutate(date = floor_date(date, week_start = 3, unit = "week")) |>
  group_by(date, sex, agegroup) |>
  summarize(outcome = sum(outcome), population = mean(population), 
            n = n(), .groups = "drop") |> filter(n == 7)

saveRDS(weekly_death_counts, "data/weekly_death_counts.rds")

data_by_age <- weekly_death_counts |>
  filter(between(year(date), 2007, 2018)) |> 
  mutate(agegroup = fct_collapse(agegroup, 
                                 "0-4" = c("0-4"),
                                 "5-14" = c("5-9", "10-14"),
                                 "15-29" = c("15-19","20-24","25-29"),
                                 "30-54" = c("30-34", "35-39", "40-44"),
                                 "45-54" = c("45-49","50-54"),
                                 "55-59" = c("55-59"),
                                 "60-64" = c("60-64"),
                                 other_level = "65+")) |>
  group_by(date, agegroup, sex) |>
  summarize(outcome = sum(outcome),
            population = sum(population), .groups = "drop") |>
  mutate(diftime = difftime(date, min(date), units =  "days"),
         week = epiweek(date), year = year(date), rate = outcome/population)

saveRDS(data_by_age,"data/weekly_death_counts_by_age.rds")

