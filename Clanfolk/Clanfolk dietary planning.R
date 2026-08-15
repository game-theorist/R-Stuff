library(readxl)
library(tidyverse)
library(janitor)
library(infer)

clanfolk_recipes <- read_xlsx("G:/My Drive/Game Theory/Clanfolk/Food recipes.xlsx")

livestock_output <- read_xlsx("G:/My Drive/Game Theory/Clanfolk/Livestock Output.xlsx")

grain_yield <- 10

vegetable_yield <- 6

harvests_per_year <- 3

#livestock data wrangling

clanfolk_livestock <- livestock_output |>
  #cleaning the tibble
  filter(str_detect(animal, "_adult")) |>
  rowwise() %>%
  mutate(litter_size = list(eval(parse(text = litter_size)))
  ) |> 
  unnest(litter_size) |> 
  #creating new variables
  group_by(animal) |> 
  summarize(across(c(feed_per_day, pregnancy_time, growth_time, fresh_hide, raw_meat, pluck, milk, wool),
                   ~ mean(.x, na.rm = TRUE)),
            litter_size_mean = mean(litter_size, na.rm = TRUE),
            litter_size_sd = sd(litter_size, na.rm =TRUE),
            babies_per_day_per_female = (litter_size_mean / (pregnancy_time + growth_time)),
            pluck_per_day_per_female = pluck * babies_per_day_per_female,
            meat_per_day_per_female = raw_meat * babies_per_day_per_female
  ) |> 
  select(!c(feed_per_day, fresh_hide, raw_meat, pluck, milk, wool))

cattle <- clanfolk_livestock |> 
  filter(animal == "cattle_adult")

pig <- clanfolk_livestock |> 
  filter(animal == "pig_adult")


# ingredients per day per clanfolk

clanfolk_n <- 14

clanfolk_fast_metabolism <- 3

clanfolk_slow_metabolism <- 2

extra_nutrition_necessity <- 1

#desired_dishes <- c("hearty_stew")

nutrition_necessity_per_day <- clanfolk_n * 1800 * (5 + (clanfolk_fast_metabolism * 2) - (clanfolk_slow_metabolism * 2) + extra_nutrition_necessity)

ingredients_per_day <- clanfolk_recipes |> 
  #filter(dish == desired_dishes) |> 
  mutate(dishes_needed_per_day = (nutrition_necessity_per_day / nutrition_per_unit),
         .after = "dish"
  ) |> 
  mutate(across(!c(dish, nutrition_per_unit, dishes_needed_per_day),
                ~ .x * dishes_needed_per_day)) |>
  #plants
  mutate(across(oat_grain,
                ~ (.x / (grain_yield * harvests_per_year / 40)),
                .names = "{str_remove(.col, '_grain')}_plants"),
         across(c(broad_beans, onion, neeps, kail),
                ~ (.x / (vegetable_yield * harvests_per_year / 40)),
                .names = "{.col}_plants")
  ) |> 
  remove_empty("cols") |>
  #animals
  mutate(pig_females_pluck = (pluck / pig$pluck_per_day_per_female),
         pig_females_meat = (raw_meat / pig$meat_per_day_per_female)
         ) |> 
  select(!c(nutrition_per_unit))

ingredients_per_day

nutrition_necessity_per_day