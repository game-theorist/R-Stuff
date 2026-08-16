library(readxl)
library(tidyverse)
library(janitor)
library(infer)

clanfolk_recipes <- read_xlsx("G:/My Drive/Game Theory/Clanfolk/Food recipes.xlsx")

livestock_output <- read_xlsx("G:/My Drive/Game Theory/Clanfolk/Livestock Output.xlsx")

grain_yield <- 10

vegetable_yield <- 6

harvests_per_year <- 3

# Livestock data wrangling

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


# Ingredients per day per clanfolk

extra_nutrition_necessity <- 1

# Tibble of eaters

eaters <- tribble(
  ~"dish", ~"normal_eater", ~"fast_eater", ~"slow_eater",
  "hearty_stew", 1, 1, 1,
  "fish_stew", 2, 2, 2,
  "haggis", 0, 0, 0,
  "haggis_and_neeps", 1, 1, 1
  )

nutrition_need_per_day <- eaters |> 
  mutate(total_nutrition_need = 1800 * ((sum(normal_eater) * (5 + extra_nutrition_necessity)) + (sum(fast_eater) * (7 + extra_nutrition_necessity)) + (sum(slow_eater) * (3 + extra_nutrition_necessity)))
  ) |> 
  pull()

ingredients_per_day <- clanfolk_recipes |> 
  left_join(eaters, by = "dish") |> 
  rowwise() |> 
  mutate(dish_needed_nutrition = 
           1800 * ((normal_eater * (5 + extra_nutrition_necessity)) + (fast_eater * (7 + extra_nutrition_necessity)) + (slow_eater * (3 + extra_nutrition_necessity))),
         .after = "dish"
         ) |> 
  mutate(dish_needed_per_day = (dish_needed_nutrition / nutrition_per_unit),
         .after = "dish_needed_nutrition"
  ) |> 
  mutate(across(!c(dish, nutrition_per_unit, dish_needed_per_day),
                ~ .x * dish_needed_per_day)) |>
  # Plants
  mutate(across(oat_grain,
                ~ (.x / (grain_yield * harvests_per_year / 40)),
                .names = "{str_remove(.col, '_grain')}_plants"),
         across(c(broad_beans, onion, neeps, kail),
                ~ (.x / (vegetable_yield * harvests_per_year / 40)),
                .names = "{.col}_plants")
  ) |> 
  remove_empty("cols") |>
  # Animals
  mutate(pig_females_pluck = (pluck / pig$pluck_per_day_per_female),
         pig_females_meat = (raw_meat / pig$meat_per_day_per_female)
         ) |> 
  select(!c(nutrition_per_unit, dish_needed_nutrition, normal_eater, fast_eater, slow_eater))

ingredients_per_day

nutrition_necessity_per_day