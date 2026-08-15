library(readxl)
library(tidyverse)
library(janitor)

clanfolk_recipes <- read_xlsx("G:/My Drive/Game Theory/Clanfolk/Food recipes.xlsx")

# ingredients per day per clanfolk

clanfolk_n <- 13

extra_nutrition_necessity <- 2

desired_dishes <- c("haggis_and_neeps")

nutrition_necessity_per_day <- clanfolk_n * 1800 * (3 + extra_nutrition_necessity)

nutrition_necessity_per_day

ingredient_needs <- clanfolk_recipes |> 
  filter(dish == desired_dishes) |> 
  mutate(dishes_needed_per_day = (nutrition_necessity_per_day / nutrition_per_unit),
         .after = "dish"
  ) |> 
  mutate(across(!c(dish, nutrition_per_unit, dishes_needed_per_day),
                ~ .x * dishes_needed_per_day)) |> 
  select(!c(nutrition_per_unit))

ingredient_needs

#farming

clanfolk_crops <- read_xlsx("G:/My Drive/Game Theory/Clanfolk/Crops.xlsx")

harvests_per_year <- 3

grain_yield <- 10

vegetable_yield <- 6

ingredient_needs |> 
  mutate(across(oat_grain,
                ~ (.x / (grain_yield * harvests_per_year / 40)),
                .names = "{str_remove(.col, '_grain')}_plants"),
         across(c(broad_beans, onion, neeps, kail),
                ~ (.x / (vegetable_yield * harvests_per_year / 40)),
                .names = "{.col}_plants")
  ) |> 
  View()