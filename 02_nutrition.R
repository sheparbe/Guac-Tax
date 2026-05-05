library(tidyverse)
library(janitor)

dir.create("data", showWarnings = FALSE)

nutrition_raw <- tribble(
  ~item_name,                    ~portion_oz, ~calories, ~fat_g, ~sat_fat_g, ~trans_fat_g, ~cholesterol_mg, ~sodium_mg, ~carbs_g, ~fiber_g, ~sugar_g, ~protein_g,
  "Flour Tortilla (Burrito)",    1,           320,       9,      0.5,        0,            0,               600,        50,       3,        0,        8,
  "Flour Tortilla (Taco)",       1,           80,        2.5,    0,          0,            0,               160,        13,       1,        0,        2,
  "Crispy Corn Tortilla",        1,           70,        3,      0,          0,            0,               0,          10,       1,        0,        1,
  "Cilantro-Lime Brown Rice",    4,           210,       6,      0,          0,            0,               190,        36,       2,        0,        4,
  "Cilantro-Lime White Rice",    4,           210,       4,      1,          0,            0,               350,        40,       1,        0,        4,
  "Black Beans",                 4,           130,       1.5,    0,          0,            0,               210,        22,       7,        2,        8,
  "Pinto Beans",                 4,           130,       1.5,    0,          0,            0,               210,        21,       8,        1,        8,
  "Fajita Vegetables",           2,           20,        0,      0,          0,            0,               150,        5,        1,        2,        1,
  "Barbacoa",                    4,           170,       7,      2.5,        0,            65,              530,        2,        1,        0,        24,
  "Chicken",                     4,           180,       7,      3,          0,            125,             310,        0,        0,        0,        32,
  "Carnitas",                    4,           210,       12,     7,          0,            65,              450,        0,        0,        0,        23,
  "Steak",                       4,           150,       6,      2.5,        0,            80,              330,        1,        1,        0,        21,
  "Sofritas",                    4,           150,       10,     1.5,        0,            0,               560,        9,        3,        5,        8,
  "Fresh Tomato Salsa",          4,           25,        0,      0,          0,            0,               550,        4,        1,        1,        0,
  "Roasted Chili-Corn Salsa",    4,           80,        1.5,    0,          0,            0,               330,        16,       3,        4,        3,
  "Tomatillo-Green Chili Salsa", 2,           15,        0,      0,          0,            0,               260,        4,        0,        2,        0,
  "Tomatillo-Red Chili Salsa",   2,           30,        0,      0,          0,            0,               500,        4,        1,        0,        0,
  "Guacamole",                   4,           230,       22,     3.5,        0,            0,               370,        8,        6,        1,        2,
  "Guacamole (Large)",           8,           460,       44,     7,          0,            0,               740,        16,       12,       2,        4,
  "Cheese",                      1,           110,       8,      5,          0,            30,              190,        1,        0,        0,        6,
  "Sour Cream",                  2,           110,       9,      7,          0,            40,              30,         2,        0,        2,        2,
  "Queso Blanco (Entree)",       2,           120,       9,      6,          0,            30,              250,        4,        0,        1,        5,
  "Queso Blanco (Side)",         4,           240,       18,     12,         1,            60,              490,        7,        0,        2,        10,
  "Romaine Lettuce",             1,           5,         0,      0,          0,            0,               0,          1,        1,        0,        0,
  "Supergreens Salad Mix",       3,           15,        0,      0,          0,            0,               15,         3,        2,        1,        1,
  "Chips (Regular)",             4,           540,       25,     3.5,        0,            0,               390,        73,       7,        1,        7,
  "Chips (Large)",               6,           810,       38,     5,          0,            0,               590,        110,      11,       2,        11,
  "Chipotle-Honey Vinaigrette",  2,           220,       16,     2.5,        0,            0,               850,        18,       1,        12,       1
)

nutrition_clean <- nutrition_raw |>
  mutate(
    protein_per_100cal = round((protein_g  / calories) * 100, 2),
    fiber_per_100cal   = round((fiber_g    / calories) * 100, 2),
    sodium_per_100cal  = round((sodium_mg  / calories) * 100, 2),
    fat_per_100cal     = round((fat_g      / calories) * 100, 2),
    is_guacamole       = str_detect(item_name, regex("guacamole", ignore_case = TRUE)),
    category = case_when(
      str_detect(item_name, regex("tortilla|chips", ignore_case = TRUE)) ~ "Tortillas & Chips",
      str_detect(item_name, regex("rice|beans|vegeta", ignore_case = TRUE)) ~ "Rice, Beans & Veggies",
      item_name %in% c("Barbacoa","Chicken","Carnitas","Steak","Sofritas") ~ "Proteins",
      str_detect(item_name, regex("salsa|vinaigrette", ignore_case = TRUE)) ~ "Salsas & Dressings",
      str_detect(item_name, regex("guac|cheese|cream|queso|lettuce|greens", ignore_case = TRUE)) ~ "Toppings",
      TRUE ~ "Other"
    ),
    data_source     = "Chipotle Official Nutrition PDF (March 2025)",
    extraction_date = Sys.Date()
  )

nutrition_validated <- nutrition_clean |>
  mutate(
    flag_zero_calories   = calories <= 0,
    flag_high_calories   = calories > 2000,
    flag_missing_protein = is.na(protein_g),
    flag_negative_values = if_any(c(fat_g, carbs_g, sodium_mg), \(x) x < 0),
    flag_any_issue       = flag_zero_calories | flag_high_calories |
      flag_missing_protein | flag_negative_values
  )

cat("\n── Validation Summary ──────────────────────────────\n")
nutrition_validated |>
  summarise(
    total_items   = n(),
    guac_items    = sum(is_guacamole),
    flagged_items = sum(flag_any_issue),
    categories    = n_distinct(category)
  ) |>
  print()

cat("\nBy category:\n")
nutrition_validated |> count(category) |> print()

cat("\nGuacamole rows:\n")
nutrition_validated |>
  filter(is_guacamole) |>
  select(item_name, portion_oz, calories, fat_g,
         fiber_g, protein_g, protein_per_100cal) |>
  print()

cat("Flagged rows:", sum(nutrition_validated$flag_any_issue), "\n")

write_csv(nutrition_validated, "data/chipotle_nutrition_clean.csv")
cat("\n✓ Saved: data/chipotle_nutrition_clean.csv\n")


