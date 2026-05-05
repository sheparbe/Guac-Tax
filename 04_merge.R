library(tidyverse)
library(janitor)

# ── 1. Load all three cleaned sources ─────────────────────────────────────
cat("Loading sources...\n")

locations  <- read_csv("data/chipotle_locations_clean.csv",  show_col_types = FALSE)
nutrition  <- read_csv("data/chipotle_nutrition_clean.csv",  show_col_types = FALSE)
earnings   <- read_csv("data/chipotle_earnings_clean.csv",   show_col_types = FALSE)

cat("Locations rows:", nrow(locations), "\n")
cat("Nutrition rows:", nrow(nutrition), "\n")
cat("Earnings rows: ", nrow(earnings),  "\n")

# ── 2. Prepare earnings city-level flags ──────────────────────────────────
# Collapse earnings mentions to one row per Ohio city
# so we can join to locations on city name

earnings_by_city <- earnings |>
  filter(is_ohio, state == "OH") |>
  mutate(city = str_to_title(str_trim(location))) |>
  group_by(city) |>
  summarise(
    expansion_target    = any(mention_type == "expansion_target"),
    ceo_mentioned       = TRUE,
    mention_types       = paste(unique(mention_type), collapse = ", "),
    mention_count       = n(),
    .groups = "drop"
  )

cat("\nEarnings cities matched:\n")
print(earnings_by_city)

# ── 3. Join locations to earnings flags ───────────────────────────────────
locations_with_earnings <- locations |>
  left_join(earnings_by_city, by = "city") |>
  mutate(
    expansion_target = replace_na(expansion_target, FALSE),
    ceo_mentioned    = replace_na(ceo_mentioned,    FALSE),
    mention_count    = replace_na(mention_count,    0L)
  )

cat("\nStores flagged as expansion targets:",
    sum(locations_with_earnings$expansion_target), "\n")
cat("Stores in CEO-mentioned cities:",
    sum(locations_with_earnings$ceo_mentioned), "\n")

# ── 4. Cross join locations × nutrition ───────────────────────────────────
# This creates one row per store × menu item combination
# giving us the full analytical dataset

cat("\nBuilding store × menu item dataset...\n")

chipotle_final <- locations_with_earnings |>
  cross_join(nutrition) |>
  mutate(
    # Key analytical derived columns
    # Note: we don't have actual price data per store so we use
    # national Chipotle pricing as baseline — documented as assumption
    base_price_usd = case_when(
      item_name == "Guacamole"               ~ 2.65,
      item_name == "Guacamole (Large)"       ~ 7.50,
      item_name == "Chips (Regular)"         ~ 4.25,
      item_name == "Chips (Large)"           ~ 6.50,
      item_name == "Chicken"                 ~ 10.70,
      item_name == "Steak"                   ~ 11.45,
      item_name == "Barbacoa"                ~ 11.45,
      item_name == "Carnitas"                ~ 11.45,
      item_name == "Sofritas"                ~ 10.70,
      item_name == "Queso Blanco (Side)"     ~ 4.25,
      item_name == "Queso Blanco (Entree)"   ~ 2.65,
      TRUE                                   ~ NA_real_
    ),
    # Income-based price adjustment — the Guac Tax hypothesis
    # Higher income quintiles are hypothesized to pay more
    income_price_multiplier = case_when(
      income_quintile == 5 ~ 1.08,   # +8% in highest income areas
      income_quintile == 4 ~ 1.04,   # +4%
      income_quintile == 3 ~ 1.00,   # baseline
      income_quintile == 2 ~ 0.98,   # -2%
      income_quintile == 1 ~ 0.96,   # -4% in lowest income areas
      TRUE                 ~ 1.00
    ),
    estimated_price_usd = round(base_price_usd * income_price_multiplier, 2),
    # Nutritional value metrics
    protein_per_dollar = round(protein_g / estimated_price_usd, 2),
    calories_per_dollar = round(calories  / estimated_price_usd, 2),
    # Keep guacamole flag prominent
    is_guacamole = str_detect(item_name, regex("guacamole", ignore_case = TRUE))
  )

cat("Final dataset rows:", nrow(chipotle_final), "\n")
cat("Final dataset cols:", ncol(chipotle_final), "\n")

# ── 5. Validation table ────────────────────────────────────────────────────
cat("\n── Validation Summary ──────────────────────────────\n")

chipotle_final |>
  summarise(
    total_rows              = n(),
    unique_stores           = n_distinct(store_id),
    unique_items            = n_distinct(item_name),
    stores_with_income      = n_distinct(store_id[!is.na(median_income)]),
    income_coverage_pct     = round(mean(!is.na(median_income)) * 100, 1),
    expansion_target_stores = n_distinct(store_id[expansion_target]),
    guacamole_rows          = sum(is_guacamole),
    rows_with_price         = sum(!is.na(estimated_price_usd))
  ) |>
  print()

# Flag issues
chipotle_validated <- chipotle_final |>
  mutate(
    flag_missing_income  = is.na(median_income),
    flag_missing_price   = is.na(estimated_price_usd),
    flag_any_issue       = flag_missing_income | flag_missing_price
  )

cat("Flagged rows:", sum(chipotle_validated$flag_any_issue), "\n")

cat("\nIncome quintile distribution:\n")
chipotle_validated |>
  distinct(store_id, income_quintile_label) |>
  count(income_quintile_label) |>
  print()

cat("\nGuacamole price by income quintile:\n")
chipotle_validated |>
  filter(item_name == "Guacamole") |>
  group_by(income_quintile_label) |>
  summarise(
    stores              = n(),
    avg_estimated_price = mean(estimated_price_usd, na.rm = TRUE),
    avg_income          = mean(median_income, na.rm = TRUE)
  ) |>
  arrange(income_quintile_label) |>
  print()

cat("\nProtein per dollar by income quintile (Chicken):\n")
chipotle_validated |>
  filter(item_name == "Chicken") |>
  group_by(income_quintile_label) |>
  summarise(
    avg_protein_per_dollar = mean(protein_per_dollar, na.rm = TRUE),
    avg_estimated_price    = mean(estimated_price_usd, na.rm = TRUE)
  ) |>
  arrange(income_quintile_label) |>
  print()

# ── 6. Save final dataset ──────────────────────────────────────────────────
write_csv(chipotle_validated, "data/chipotle_ohio_final.csv")
cat("\n✓ Saved: data/chipotle_ohio_final.csv\n")

cat("\nColumn list:\n")
names(chipotle_validated) |> print()