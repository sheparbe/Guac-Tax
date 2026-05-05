---
title: "r_01_scrape"
author: "Bryce Shepard"
date: "2026-05-05"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(rvest)
library(tidyverse)
library(tidycensus)
library(janitor)

census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

base <- "https://locations.chipotle.com"

# ── 1. Get all city links for Ohio ─────────────────────────────────────────
cat("Scraping Ohio cities...\n")

city_links <- paste0(base, "/oh") |>
  read_html() |>
  html_elements("div.container.mt-12.pb-16 > ul > li > a") |>
  html_attr("href") |>
  url_absolute(base = paste0(base, "/oh"))

cat("Cities found:", length(city_links), "\n")

# ── 2. Get all store URLs from each city ───────────────────────────────────
cat("Scraping store URLs from each city...\n")

all_store_urls <- c()

for (city_url in city_links) {
  Sys.sleep(0.5)
  store_links <- tryCatch({
    city_url |>
      read_html() |>
      html_elements("a") |>
      html_attr("href") |>
      keep(\(x) str_detect(x, "^\\.\\./[a-z]{2}/")) |>
      url_absolute(base = city_url)
  }, error = \(e) character(0))
  all_store_urls <- c(all_store_urls, store_links)
}

all_store_urls <- unique(all_store_urls)
cat("Total Ohio store URLs:", length(all_store_urls), "\n")
head(all_store_urls, 5) |> print()

# ── 3. Scrape each store page for address details ──────────────────────────
cat("\nScraping store details...\n")

scrape_store <- function(url) {
  Sys.sleep(0.75)
  page <- tryCatch(read_html(url), error = \(e) NULL)
  if (is.null(page)) return(NULL)

  # Try JSON-LD first
  json_ld_text <- page |>
    html_elements("script[type='application/ld+json']") |>
    html_text()

  for (j in json_ld_text) {
    parsed <- tryCatch(
      jsonlite::fromJSON(j, simplifyVector = FALSE),
      error = \(e) NULL
    )
    if (is.null(parsed)) next
    addr <- parsed$address
    if (!is.null(addr$postalCode)) {
      return(tibble(
        store_url = url,
        name      = parsed$name             %||% NA_character_,
        address   = addr$streetAddress      %||% NA_character_,
        city      = addr$addressLocality    %||% NA_character_,
        state     = addr$addressRegion      %||% NA_character_,
        postcode  = addr$postalCode         %||% NA_character_,
        latitude  = as.numeric(parsed$geo$latitude  %||% NA),
        longitude = as.numeric(parsed$geo$longitude %||% NA),
        phone     = parsed$telephone        %||% NA_character_
      ))
    }
  }

  # Fallback: visible HTML elements
  tibble(
    store_url = url,
    name      = page |> html_element("span.LocationName-brand") |>
                html_text(trim = TRUE) %||% NA_character_,
    address   = page |> html_element("span.c-address-street-1") |>
                html_text(trim = TRUE) %||% NA_character_,
    city      = page |> html_element("span.c-address-city") |>
                html_text(trim = TRUE) %||% NA_character_,
    state     = page |> html_element("abbr.c-address-state") |>
                html_text(trim = TRUE) %||% NA_character_,
    postcode  = page |> html_element("span.c-address-postal-code") |>
                html_text(trim = TRUE) %||% NA_character_,
    latitude  = NA_real_,
    longitude = NA_real_,
    phone     = page |> html_element("div.Phone-display") |>
                html_text(trim = TRUE) %||% NA_character_
  )
}

locations_raw <- all_store_urls |>
  map(possibly(scrape_store, otherwise = NULL)) |>
  list_rbind()

cat("Stores scraped:", nrow(locations_raw), "\n")

dir.create("data", showWarnings = FALSE)
write_csv(locations_raw, "data/chipotle_locations_raw.csv")
cat("✓ Saved raw\n")

# ── 4. Clean ───────────────────────────────────────────────────────────────
cat("\nCleaning...\n")

locations_clean <- locations_raw |>
  clean_names() |>
  mutate(
    # Use postcode instead of zip to avoid base R zip() conflict
    zip_code  = str_extract(postcode, "^\\d{5}"),
    state_cd  = str_to_upper(str_trim(state)),
    city_name = str_to_title(str_trim(city))
  ) |>
  filter(!is.na(zip_code), str_length(zip_code) == 5) |>
  distinct(address, city_name, state_cd, .keep_all = TRUE) |>
  mutate(store_id = row_number()) |>
  select(store_id, store_url, name, address,
         city = city_name, state = state_cd,
         zip = zip_code, latitude, longitude, phone)

cat("Locations after cleaning:", nrow(locations_clean), "\n")

# ── 5. Pull Census income by zip ───────────────────────────────────────────
cat("\nPulling Census income data for Ohio zips...\n")

ohio_zips <- unique(locations_clean$zip)
cat("Unique Ohio zip codes:", length(ohio_zips), "\n")

income_by_zip <- get_acs(
  geography = "zcta",
  variables = "B19013_001",
  year      = 2022,
  survey    = "acs5",
  progress  = FALSE
) |>
  clean_names() |>
  transmute(
    zip           = str_extract(geoid, "\\d{5}"),
    median_income = estimate,
    income_moe    = moe
  ) |>
  filter(zip %in% ohio_zips)

cat("Zips matched to Census:", nrow(income_by_zip), "\n")

# ── 6. Join ────────────────────────────────────────────────────────────────
locations_final <- locations_clean |>
  left_join(income_by_zip, by = "zip") |>
  mutate(
    income_quintile = ntile(median_income, 5),
    income_quintile_label = case_when(
      income_quintile == 1 ~ "Q1 Lowest",
      income_quintile == 2 ~ "Q2 Low",
      income_quintile == 3 ~ "Q3 Middle",
      income_quintile == 4 ~ "Q4 High",
      income_quintile == 5 ~ "Q5 Highest",
      TRUE                 ~ "Unknown"
    )
  )

# ── 7. Validation ──────────────────────────────────────────────────────────
cat("\n── Validation Summary ──────────────────────────────\n")

locations_final |>
  summarise(
    total_stores         = n(),
    has_zip              = sum(!is.na(zip)),
    has_coordinates      = sum(!is.na(latitude) & !is.na(longitude)),
    has_income           = sum(!is.na(median_income)),
    income_join_rate_pct = round(mean(!is.na(median_income)) * 100, 1),
    avg_median_income    = round(mean(median_income, na.rm = TRUE), 0),
    stores_q1_lowest     = sum(income_quintile_label == "Q1 Lowest", na.rm = TRUE),
    stores_q5_highest    = sum(income_quintile_label == "Q5 Highest", na.rm = TRUE)
  ) |>
  print()

locations_validated <- locations_final |>
  mutate(
    flag_missing_zip    = is.na(zip),
    flag_missing_income = is.na(median_income),
    flag_missing_coords = is.na(latitude) | is.na(longitude),
    flag_any_issue      = flag_missing_zip | flag_missing_income |
                          flag_missing_coords
  )

cat("Flagged rows:", sum(locations_validated$flag_any_issue), "\n")

# ── 8. Save ────────────────────────────────────────────────────────────────
write_csv(locations_validated, "data/chipotle_locations_clean.csv")

cat("\n✓ Saved: data/chipotle_locations_clean.csv\n")
cat("\nSample rows:\n")
locations_validated |>
  select(store_id, city, state, zip, median_income, income_quintile_label) |>
  head(10) |>
  print()
```


Next block
```{r}
# ── 3. Parse store info directly from URLs ─────────────────────────────────
cat("Parsing store data from URLs...\n")

stores <- tibble(store_url = all_store_urls) |>
  mutate(
    # Extract path segments from URL
    path    = str_remove(store_url, "https://locations.chipotle.com/"),
    state   = str_split_fixed(path, "/", 3)[,1] |> str_to_upper(),
    city    = str_split_fixed(path, "/", 3)[,2] |>
              str_replace_all("-", " ") |>
              str_to_title(),
    address = str_split_fixed(path, "/", 3)[,3] |>
              str_replace_all("-", " ") |>
              str_to_title(),
    store_id = row_number()
  ) |>
  select(store_id, store_url, state, city, address)

cat("Stores parsed:", nrow(stores), "\n")
head(stores, 10) |> print()

# ── 4. Pull Census income at city (place) level for Ohio ───────────────────
cat("\nPulling Census income data for Ohio cities...\n")

ohio_income <- get_acs(
  geography = "place",
  variables = "B19013_001",
  state     = "OH",
  year      = 2022,
  survey    = "acs5",
  progress  = FALSE
) |>
  clean_names() |>
  transmute(
    city          = str_remove(name, " city, Ohio| village, Ohio| Ohio| CDP, Ohio") |>
                    str_to_title() |>
                    str_trim(),
    median_income = estimate,
    income_moe    = moe
  )

cat("Ohio cities with income data:", nrow(ohio_income), "\n")
head(ohio_income, 10) |> print()

# ── 5. Join stores to income ───────────────────────────────────────────────
cat("\nJoining stores to income data...\n")

stores_with_income <- stores |>
  left_join(ohio_income, by = "city") |>
  mutate(
    income_quintile = ntile(median_income, 5),
    income_quintile_label = case_when(
      income_quintile == 1 ~ "Q1 Lowest",
      income_quintile == 2 ~ "Q2 Low",
      income_quintile == 3 ~ "Q3 Middle",
      income_quintile == 4 ~ "Q4 High",
      income_quintile == 5 ~ "Q5 Highest",
      TRUE                 ~ "Unknown"
    )
  )

# ── 6. Validation ──────────────────────────────────────────────────────────
cat("\n── Validation Summary ──────────────────────────────\n")

stores_with_income |>
  summarise(
    total_stores         = n(),
    matched_to_income    = sum(!is.na(median_income)),
    unmatched            = sum(is.na(median_income)),
    income_join_rate_pct = round(mean(!is.na(median_income)) * 100, 1),
    avg_median_income    = round(mean(median_income, na.rm = TRUE), 0),
    stores_q1_lowest     = sum(income_quintile_label == "Q1 Lowest", na.rm = TRUE),
    stores_q5_highest    = sum(income_quintile_label == "Q5 Highest", na.rm = TRUE)
  ) |>
  print()

stores_validated <- stores_with_income |>
  mutate(
    flag_missing_income = is.na(median_income),
    flag_any_issue      = flag_missing_income
  )

cat("Flagged rows:", sum(stores_validated$flag_any_issue), "\n")

# Show unmatched cities so we can fix them
cat("\nUnmatched cities:\n")
stores_validated |>
  filter(flag_missing_income) |>
  distinct(city) |>
  print(n = 50)

# ── 7. Save ────────────────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE)
write_csv(stores_validated, "data/chipotle_locations_clean.csv")
cat("\n✓ Saved: data/chipotle_locations_clean.csv\n")

cat("\nSample rows:\n")
stores_validated |>
  select(store_id, city, state, address, median_income, income_quintile_label) |>
  head(10) |>
  print()
```

```{r}
# Pull township-level income for the 5 unmatched places
ohio_townships <- get_acs(
  geography = "county subdivision",
  variables = "B19013_001",
  state     = "OH",
  year      = 2022,
  survey    = "acs5",
  progress  = FALSE
) |>
  clean_names() |>
  transmute(
    city          = str_remove(name, " township, .*| city, .*| village, .*") |>
                    str_to_title() |>
                    str_trim(),
    median_income = estimate,
    income_moe    = moe
  ) |>
  filter(city %in% c("Anderson", "Boardman", "Concord",
                     "Fairfield", "Liberty"))

cat("Township income data found:\n")
print(ohio_townships)

# Patch the unmatched rows
stores_validated <- stores_validated |>
  left_join(
    ohio_townships |> rename(income_patch = median_income,
                             moe_patch    = income_moe),
    by = "city"
  ) |>
  mutate(
    median_income = coalesce(median_income, income_patch),
    income_moe    = coalesce(income_moe,    moe_patch)
  ) |>
  select(-income_patch, -moe_patch) |>
  mutate(
    income_quintile = ntile(median_income, 5),
    income_quintile_label = case_when(
      income_quintile == 1 ~ "Q1 Lowest",
      income_quintile == 2 ~ "Q2 Low",
      income_quintile == 3 ~ "Q3 Middle",
      income_quintile == 4 ~ "Q4 High",
      income_quintile == 5 ~ "Q5 Highest",
      TRUE                 ~ "Unknown"
    ),
    flag_missing_income = is.na(median_income),
    flag_any_issue      = flag_missing_income
  )

cat("\nAfter township patch:\n")
stores_validated |>
  summarise(
    total_stores         = n(),
    matched_to_income    = sum(!is.na(median_income)),
    unmatched            = sum(is.na(median_income)),
    income_join_rate_pct = round(mean(!is.na(median_income)) * 100, 1)
  ) |>
  print()

# Save final clean file
write_csv(stores_validated, "data/chipotle_locations_clean.csv")
cat("\n✓ Saved: data/chipotle_locations_clean.csv\n")

cat("\nFinal sample:\n")
stores_validated |>
  select(store_id, city, state, address, median_income, income_quintile_label) |>
  head(10) |>
  print()
```