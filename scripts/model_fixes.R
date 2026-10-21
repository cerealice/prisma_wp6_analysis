# ============================================================
# model_fixes.R — model-specific data corrections
# Source this file in any script that needs these helpers.
# ============================================================

# GEM-E3 reports year 2017 as its 2015 base year and year 2021
# as its 2020 value.  Call fix_gem_e3_years() after loading data
# to remap those years to the standard 5-year grid.
fix_gem_e3_years <- function(df) {
  gem_mask <- grepl("GEM.E3", df$model, ignore.case = TRUE)
  df$year[gem_mask & df$year == 2017L] <- 2015L
  df$year[gem_mask & df$year == 2021L] <- 2020L
  df
}

GEM_E3_DISCLAIMER <-
  "GEM-E3 reports values for 2017 and 2021, which we use as a proxy for 2015 and 2020 respectively."

# ── REMIND version deduplication ──────────────────────────────────────────────
#
# When both REMIND-MAgPIE 3.6-4.14 and 3.7-4.14 are present, keep only 3.7.
# Pass any character vector of model names; returns the filtered vector.
filter_latest_remind <- function(models) {
  has_36 <- "REMIND-MAgPIE 3.6-4.14" %in% models
  has_37 <- "REMIND-MAgPIE 3.7-4.14" %in% models
  if (has_36 && has_37) {
    message("Dropping REMIND-MAgPIE 3.6-4.14 (3.7-4.14 is present).")
    models <- models[models != "REMIND-MAgPIE 3.6-4.14"]
  }
  models
}

# ── Sparse-model band masking ──────────────────────────────────────────────────
#
# Some models report only at 10-year intervals after 2050.  After linear
# interpolation onto the 5-year grid, their values at years like 2055, 2065, …
# are artificial.  Including those values in median/min/max band statistics
# produces zig-zag artefacts.
#
# Pattern to use in any script that does complete() + approx() before bands:
#
#   df <- df %>%
#     mutate(.reported = !is.na(value)) %>%    # ← add BEFORE complete()
#     group_by(...) %>%
#     complete(year = all_years) %>%
#     arrange(year) %>%
#     mutate(
#       .reported = replace_na(.reported, FALSE),
#       value     = { ... approx ... }
#     ) %>%
#     filter(!is.na(value)) %>%
#     ungroup()
#
#   # For band stats: NA-out interpolated values so na.rm=TRUE skips them
#   band <- df %>%
#     mutate(value_b = if_else(.reported, value, NA_real_)) %>%
#     group_by(...) %>%
#     summarise(
#       value_median = median(value_b, na.rm = TRUE),
#       value_min    = min(value_b,    na.rm = TRUE),
#       value_max    = max(value_b,    na.rm = TRUE),
#       n_models     = sum(.reported),
#       .groups = "drop"
#     )
