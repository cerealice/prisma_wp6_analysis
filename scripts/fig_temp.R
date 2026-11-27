library(tidyverse)
library(arrow)
library(patchwork)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — TEMP: near-term plausibility check with piamValidation
#
#   Downloads ALL absolute-check variables from scenarioMIP_IAMC:
#     Capacity|Electricity|Solar, |Wind|Onshore, |Nuclear, |Hydro,
#     Carbon Capture|Geological Storage, Primary Energy|Biomass,
#     Sales/Stocks|Transportation|LDV|Battery-Electric
#
#   Checks at World level only (scenarioMIP_IAMC is World-only).
#   Tile labels show the model value AND the [min_red–max_red] thresholds
#   directly from the piamValidation output.
#
#   Model filtering:
#     • Excludes WITCH 6.0-EU28
#     • Drops REMIND-MAgPIE 3.6-4.14 when 3.7-4.14 is present
#     • GEM-E3 V2026 excluded (did not submit Capacity data to IIASA)
#
# Output: graphs/fig_temp_validation.png
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("piamValidation", quietly = TRUE)) {
  options(repos = c(pik  = "https://rse.pik-potsdam.de/r/packages",
                    CRAN = "https://cran.r-project.org"))
  install.packages("piamValidation")
}
library(piamValidation)

# ── Paths ──────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
direct_dir  <- file.path(project_dir, "data", "direct")
graphs_dir  <- file.path(project_dir, "graphs")
dir.create(graphs_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(direct_dir,  recursive = TRUE, showWarnings = FALSE)

source(file.path(scripts_dir, "prisma_styles.R"))
source(file.path(scripts_dir, "model_fixes.R"))

if (!exists("DOWNLOAD_DATA", inherits = FALSE)) DOWNLOAD_DATA <- TRUE
python_exe <- find_python_pyam()

# ── Scenarios ─────────────────────────────────────────────────
prisma_scenarios <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive"
)
scen_labels <- c(
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition",
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Staying Alive"        = "Tech Momentum",
  "SSP2 - Meet Aspirations"     = "Meet Aspirations"
)
scen_order <- c("Shifted Ambition", "Late Reawakening", "Tech Momentum", "Meet Aspirations")

model_short <- c(
  "GEM-E3 V2026"                       = "GEM-E3",
  "IMACLIM 2.0"                        = "IMACLIM-R",
  "IMAGE 3.4"                          = "IMAGE",
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "MESSAGE",
  "OPEN-PROM 2.2"                      = "OPEN-PROM",
  "REMIND-MAgPIE 3.6-4.14"            = "REMIND 3.6",
  "REMIND-MAgPIE 3.7-4.14"            = "REMIND 3.7",
  "WITCH 6.0"                          = "WITCH"
)

# ── Load piamValidation config (all metrics) ───────────────────
cat("Loading piamValidation config (scenarioMIP_IAMC)...\n")
full_cfg <- piamValidation::getConfig("scenarioMIP_IAMC")
abs_rows <- sum(full_cfg$metric == "absolute")
cat("Config rows:", nrow(full_cfg), " | absolute:", abs_rows,
    "| relative/difference (skipped \u2014 no reference data):",
    nrow(full_cfg) - abs_rows, "\n")

# All variables across all check types; IAMC config is World-only
all_vars <- unique(full_cfg$variable)
cat("Variables to download (", length(all_vars), "):",
    paste(sort(all_vars), collapse = ", "), "\n")

# ── Download ───────────────────────────────────────────────────
local({
  prefix   <- "piam_iamc"
  pat      <- paste0("^", prefix, "_.*\\.parquet$")
  existing <- list.files(direct_dir, pat, full.names = TRUE)
  existing <- existing[!grepl("_meta\\.parquet$", existing)]

  if (length(existing) > 0) {
    if (!DOWNLOAD_DATA) return(invisible(NULL))
    latest <- existing[which.max(file.info(existing)$mtime)]
    if (as.Date(file.info(latest)$mtime) == Sys.Date()) {
      message("Skipping 'piam_iamc' \u2014 today's parquet already exists.")
      return(invisible(NULL))
    }
  }

  message("Downloading: piam_iamc ...")
  py_code <- paste0(
    "import sys\n",
    "sys.path.insert(0, r'''", scripts_dir, "''')\n",
    "from download_prisma import download_prisma\n",
    "import json\n",
    "download_prisma(\n",
    "  variables=json.loads(r'''", toJSON(all_vars,         auto_unbox = FALSE), "'''),\n",
    "  scenarios=json.loads(r'''", toJSON(prisma_scenarios, auto_unbox = FALSE), "'''),\n",
    "  regions=json.loads(r'''[\"World\"]'''),\n",
    "  prefix='piam_iamc',\n",
    "  outdir=r'''", normalizePath(direct_dir, winslash = "/", mustWork = FALSE), "'''\n)\n"
  )
  tmp <- tempfile(fileext = ".py")
  writeLines(py_code, tmp)
  status <- system2(python_exe, tmp)
  if (status != 0) { message("Retrying..."); status <- system2(python_exe, tmp) }
  if (status != 0) warning("Download failed for 'piam_iamc'.")
  else             message("Download of 'piam_iamc' completed.")
})

# ── Load parquet ───────────────────────────────────────────────
piam_files <- list.files(direct_dir, pattern = "^piam_iamc_.*\\.parquet$", full.names = TRUE)
piam_files <- piam_files[!grepl("_meta\\.parquet$", piam_files)]
if (length(piam_files) == 0) stop("No piam_iamc parquet found \u2014 download may have failed.")

piam_pq <- piam_files[which.max(file.info(piam_files)$mtime)]
cat("Loading:", basename(piam_pq), "\n")
raw_data <- read_parquet(piam_pq)

# ── Filter & reshape ───────────────────────────────────────────
model_data <- raw_data %>%
  rename(period = year) %>%
  mutate(period = as.integer(period)) %>%
  filter(
    scenario %in% prisma_scenarios,
    region   == "World",
    variable %in% all_vars,
    model    != "WITCH 6.0-EU28"
  )

keep_mods  <- filter_latest_remind(unique(model_data$model))
model_data <- filter(model_data, model %in% keep_mods)

model_data <- model_data %>%
  mutate(
    model    = ifelse(model %in% names(model_short), model_short[model], model),
    scenario = ifelse(scenario %in% names(scen_labels), scen_labels[scenario], scenario),
    scenario = factor(scenario, levels = scen_order)
  )

cat("Models:   ", paste(sort(unique(model_data$model)), collapse = ", "), "\n")
cat("Variables:", paste(sort(unique(model_data$variable)), collapse = ", "), "\n")

# ── piamValidation ─────────────────────────────────────────────
cat("\nRunning validateScenarios...\n")
# Relative checks require embedded historical reference data (IEA, CEDS, etc.)
# which the package does not fully supply; restrict to absolute checks.
abs_cfg <- dplyr::filter(full_cfg, metric == "absolute")
vali_results <- piamValidation::validateScenarios(
  dataPath    = model_data,
  config      = abs_cfg,
  extraColors = FALSE
)
cat("Results:", nrow(vali_results), "rows\n")
cat("Check summary:\n"); print(table(vali_results$check))

# ── Prepare plot data ──────────────────────────────────────────
# 6-level directional color scheme:
#   blue_lo  = below min_red  (critical underperformance)
#   yel_lo   = below min_yel  (warning, low side)
#   green    = within range
#   yel_hi   = above max_yel  (warning, high side)
#   red_hi   = above max_red  (critical overperformance)
#   grey     = no data
piam_colors <- c(
  blue_lo = "#2166AC",
  yel_lo  = "#92C5DE",
  green   = "#008450",
  yel_hi  = "#EFBB0F",
  red_hi  = "#AA0014",
  grey    = "#C0C0C0"
)

# Adaptive formatter: handles both large absolute values and small relative deviations
fmt_num <- function(x) {
  dplyr::case_when(
    is.na(x)       ~ "",
    abs(x) >= 100  ~ sprintf("%.0f", x),
    abs(x) >= 1    ~ sprintf("%.1f", x),
    TRUE           ~ sprintf("%.3f", x)
  )
}

thr_label <- function(lo, hi) {
  dplyr::case_when(
    is.na(lo) & is.na(hi) ~ "",
    is.na(lo)             ~ paste0("<\u202f", fmt_num(hi)),
    is.na(hi)             ~ paste0(">\u202f", fmt_num(lo)),
    TRUE                  ~ paste0("[", fmt_num(lo), "\u2013", fmt_num(hi), "]")
  )
}

plot_df <- vali_results %>%
  mutate(
    period    = as.character(period),
    model     = factor(model),
    scenario  = factor(as.character(scenario), levels = scen_order),
    check_dir = dplyr::case_when(
      check == "grey"   ~ "grey",
      check == "green"  ~ "green",
      check == "yellow" & !is.na(min_yel) & value < min_yel ~ "yel_lo",
      check == "yellow" ~ "yel_hi",
      check == "red"    & !is.na(min_red)  & value < min_red ~ "blue_lo",
      check == "red"    ~ "red_hi",
      TRUE              ~ "grey"
    ),
    check_dir = factor(check_dir, levels = names(piam_colors)),
    thr_lbl   = thr_label(min_red, max_red),
    val_lbl   = paste0(fmt_num(value), "\n", thr_lbl)
  ) %>%
  group_by(variable, model, scenario, region, period) %>%
  slice(1) %>%
  ungroup()

world_df <- filter(plot_df, region == "World")

# ── Variable metadata ──────────────────────────────────────────
var_meta <- tibble::tribble(
  ~variable,                                                       ~label,                        ~unit,
  "Final Energy",                                                  "Final Energy",                "EJ/yr",
  "Primary Energy|Coal",                                           "Primary Energy Coal",         "EJ/yr",
  "Primary Energy|Oil",                                            "Primary Energy Oil",          "EJ/yr",
  "Primary Energy|Gas",                                            "Primary Energy Gas",          "EJ/yr",
  "Primary Energy|Biomass",                                        "Primary Energy Biomass",      "EJ/yr",
  "Capacity|Electricity|Solar",                                    "Solar Capacity",              "GW",
  "Capacity|Electricity|Wind|Onshore",                             "Wind Onshore Capacity",       "GW",
  "Capacity|Electricity|Nuclear",                                  "Nuclear Capacity",            "GW",
  "Capacity|Electricity|Hydro",                                    "Hydro Capacity",              "GW",
  "Carbon Capture|Geological Storage",                             "CCS",                        "Mt\u202fCO\u2082/yr",
  "Emissions|CO2|Energy and Industrial Processes",                 "CO\u2082 Emissions (EIP)",   "Mt\u202fCO\u2082/yr",
  "Emissions|CH4|Waste",                                           "CH\u2084 Emissions Waste",   "Mt\u202fCH\u2084/yr",
  "Sales|Transportation|Light-Duty Vehicle|Battery-Electric",      "LDV BEV Sales",              "million",
  "Stocks|Transportation|Light-Duty Vehicle|Battery-Electric",     "LDV BEV Stocks",             "million"
)
# Keep only variables actually present in the config
var_meta <- var_meta %>% filter(variable %in% all_vars)

# ── Shared theme ───────────────────────────────────────────────
theme_piam <- theme_bw(base_size = 10, base_family = "Arial") +
  theme(
    plot.title       = element_text(size = 10, face = "bold", hjust = 0),
    strip.background = element_blank(),
    strip.text       = element_text(size = 9, face = "bold"),
    axis.text.x      = element_text(size = 8),
    axis.text.y      = element_text(size = 8),
    panel.grid       = element_blank(),
    legend.position  = "none"
  )

fill_scale <- scale_fill_manual(
  values = piam_colors,
  labels = c(blue_lo = "Below min (critical)",
             yel_lo  = "Below range (warning)",
             green   = "Within range",
             yel_hi  = "Above range (warning)",
             red_hi  = "Above max (critical)",
             grey    = "No data"),
  drop = FALSE, name = NULL
)

# ── Panel builders ─────────────────────────────────────────────

# World panel: y = period, x = model, facet = scenario
make_world_panel <- function(var, label, unit) {
  df_v <- filter(world_df, as.character(variable) == var)
  if (nrow(df_v) == 0) return(NULL)

  ggplot(df_v, aes(x = model, y = factor(period), fill = check_dir)) +
    geom_tile(colour = "#E8E8E8", linewidth = 0.3) +
    geom_text(aes(label = val_lbl), size = 2.0, colour = "white",
              fontface = "bold", lineheight = 0.78) +
    fill_scale +
    facet_wrap(~ scenario, nrow = 1) +
    labs(title = paste0(label, " [", unit, "] \u2014 World"), x = NULL, y = NULL) +
    theme_piam +
    theme(axis.text.x = element_text(size = 8, angle = 25, hjust = 1))
}

# ── Build world panels ─────────────────────────────────────────
world_plots <- purrr::map(seq_len(nrow(var_meta)), function(i)
  make_world_panel(var_meta$variable[i], var_meta$label[i], var_meta$unit[i]))

if (all(sapply(world_plots, is.null))) stop("No plots generated.")

# ── Save per-variable figures in graphs/piam_validation/ ───────
piam_dir <- file.path(graphs_dir, "piam_validation")
dir.create(piam_dir, recursive = TRUE, showWarnings = FALSE)

legend_theme <- theme(
  legend.position = "bottom",
  legend.text     = element_text(size = 9),
  legend.key.size = unit(0.4, "cm")
)

caption_txt <- "piamValidation, scenarioMIP_IAMC | IIASA prisma-internal database"

var_filenames <- c(
  "Final Energy"                                              = "piam_01_final_energy",
  "Primary Energy|Coal"                                       = "piam_02_pe_coal",
  "Primary Energy|Oil"                                        = "piam_03_pe_oil",
  "Primary Energy|Gas"                                        = "piam_04_pe_gas",
  "Primary Energy|Biomass"                                    = "piam_05_pe_biomass",
  "Capacity|Electricity|Solar"                                = "piam_06_solar",
  "Capacity|Electricity|Wind|Onshore"                        = "piam_07_wind_onshore",
  "Capacity|Electricity|Nuclear"                              = "piam_08_nuclear",
  "Capacity|Electricity|Hydro"                                = "piam_09_hydro",
  "Carbon Capture|Geological Storage"                         = "piam_10_ccs",
  "Emissions|CO2|Energy and Industrial Processes"             = "piam_11_co2_eip",
  "Emissions|CH4|Waste"                                       = "piam_12_ch4_waste",
  "Sales|Transportation|Light-Duty Vehicle|Battery-Electric"  = "piam_13_ldv_sales",
  "Stocks|Transportation|Light-Duty Vehicle|Battery-Electric" = "piam_14_ldv_stocks"
)

for (i in seq_len(nrow(var_meta))) {
  var   <- var_meta$variable[i]
  label <- var_meta$label[i]
  unit  <- var_meta$unit[i]
  fname <- var_filenames[var]
  if (is.na(fname)) fname <- paste0("piam_", gsub("[^A-Za-z0-9]", "_", label))

  wp <- world_plots[[i]]
  if (is.null(wp)) { cat("No data for:", label, "— skipping\n"); next }

  fig <- wp +
    labs(caption = caption_txt) +
    theme(
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      legend.key.size = unit(0.4, "cm")
    )

  out <- file.path(piam_dir, paste0(fname, ".png"))
  ggsave(out, fig,
         width = 20, height = 5,
         dpi = 150, limitsize = FALSE, bg = "white")
  cat("Saved:", basename(out), "\n")
}

# ── Summary: critical failures only (red_hi + blue_lo) ────────
critical_df <- world_df %>%
  filter(check_dir == "red_hi") %>%
  left_join(var_meta %>% select(variable, label), by = "variable") %>%
  mutate(
    var_lbl = paste0(label, "\n(", period, ")")
  )

if (nrow(critical_df) > 0) {
  summary_fig <- ggplot(critical_df,
      aes(x = model, y = var_lbl, fill = check_dir)) +
    geom_tile(colour = "#E8E8E8", linewidth = 0.3) +
    geom_text(aes(label = val_lbl), size = 2.0, colour = "white",
              fontface = "bold", lineheight = 0.78) +
    scale_fill_manual(
      values = piam_colors["red_hi"],
      labels = c(red_hi = "Above max (critical)"),
      drop = FALSE, name = NULL
    ) +
    facet_wrap(~ scenario, nrow = 1) +
    labs(
      title   = "PRISMA WP6 \u2014 Critical Validation Failures (World)",
      caption = caption_txt,
      x = NULL, y = NULL
    ) +
    theme_piam +
    theme(
      axis.text.x     = element_text(size = 8, angle = 25, hjust = 1),
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      legend.key.size = unit(0.4, "cm")
    )

  n_vars   <- length(unique(critical_df$var_lbl))
  n_models <- length(unique(critical_df$model))
  out <- file.path(piam_dir, "piam_00_summary_critical.png")
  ggsave(out, summary_fig,
         width  = max(10, n_models * 1.6 + 4),
         height = max(6,  n_vars   * 0.7 + 4),
         dpi = 150, limitsize = FALSE, bg = "white")
  cat("Saved:", basename(out), "\n")
} else {
  cat("No critical failures found at World level.\n")
}

# ── Summary: yellow + red, power system & emissions only ───────
power_em_vars <- c(
  "Capacity|Electricity|Solar",
  "Capacity|Electricity|Wind|Onshore",
  "Capacity|Electricity|Nuclear",
  "Capacity|Electricity|Hydro",
  "Carbon Capture|Geological Storage",
  "Primary Energy|Biomass"
)

power_em_df <- world_df %>%
  filter(
    check_dir %in% c("yel_hi", "red_hi"),
    as.character(variable) %in% power_em_vars
  ) %>%
  left_join(var_meta %>% select(variable, label), by = "variable") %>%
  mutate(var_lbl = paste0(label, "\n(", period, ")"))

if (nrow(power_em_df) > 0) {
  power_em_keep   <- c("yel_hi", "red_hi")
  power_em_df     <- power_em_df %>% filter(check_dir %in% power_em_keep)
  power_em_colors <- piam_colors[names(piam_colors) %in%
                                   unique(as.character(power_em_df$check_dir))]
  power_em_labels <- c(
    yel_hi  = "Above range (warning)",
    red_hi  = "Above max (critical)"
  )

  power_em_fig <- ggplot(power_em_df,
      aes(x = model, y = var_lbl, fill = check_dir)) +
    geom_tile(colour = "#E8E8E8", linewidth = 0.3) +
    geom_text(aes(label = val_lbl), size = 2.0, colour = "white",
              fontface = "bold", lineheight = 0.78) +
    scale_fill_manual(
      values = power_em_colors,
      labels = power_em_labels[names(power_em_colors)],
      drop = TRUE, name = NULL
    ) +
    facet_wrap(~ scenario, nrow = 1) +
    labs(
      title   = "PRISMA WP6 \u2014 Power System & Emissions Plausibility (World)",
      caption = caption_txt,
      x = NULL, y = NULL
    ) +
    theme_piam +
    theme(
      axis.text.x     = element_text(size = 8, angle = 25, hjust = 1),
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      legend.key.size = unit(0.4, "cm")
    )

  n_vars   <- length(unique(power_em_df$var_lbl))
  n_models <- length(unique(power_em_df$model))
  out <- file.path(piam_dir, "piam_00_summary_power_emissions.png")
  ggsave(out, power_em_fig,
         width  = max(10, n_models * 1.6 + 4),
         height = max(6,  n_vars   * 0.7 + 4),
         dpi = 150, limitsize = FALSE, bg = "white")
  cat("Saved:", basename(out), "\n")
} else {
  cat("No yellow/red results for power system & emissions variables.\n")
}

# ── Overview: all World panels stacked ─────────────────────────
world_only <- Filter(Negate(is.null), world_plots)
if (length(world_only) > 0) {
  overview <- wrap_plots(world_only, ncol = 1, guides = "collect") +
    plot_annotation(
      title   = "PRISMA WP6 \u2014 Multi-Model Near-Term Plausibility Check (World)",
      caption = caption_txt,
      theme   = legend_theme
    )
  out <- file.path(piam_dir, "piam_00_overview_world.png")
  ggsave(out, overview,
         width = 20, height = 4.5 * length(world_only) + 3,
         dpi = 150, limitsize = FALSE, bg = "white")
  cat("Saved:", basename(out), "\n")
}
