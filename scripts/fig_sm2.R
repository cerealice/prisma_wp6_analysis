library(tidyverse)
library(arrow)
library(patchwork)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 supplementary figure SM1
#
#   (a) T_median (p50) timeseries by scenario
#   (b) T_67th   (p67) timeseries by scenario  [same y axis as (a)]
#   (c) Cumulative CO₂ from 2020  (moved from fig1 panel c)
#   (d) Effective Radiative Forcing (moved from fig1 panel d)
#   (e) CO₂ emissions by PRISMA cluster
#
# Output: graphs/fig_sm1.png
# ══════════════════════════════════════════════════════════════

# ── Paths ──────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
direct_dir  <- file.path(project_dir, "data", "direct")
graphs_dir  <- file.path(project_dir, "graphs")
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(scripts_dir, "prisma_styles.R"))
source(file.path(scripts_dir, "model_fixes.R"))

if (!exists("DOWNLOAD_DATA", inherits = FALSE)) DOWNLOAD_DATA <- TRUE

# ── Scenarios ──────────────────────────────────────────────────
scenarios <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive",
  "SSP2 - Asymmetric Roll-Back"
)
ma_scen         <- "SSP2 - Meet Aspirations"
scen_plot_order <- scenarios

scenario_labels <- c(
  "SSP2 - Meet Aspirations"     = "Meet Aspirations",
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Staying Alive"        = "Tech Momentum",
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition"
)
scen_col_ssp2 <- c(
  "SSP2 - Meet Aspirations"     = "#5BB85C",
  "SSP2 - Late Reawakening"     = "#D62728",
  "SSP2 - Staying Alive"        = "#87CEEB",
  "SSP2 - Asymmetric Roll-Back" = "#999999"
)
scen_col_lbl    <- setNames(unname(scen_col_ssp2), unname(scenario_labels))
scen_lbl_levels <- unname(scenario_labels[scen_plot_order])

# ── Regions & cluster map ──────────────────────────────────────
r10_regions <- c(
  "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)"
)
cluster_map <- c(
  "Africa (R10)"               = "Diversifying Economies",
  "China+ (R10)"               = "Diversifying Economies",
  "Europe (R10)"               = "Transition Leaders",
  "India+ (R10)"               = "Diversifying Economies",
  "Latin America (R10)"        = "Diversifying Economies",
  "Middle East (R10)"          = "Fossil-Dependent",
  "North America (R10)"        = "Diversifying Economies",
  "Other (R10)"                = "Diversifying Economies",
  "Pacific OECD (R10)"         = "Transition Leaders",
  "Reforming Economies (R10)"  = "Fossil-Dependent",
  "Rest of Asia (R10)"         = "Diversifying Economies"
)
cluster_order <- c("Transition Leaders", "Diversifying Economies", "Fossil-Dependent")
all_regions   <- c("World", r10_regions)

# ── Theme ──────────────────────────────────────────────────────
theme_set(
  theme_bw(base_size = 20, base_family = "Arial") +
    theme(
      plot.title       = element_text(size = 20.0, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = 15.0, colour = "grey35", lineheight = 1.3),
      axis.title       = element_text(size = 18.5),
      axis.text        = element_text(size = 17.0),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text       = element_text(size = 18.5, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title     = element_text(size = 17.0),
      legend.text      = element_text(size = 16.5),
      plot.caption     = element_text(size = 14.5, colour = "grey50", hjust = 0)
    )
)

# ── MAGICC variables ───────────────────────────────────────────
MAGICC_TAG <- "[MAGICC v7.6.0a3]"
ca_vars <- c(
  paste0("Climate Assessment|Surface Temperature (GSAT)|33rd Percentile ", MAGICC_TAG),
  paste0("Climate Assessment|Surface Temperature (GSAT)|Median ",          MAGICC_TAG),
  paste0("Climate Assessment|Surface Temperature (GSAT)|67th Percentile ", MAGICC_TAG)
)

# ── Python / parquet helpers ───────────────────────────────────
miniconda_envs        <- file.path(Sys.getenv("USERPROFILE"), "miniconda3", "envs")
env_dirs              <- list.dirs(miniconda_envs, recursive = FALSE)
python_exe_candidates <- Filter(file.exists, file.path(env_dirs, "python.exe"))
if (length(python_exe_candidates) == 0) stop("No python.exe found under ", miniconda_envs)
python_exe <- python_exe_candidates[1]

run_python_download <- function(variable, prefix, regions_vec = "World") {
  pat      <- paste0("^", prefix, "_\\d.*\\.parquet$")
  existing <- list.files(direct_dir, pat, full.names = TRUE)
  existing <- existing[!grepl("_meta\\.parquet$", existing)]
  if (length(existing) > 0) {
    if (!DOWNLOAD_DATA) return(invisible(NULL))
    latest <- existing[which.max(file.info(existing)$mtime)]
    if (as.Date(file.info(latest)$mtime) == Sys.Date()) {
      message("Skipping '", prefix, "' \u2014 today's parquet already exists.")
      return(invisible(NULL))
    }
  }
  message("Downloading: ", prefix, " ...")
  py_code <- paste0(
    "import sys\n",
    "sys.path.insert(0, r'''", scripts_dir, "''')\n",
    "from download_prisma import download_prisma\n",
    "import json\n",
    "download_prisma(\n",
    "  variables=json.loads(r'''", toJSON(variable, auto_unbox = TRUE), "'''),\n",
    "  scenarios=json.loads(r'''", toJSON(scenarios, auto_unbox = FALSE), "'''),\n",
    "  regions=json.loads(r'''",  toJSON(regions_vec, auto_unbox = FALSE), "'''),\n",
    "  prefix='", prefix, "',\n",
    "  outdir=r'''", normalizePath(direct_dir, winslash = "/", mustWork = FALSE), "'''\n)\n"
  )
  tmp <- tempfile(fileext = ".py")
  writeLines(py_code, tmp)
  status <- system2(python_exe, tmp)
  if (status != 0) { message("Retrying..."); status <- system2(python_exe, tmp) }
  if (status != 0) warning("Download failed.")
  else             message("Download of '", prefix, "' completed.")
}

load_pq <- function(prefix) {
  pat   <- paste0("^", prefix, "_.*\\.parquet$")
  files <- list.files(direct_dir, pat, full.names = TRUE)
  files <- files[!grepl("_meta\\.parquet$", files)]
  # Fallback to committed backup data if no fresh download available
  if (length(files) == 0) {
    backup_dir <- file.path(project_dir, "data", "backup")
    files <- list.files(backup_dir, pat, full.names = TRUE)
    files <- files[!grepl("_meta\\.parquet$", files)]
    if (length(files) > 0)
      message("Using backup data for '", prefix, "' (no fresh download found).")
  }
  if (length(files) == 0) stop("No data found for '", prefix, "'. Run download manually.")
  read_parquet(files[which.max(file.info(files)$mtime)])
}

# ══════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════

# ── CO2 (shared parquet with fig1.R) ──────────────────────────
run_python_download("Emissions|CO2", "post_co2_total", all_regions)

all_years <- c(seq(2020, 2050, by = 5), seq(2060, 2100, by = 10))

co2_raw <- load_pq("post_co2_total") %>%
  filter(variable == "Emissions|CO2",
         scenario %in% scenarios,
         year     %in% all_years)
co2_raw <- fix_gem_e3_years(co2_raw)

m_all <- sort(unique(as.character(co2_raw$model)))
m_all <- m_all[m_all != "WITCH 6.0-EU28"]
m_all <- filter_latest_remind(m_all)

m_ok <- co2_raw %>%
  filter(model %in% m_all, region == "World", !is.na(value)) %>%
  distinct(model, scenario) %>%
  group_by(model) %>%
  summarise(has_meet = any(scenario == ma_scen),
            n_scen   = n_distinct(scenario), .groups = "drop") %>%
  filter(has_meet, n_scen >= min(3L, length(scenarios))) %>%
  pull(model)
message("CO2 models: ", paste(m_ok, collapse = ", "))

# ── GSAT ───────────────────────────────────────────────────────
run_python_download(ca_vars, "climate_assess_full")

ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
dir.create(ca_backup_dir, recursive = TRUE, showWarnings = FALSE)
for (f in list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)) {
  dst <- file.path(ca_backup_dir, basename(f))
  if (!file.exists(dst)) { file.copy(f, dst); message("Backed up: ", dst) }
}

ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run fig2_gsat.R first.")
raw_gsat <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)])

gsat <- raw_gsat %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         region == "World", scenario %in% scenarios,
         year >= 2020, year <= 2100) %>%
  mutate(pct_col = case_when(
    grepl("33rd",        variable, ignore.case = TRUE) ~ "p33",
    grepl("Median|50th", variable, ignore.case = TRUE) ~ "p50",
    grepl("67th",        variable, ignore.case = TRUE) ~ "p67",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value) %>%
  filter(!is.na(p50), !is.na(p67))

mods_gsat <- filter_latest_remind(sort(unique(as.character(gsat$model))))
mods_gsat <- mods_gsat[mods_gsat != "WITCH 6.0-EU28"]

gsat <- gsat %>%
  filter(model %in% mods_gsat) %>%
  mutate(scenario = factor(scenario, levels = scen_plot_order))

message("GSAT models (", length(mods_gsat), "): ", paste(mods_gsat, collapse = ", "))

# Short model names
model_short_names <- c(
  "GEM-E3 V2026"                      = "GEM-E3",
  "IMAGE 3.4"                          = "IMAGE",
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "MESSAGEix",
  "OPEN-PROM 2.2"                      = "OPEN-PROM",
  "REMIND-MAgPIE 3.7-4.14"            = "REMIND",
  "WITCH 6.0"                          = "WITCH",
  "IMACLIM 2.0"                        = "IMACLIM"
)
gsat <- gsat %>%
  mutate(model = ifelse(model %in% names(model_short_names),
                        model_short_names[model], model))
mods_gsat <- sort(unique(as.character(gsat$model)))

# ══════════════════════════════════════════════════════════════
# 2. GSAT TIMESERIES DATA  (panels a and b)
# ══════════════════════════════════════════════════════════════

# ── Panel (a): T_median (p50) ─────────────────────────────────
web_ts_p50 <- gsat %>%
  group_by(scenario, year) %>%
  summarise(
    ts_min = min(p50,    na.rm = TRUE),
    ts_med = median(p50, na.rm = TRUE),
    ts_max = max(p50,    na.rm = TRUE),
    .groups = "drop"
  )

central_p50 <- gsat %>%
  inner_join(web_ts_p50 %>% select(scenario, year, ts_med),
             by = c("scenario", "year")) %>%
  group_by(scenario, model) %>%
  summarise(rmse = sqrt(mean((p50 - ts_med)^2, na.rm = TRUE)), .groups = "drop") %>%
  group_by(scenario) %>%
  slice_min(rmse, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(scenario, model)

peak_p50 <- gsat %>%
  semi_join(central_p50, by = c("scenario", "model")) %>%
  group_by(scenario) %>%
  slice_max(p50, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label = paste0(sprintf("%.1f", p50), "\u00b0C (", year, ")"))

# ── Panel (b): T_67th (p67) ───────────────────────────────────
web_ts_p67 <- gsat %>%
  group_by(scenario, year) %>%
  summarise(
    ts_min = min(p67,    na.rm = TRUE),
    ts_med = median(p67, na.rm = TRUE),
    ts_max = max(p67,    na.rm = TRUE),
    .groups = "drop"
  )

central_p67 <- gsat %>%
  inner_join(web_ts_p67 %>% select(scenario, year, ts_med),
             by = c("scenario", "year")) %>%
  group_by(scenario, model) %>%
  summarise(rmse = sqrt(mean((p67 - ts_med)^2, na.rm = TRUE)), .groups = "drop") %>%
  group_by(scenario) %>%
  slice_min(rmse, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(scenario, model)

peak_p67 <- gsat %>%
  semi_join(central_p67, by = c("scenario", "model")) %>%
  group_by(scenario) %>%
  slice_max(p67, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label = paste0(sprintf("%.1f", p67), "\u00b0C (", year, ")"))

message("Most-central models (p50): ", paste(central_p50$model, collapse = ", "))
message("Most-central models (p67): ", paste(central_p67$model, collapse = ", "))

# Shared y-axis limits across panels a and b
ts_y_lo <- floor(min(web_ts_p50$ts_min, web_ts_p67$ts_min, na.rm = TRUE) * 10) / 10
ts_y_hi <- ceiling(max(
  max(peak_p50$p50, na.rm = TRUE),
  max(peak_p67$p67, na.rm = TRUE),
  web_ts_p50$ts_max,
  web_ts_p67$ts_max,
  na.rm = TRUE
) * 10) / 10 + 0.3   # extra headroom for peak labels

# ── ERF  (panel d) ────────────────────────────────────────────
extract_erf <- function(raw_df) {
  df <- raw_df %>%
    filter(grepl("Effective Radiative Forcing", variable),
           scenario %in% scenarios, region == "World",
           year >= 2020, year <= 2100, !is.na(value)) %>%
    group_by(model, scenario, year) %>%
    arrange(desc(grepl("Median", variable, ignore.case = TRUE))) %>%
    slice(1) %>%
    ungroup() %>%
    select(model, scenario, year, value)
  if (nrow(df) == 0) NULL else df
}
erf_raw <- tryCatch(extract_erf(load_pq("f15_ca_all")), error = function(e) NULL)
if (is.null(erf_raw))
  erf_raw <- tryCatch(extract_erf(load_pq("f15_erf")), error = function(e) NULL)
if (is.null(erf_raw)) {
  run_python_download("Emissions|Kyoto Gases", "f15_kghg")
  kghg_raw <- tryCatch(
    load_pq("f15_kghg") %>%
      filter(region == "World", scenario %in% scenarios,
             year %in% all_years, !is.na(value)) %>%
      select(model, scenario, year, value),
    error = function(e) NULL
  )
} else {
  kghg_raw <- NULL
}
message("ERF rows: ", if (is.null(erf_raw)) "none" else nrow(erf_raw))

# ══════════════════════════════════════════════════════════════
# 3. CUMULATIVE CO₂ AND ERF DATA  (panels c and d)
# ══════════════════════════════════════════════════════════════

trap_cumul <- function(yr, val) {
  n   <- length(yr)
  out <- numeric(n)
  for (i in seq(2, n))
    out[i] <- out[i - 1] + (val[i - 1] + val[i]) / 2 * (yr[i] - yr[i - 1])
  out
}

co2_world <- co2_raw %>%
  filter(model %in% m_ok, region == "World") %>%
  mutate(
    value    = value / 1000,
    scenario = unname(scenario_labels[as.character(scenario)])
  ) %>%
  select(model, scenario, year, value)

co2_cumul_band <- co2_world %>%
  group_by(model, scenario) %>%
  complete(year = all_years) %>%
  arrange(year) %>%
  mutate(value = {
    ok <- !is.na(value)
    if (sum(ok) >= 2) approx(year[ok], value[ok], xout = year, rule = 2)$y
    else value
  }) %>%
  filter(!is.na(value)) %>%
  mutate(cumulative = trap_cumul(year, value)) %>%
  ungroup() %>%
  group_by(scenario, year) %>%
  summarise(
    med = median(cumulative, na.rm = TRUE),
    lo  = min(cumulative,    na.rm = TRUE),
    hi  = max(cumulative,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(scenario = factor(scenario, levels = scen_lbl_levels))

budget_df <- tibble(
  y   = c(400L, 500L, 1150L, 1350L),
  lbl = c("1.5\u00b0C (67%)", "1.5\u00b0C (50%)", "2.0\u00b0C (67%)", "2.0\u00b0C (50%)")
)

use_erf <- !is.null(erf_raw) && nrow(erf_raw) > 0
if (use_erf) {
  d_raw    <- erf_raw %>%
    filter(model %in% m_ok) %>%
    mutate(scenario = unname(scenario_labels[as.character(scenario)]))
  d_ylabel <- "W/m\u00b2"
  d_title  <- "(d) Effective radiative forcing"
} else if (!is.null(kghg_raw) && nrow(kghg_raw) > 0) {
  co2_for_d <- co2_raw %>%
    filter(model %in% m_ok, region == "World") %>%
    mutate(value = value / 1000, scenario = as.character(scenario)) %>%
    select(model, scenario, year, co2_val = value)
  d_raw <- inner_join(
    kghg_raw %>%
      filter(model %in% m_ok) %>%
      mutate(value = value / 1000, scenario = as.character(scenario)) %>%
      select(model, scenario, year, kghg_val = value),
    co2_for_d, by = c("model", "scenario", "year")
  ) %>%
    mutate(value = kghg_val - co2_val,
           scenario = unname(scenario_labels[scenario])) %>%
    select(model, scenario, year, value)
  d_ylabel <- "Gt CO\u2082-eq/yr"
  d_title  <- "(d) Non-CO\u2082 GHG emissions"
} else {
  d_raw    <- NULL
  d_ylabel <- ""
  d_title  <- "(d) Effective radiative forcing"
}

if (!is.null(d_raw) && nrow(d_raw) > 0) {
  d_band <- d_raw %>%
    interp_band(c("model", "scenario")) %>%
    group_by(scenario, year) %>%
    summarise(
      med = median(value, na.rm = TRUE),
      lo  = min(value,    na.rm = TRUE),
      hi  = max(value,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = factor(scenario, levels = scen_lbl_levels))
} else {
  d_band <- NULL
}

# ══════════════════════════════════════════════════════════════
# 4. CO2 CLUSTER DATA  (panel e)
# ══════════════════════════════════════════════════════════════

interp_band <- function(df, group_vars) {
  if (nrow(df) == 0) return(df)
  df %>%
    mutate(.rep = !is.na(value)) %>%
    group_by(across(all_of(group_vars))) %>%
    complete(year = all_years) %>%
    arrange(year) %>%
    mutate(
      .rep  = replace_na(.rep, FALSE),
      value = { ok <- !is.na(value)
                if (sum(ok) >= 2) approx(year[ok], value[ok], xout = year, rule = 1)$y
                else value }
    ) %>%
    filter(!is.na(value)) %>%
    ungroup()
}

co2_cluster <- co2_raw %>%
  filter(model %in% m_ok, region %in% r10_regions) %>%
  mutate(
    value    = value / 1000,
    scenario = unname(scenario_labels[as.character(scenario)]),
    cluster  = cluster_map[region]
  ) %>%
  filter(!is.na(cluster)) %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

if (nrow(co2_cluster) == 0)
  stop("No R10 regional CO2 data found — check that post_co2_total includes R10 regions.")

co2_cluster_interp <- interp_band(co2_cluster, c("model", "scenario", "cluster"))

co2_band_cluster <- co2_cluster_interp %>%
  mutate(value_b = if_else(.rep, value, NA_real_)) %>%
  group_by(scenario, cluster, year) %>%
  summarise(
    med      = median(value_b, na.rm = TRUE),
    lo       = min(value_b,    na.rm = TRUE),
    hi       = max(value_b,    na.rm = TRUE),
    n_models = sum(.rep),
    .groups  = "drop"
  ) %>%
  mutate(
    scenario = factor(scenario, levels = unname(scenario_labels)),
    cluster  = factor(cluster,  levels = cluster_order)
  )

# ══════════════════════════════════════════════════════════════
# 4. BUILD PANELS
# ══════════════════════════════════════════════════════════════

x_breaks_ts <- seq(2020, 2100, 20)
x_breaks_cl <- c(2020, seq(2040, 2100, by = 20))


# ── Panel (a): T_median (p50) ─────────────────────────────────
p_a <- ggplot(web_ts_p50,
              aes(x = year, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = ts_min, ymax = ts_max), alpha = 0.18, colour = NA) +
  geom_line(aes(y = ts_min), linewidth = 0.45, alpha = 0.6) +
  geom_line(aes(y = ts_max), linewidth = 0.45, alpha = 0.6) +
  geom_line(
    data      = semi_join(gsat, central_p50, by = c("scenario", "model")),
    aes(x = year, y = p50, group = interaction(model, scenario)),
    linewidth = 1.1
  ) +
  geom_point(
    data = peak_p50, aes(x = year, y = p50),
    size = 3.1, shape = 21, fill = "white", stroke = 1.2,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_text(
    data = peak_p50,
    aes(x = year, y = p50, label = label, colour = scenario),
    vjust = -0.85, hjust = 0.5, size = 5.4, fontface = "bold",
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_colour_manual(values = scen_col_ssp2, guide = "none") +
  scale_fill_manual(  values = scen_col_ssp2, guide = "none") +
  scale_x_continuous(breaks = x_breaks_ts,
                     expand = expansion(mult = c(0.02, 0.06))) +
  coord_cartesian(ylim = c(ts_y_lo, ts_y_hi)) +
  labs(title = "(a) T\u2085\u2080 (median) timeseries",
       x = NULL, y = "\u00b0C above 1850\u20131900") +
  theme(legend.position = "none", aspect.ratio = 1)

# ── Panel (b): T_67th (p67) ───────────────────────────────────
p_b <- ggplot(web_ts_p67,
              aes(x = year, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = ts_min, ymax = ts_max), alpha = 0.18, colour = NA) +
  geom_line(aes(y = ts_min), linewidth = 0.45, alpha = 0.6) +
  geom_line(aes(y = ts_max), linewidth = 0.45, alpha = 0.6) +
  geom_line(
    data      = semi_join(gsat, central_p67, by = c("scenario", "model")),
    aes(x = year, y = p67, group = interaction(model, scenario)),
    linewidth = 1.1
  ) +
  geom_point(
    data = peak_p67, aes(x = year, y = p67),
    size = 3.1, shape = 21, fill = "white", stroke = 1.2,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_text(
    data = peak_p67,
    aes(x = year, y = p67, label = label, colour = scenario),
    vjust = -0.85, hjust = 0.5, size = 5.4, fontface = "bold",
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_colour_manual(values = scen_col_ssp2, guide = "none") +
  scale_fill_manual(  values = scen_col_ssp2, guide = "none") +
  scale_x_continuous(breaks = x_breaks_ts,
                     expand = expansion(mult = c(0.02, 0.06))) +
  coord_cartesian(ylim = c(ts_y_lo, ts_y_hi)) +
  labs(title = "(b) T\u2086\u2087 (67th pct) timeseries",
       x = NULL, y = "\u00b0C above 1850\u20131900") +
  theme(legend.position = "none", aspect.ratio = 1)

# ── Panel (c): Cumulative CO₂ from 2020 ───────────────────────
x_breaks_co2 <- c(2020, seq(2040, 2100, by = 20))
p_c <- ggplot(co2_cumul_band,
              aes(x = year, colour = scenario, fill = scenario, group = scenario)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, linewidth = 0.25) +
  geom_line(aes(y = med), linewidth = 0.9) +
  geom_hline(data = budget_df, aes(yintercept = y),
             linewidth = 0.45, linetype = "dashed", colour = "grey40",
             inherit.aes = FALSE) +
  geom_text(data = budget_df, aes(x = 2020, y = y, label = lbl),
            hjust = 0, vjust = -0.45, size = 4.7,
            colour = "grey35", inherit.aes = FALSE) +
  scale_colour_manual(values = scen_col_lbl, name = NULL) +
  scale_fill_manual(  values = scen_col_lbl, name = NULL) +
  scale_x_continuous(breaks = x_breaks_co2) +
  labs(title = "(c) Cumulative CO\u2082 from 2020",
       x = NULL, y = "Gt CO\u2082") +
  guides(colour = guide_legend(ncol = 2, override.aes = list(fill = NA)), fill = "none") +
  theme(legend.position = "none",
        axis.text.x     = element_text(angle = 45, hjust = 1),
        aspect.ratio    = 1)

# ── Panel (d): ERF or non-CO₂ GHG ────────────────────────────
if (!is.null(d_band)) {
  p_d <- ggplot(d_band,
                aes(x = year, colour = scenario, fill = scenario, group = scenario)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, linewidth = 0.25) +
    geom_line(aes(y = med), linewidth = 0.9) +
    scale_colour_manual(values = scen_col_lbl, name = NULL) +
    scale_fill_manual(  values = scen_col_lbl, name = NULL) +
    scale_x_continuous(breaks = x_breaks_co2) +
    labs(title = d_title, x = NULL, y = d_ylabel) +
    guides(colour = guide_legend(ncol = 2, override.aes = list(fill = NA)), fill = "none") +
    theme(legend.position = "none",
          axis.text.x     = element_text(angle = 45, hjust = 1),
          aspect.ratio    = 1)
} else {
  p_d <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "No data available for panel (d)\n(Effective Radiative Forcing)",
             size = 6.0, colour = "grey55", hjust = 0.5, vjust = 0.5) +
    scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
    labs(title = d_title, x = NULL, y = NULL) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank(), aspect.ratio = 1)
}

# ── Panel (e): CO2 by cluster ─────────────────────────────────
p_e <- ggplot(co2_band_cluster,
              aes(x = year, colour = scenario, fill = scenario, group = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(aes(y = med), linewidth = 0.9) +
  facet_wrap(~ cluster, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = scen_col_lbl, name = NULL) +
  scale_fill_manual(  values = scen_col_lbl, name = NULL) +
  scale_x_continuous(breaks = x_breaks_cl) +
  labs(title = "(e) CO\u2082 emissions by PRISMA cluster",
       x = NULL, y = "Gt CO\u2082/yr") +
  guides(
    colour = guide_legend(ncol = 4, override.aes = list(fill = NA)),
    fill   = "none"
  ) +
  theme(legend.position = "bottom",
        aspect.ratio    = 1)

# ══════════════════════════════════════════════════════════════
# 5. COMBINE AND SAVE
# ══════════════════════════════════════════════════════════════
fig <- (p_a | p_b) / (p_c | p_d) / p_e +
  plot_layout(heights = c(1, 1, 1))

out_file <- file.path(graphs_dir, "fig_sm2.png")
ggsave(out_file, fig, width = 16, height = 20, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig_sm2", graphs_dir)
  }
})
