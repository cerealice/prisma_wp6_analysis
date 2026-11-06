library(tidyverse)
library(arrow)
library(patchwork)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-13 fig 2 / fig SM2 / fig SM3
#   Produces three output files, one per non-Meet scenario:
#     fig3.png    — Shifted Ambition
#     fig_sm6.png — Late Reawakening
#     fig_sm8.png — Tech Momentum
#
# Each file has three panels:
#   (a) GDP timeseries vs Meet Aspirations (world, by model)
#   (b) GDP NPV — avoided costs + damage-adjusted NPV (world)
#   (c) GDP NPV by PRISMA cluster (3 columns, one per cluster)
# ══════════════════════════════════════════════════════════════

# ── Constants ──────────────────────────────────────────────────
CAT_WEIGHT    <- 0.621
GROWTH_WEIGHT <- 0.602
ALPHA_C       <- 0.622 + 1.775 * CAT_WEIGHT
BETA_C        <- 1.997 * GROWTH_WEIGHT
SE_ALPHA_C    <- sqrt(0.265^2 + (CAT_WEIGHT * 1.858)^2)
SE_BETA_C     <- GROWTH_WEIGHT * 1.130
N_MC          <- 5000L
DEFAULT_DISC  <- 0.02
BASE_YEAR     <- 2020L
ENDOGENOUS_THRESHOLD <- 0.005

damage_fn <- function(T, alpha = ALPHA_C, beta = BETA_C)
  alpha * pmax(T, 0)^1.5 + beta * pmax(T, 0)

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
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive"
)
ma_scen       <- "SSP2 - Meet Aspirations"
scens_nonmeet <- c(
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive"
)
scen_labels <- c(
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition",
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Staying Alive"        = "Tech Momentum"
)

gdp_years_std <- c(seq(2020, 2050, by = 5), seq(2060, 2100, by = 10))

# ── Cluster map ────────────────────────────────────────────────
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
  "Middle East (R10)"          = "Fossil-Dependent Economies",
  "North America (R10)"        = "Diversifying Economies",
  "Other (R10)"                = "Diversifying Economies",
  "Pacific OECD (R10)"         = "Transition Leaders",
  "Reforming Economies (R10)"  = "Fossil-Dependent Economies",
  "Rest of Asia (R10)"         = "Diversifying Economies"
)
cluster_order <- c("Transition Leaders", "Diversifying Economies", "Fossil-Dependent Economies")

# ── Model short names ──────────────────────────────────────────
model_short_names <- c(
  "GEM-E3 V2026"                       = "GEM-E3",
  "IMACLIM 2.0"                        = "IMACLIM",
  "IMAGE 3.4"                          = "IMAGE",
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "MESSAGE",
  "OPEN-PROM 2.2"                      = "OPEN-PROM",
  "REMIND-MAgPIE 3.6-4.14"            = "REMIND 3.6",
  "REMIND-MAgPIE 3.7-4.14"            = "REMIND 3.7",
  "WITCH 6.0"                          = "WITCH"
)
short_name <- function(x) ifelse(x %in% names(model_short_names), model_short_names[x], x)

# ── Theme ──────────────────────────────────────────────────────
theme_set(
  theme_bw(base_size = 16, base_family = "Arial") +
    theme(
      plot.title         = element_text(size = 18.0, face = "bold"),
      plot.subtitle      = element_text(size = 12.4, colour = "grey35", lineheight = 1.3),
      plot.caption       = element_text(size = 13.0, colour = "black", hjust = 0),
      axis.title         = element_text(size = 14.3),
      axis.text          = element_text(size = 13.0),
      strip.background   = element_blank(),
      strip.text         = element_text(size = 15.5, face = "bold"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.title       = element_text(size = 14.3),
      legend.text        = element_text(size = 13.6)
    )
)

# ── Download / load helpers ────────────────────────────────────
miniconda_envs        <- file.path(Sys.getenv("USERPROFILE"), "miniconda3", "envs")
env_dirs              <- list.dirs(miniconda_envs, recursive = FALSE)
python_exe_candidates <- Filter(file.exists, file.path(env_dirs, "python.exe"))
if (length(python_exe_candidates) == 0) stop("No python.exe found under ", miniconda_envs)
python_exe <- python_exe_candidates[1]

run_python_download <- function(variable, prefix, regions_vec = "World") {
  pat      <- paste0("^", prefix, "_.*\\.parquet$")
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

fill_sparse_w <- function(df) {
  df %>%
    group_by(model, scenario) %>%
    complete(year = gdp_years_std) %>%
    arrange(year) %>%
    mutate(value = {
      ok <- !is.na(value)
      if (sum(ok) >= 2) approx(year[ok], value[ok], xout = year, rule = 2)$y
      else value
    }) %>%
    filter(!is.na(value)) %>%
    ungroup()
}

fill_sparse_cl <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>%
    group_by(model, scenario, cluster) %>%
    complete(year = gdp_years_std) %>%
    arrange(year) %>%
    mutate(value = {
      ok <- !is.na(value)
      if (sum(ok) >= 2) approx(year[ok], value[ok], xout = year, rule = 2)$y
      else value
    }) %>%
    filter(!is.na(value)) %>%
    ungroup()
}

npv_abs_ann <- function(gdp, years, r = DEFAULT_DISC) {
  ann_y <- seq(BASE_YEAR, 2100L)
  sum(approx(years, gdp, xout = ann_y, rule = 2)$y /
        (1 + r)^(ann_y - BASE_YEAR), na.rm = TRUE)
}

# ══════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════

# ── World GDP (MER > PPP) ─────────────────────────────────────
run_python_download("GDP|MER", "gdp_mer")
run_python_download("GDP|PPP", "gdp_ppp")

load_gdp_var <- function(prefix, varname, regions = "World") {
  tryCatch(
    load_pq(prefix) %>%
      filter(variable == varname, region %in% regions, scenario %in% scenarios),
    error = function(e) NULL
  )
}
gdp_mer_raw <- load_gdp_var("gdp_mer", "GDP|MER")
gdp_ppp_raw <- load_gdp_var("gdp_ppp", "GDP|PPP")
models_mer  <- if (!is.null(gdp_mer_raw)) unique(as.character(gdp_mer_raw$model)) else character(0)

gdp_all <- bind_rows(
  gdp_mer_raw,
  if (!is.null(gdp_ppp_raw)) filter(gdp_ppp_raw, !model %in% models_mer) else NULL
) %>% filter(!is.na(value))
gdp_all <- fix_gem_e3_years(gdp_all)

# ── R10 GDP (MER > PPP) ───────────────────────────────────────
run_python_download("GDP|MER", "gdp_mer_r10", r10_regions)
run_python_download("GDP|PPP", "gdp_ppp_r10", r10_regions)

gdp_mer_r10_raw <- load_gdp_var("gdp_mer_r10", "GDP|MER", r10_regions)
gdp_ppp_r10_raw <- load_gdp_var("gdp_ppp_r10", "GDP|PPP", r10_regions)
models_mer_r10  <- if (!is.null(gdp_mer_r10_raw)) unique(as.character(gdp_mer_r10_raw$model)) else character(0)

gdp_r10_all <- bind_rows(
  gdp_mer_r10_raw,
  if (!is.null(gdp_ppp_r10_raw)) filter(gdp_ppp_r10_raw, !model %in% models_mer_r10) else NULL
) %>% filter(!is.na(value))
gdp_r10_all <- fix_gem_e3_years(gdp_r10_all)

# Fallback: if world-level GDP is missing, aggregate from R10 regions
if (nrow(gdp_all) == 0 && nrow(gdp_r10_all) > 0) {
  message("World GDP parquet empty \u2014 aggregating world total from R10 regions.")
  gdp_all <- gdp_r10_all %>%
    group_by(model, scenario, variable, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region = "World")
}

# ── R10 Population (for OECD LTM per-capita income) ──────────
run_python_download("Population", "pop_r10", r10_regions)
pop_r10_raw <- tryCatch(
  load_pq("pop_r10") %>%
    filter(variable == "Population", region %in% r10_regions),
  error = function(e) {
    message("Population data unavailable \u2014 OECD scale_i will be set to 1.")
    NULL
  }
)

# ── GSAT ───────────────────────────────────────────────────────
ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run fig2_gsat.R first.")
web_raw <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)])

# ══════════════════════════════════════════════════════════════
# 2. MODEL SELECTION
# ══════════════════════════════════════════════════════════════

models_gsat <- web_raw %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         region == "World", scenario %in% scenarios) %>%
  mutate(pct_col = case_when(
    grepl("Median|50th", variable, ignore.case = TRUE) ~ "p50",
    grepl("67th",        variable, ignore.case = TRUE) ~ "p67",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value) %>%
  filter(!is.na(p50), !is.na(p67)) %>%
  pull(model) %>% as.character() %>% unique()

# World-level endogeneity filter
cands_w <- filter_latest_remind(
  sort(intersect(unique(as.character(gdp_all$model)), models_gsat)))
cands_w <- cands_w[cands_w != "WITCH 6.0-EU28"]

meet_w <- gdp_all %>% filter(model %in% cands_w, scenario == ma_scen, !is.na(value)) %>%
  select(model, year, meet_val = value)
endo_w <- gdp_all %>% filter(model %in% cands_w, scenario != ma_scen, !is.na(value)) %>%
  inner_join(meet_w, by = c("model", "year")) %>%
  mutate(pct_diff = abs(value - meet_val) / meet_val) %>%
  filter(is.finite(pct_diff), pct_diff > ENDOGENOUS_THRESHOLD) %>%
  pull(model) %>% as.character() %>% unique()
use_models_w <- filter_latest_remind(sort(intersect(cands_w, endo_w)))

# Cluster-level endogeneity filter
cands_cl <- filter_latest_remind(
  sort(intersect(unique(as.character(gdp_r10_all$model)), models_gsat)))
cands_cl <- cands_cl[cands_cl != "WITCH 6.0-EU28"]

meet_cl <- gdp_r10_all %>% filter(model %in% cands_cl, scenario == ma_scen, !is.na(value)) %>%
  group_by(model, year) %>% summarise(meet_val = sum(value, na.rm = TRUE), .groups = "drop")
endo_cl <- gdp_r10_all %>% filter(model %in% cands_cl, scenario != ma_scen, !is.na(value)) %>%
  group_by(model, scenario, year) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  inner_join(meet_cl, by = c("model", "year")) %>%
  mutate(pct_diff = abs(value - meet_val) / meet_val) %>%
  filter(is.finite(pct_diff), pct_diff > ENDOGENOUS_THRESHOLD) %>%
  pull(model) %>% as.character() %>% unique()
use_models_cl <- filter_latest_remind(sort(intersect(cands_cl, endo_cl)))

# Common model set
use_models <- filter_latest_remind(sort(intersect(use_models_w, use_models_cl)))
message("Models (world + cluster + GSAT + endogenous): ", paste(use_models, collapse = ", "))

gdp_all     <- gdp_all     %>% filter(model %in% use_models)
gdp_r10_all <- gdp_r10_all %>% filter(model %in% use_models)

web_gsat_all <- web_raw %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         model %in% use_models, region == "World",
         scenario %in% scenarios, year >= 2020, year <= 2100) %>%
  mutate(pct_col = case_when(
    grepl("Median|50th", variable, ignore.case = TRUE) ~ "p50",
    grepl("67th",        variable, ignore.case = TRUE) ~ "p67",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value)

gdp_cluster_all <- gdp_r10_all %>%
  mutate(cluster = cluster_map[region]) %>%
  filter(!is.na(cluster)) %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

# ══════════════════════════════════════════════════════════════
# 3. TIMESERIES DATA  (panel a)
# ══════════════════════════════════════════════════════════════
ts_list <- list()

for (mod in use_models) {
  gdp_raw  <- gdp_all      %>% filter(model == mod)
  web_gsat <- web_gsat_all %>% filter(model == mod)
  if (nrow(gdp_raw) == 0 || nrow(web_gsat) == 0) {
    message("Skipping TS ", mod, " \u2014 missing data"); next
  }
  gdp_filled <- gdp_raw %>% fill_sparse_w()
  joined <- gdp_filled %>%
    filter(year %in% gdp_years_std) %>%
    mutate(scenario = as.character(scenario)) %>%
    inner_join(
      web_gsat %>% filter(year %in% gdp_years_std) %>% mutate(scenario = as.character(scenario)),
      by = c("model", "scenario", "year")
    ) %>%
    mutate(gdp_v1 = value * (1 - damage_fn(p67) / 100))

  message("  [diag] ", mod, ": joined=", nrow(joined),
          " joined_scens=", paste(unique(joined$scenario), collapse="|"))
  meet_ref <- joined %>% filter(scenario == ma_scen) %>%
    select(year, meet_raw = value, meet_v1 = gdp_v1)
  if (nrow(meet_ref) < 2) { message("Skipping TS ", mod, " \u2014 no Meet"); next }

  ts_list[[mod]] <- joined %>%
    filter(scenario %in% scens_nonmeet) %>%
    left_join(meet_ref, by = "year") %>%
    mutate(
      pct_raw     = (value  - meet_raw) / meet_raw * 100,
      pct_v1      = (gdp_v1 - meet_v1)  / meet_v1  * 100,
      model_short = short_name(mod)
    )
}
if (length(ts_list) == 0) stop("No model timeseries computed.")

ts_long <- bind_rows(ts_list) %>%
  pivot_longer(c(pct_raw, pct_v1), names_to = "type", values_to = "pct") %>%
  mutate(
    line_type = factor(
      if_else(type == "pct_raw", "Undamaged GDP", "Damage-adjusted GDP"),
      levels = c("Undamaged GDP", "Damage-adjusted GDP")
    ),
    model_short = factor(model_short, levels = sapply(use_models, short_name))
  )


# ══════════════════════════════════════════════════════════════
# 4. WORLD NPV DATA  (panel b)
# ══════════════════════════════════════════════════════════════
set.seed(42)
mc_alpha_draws <- rnorm(N_MC, ALPHA_C, SE_ALPHA_C)
mc_beta_draws  <- rnorm(N_MC, BETA_C,  SE_BETA_C)

results_list_w <- list()

for (mod in use_models) {
  message("World NPV: ", mod)
  gdp_raw  <- gdp_all      %>% filter(model == mod)
  web_gsat <- web_gsat_all %>% filter(model == mod)
  if (nrow(gdp_raw) == 0 || nrow(web_gsat) == 0) next
  gdp_filled <- gdp_raw %>% fill_sparse_w()

  joined <- gdp_filled %>%
    filter(year %in% gdp_years_std) %>%
    mutate(scenario = as.character(scenario)) %>%
    inner_join(
      web_gsat %>% filter(year %in% gdp_years_std) %>% mutate(scenario = as.character(scenario)),
      by = c("model", "scenario", "year")
    )
  meet_ref <- joined %>% filter(scenario == ma_scen) %>%
    select(year, meet_raw = value, meet_p50 = p50, meet_p67 = p67)
  if (nrow(meet_ref) < 2) next
  meet_npv_raw <- npv_abs_ann(meet_ref$meet_raw, meet_ref$year)

  nonmeet_jn <- joined %>% filter(scenario %in% scens_nonmeet) %>%
    left_join(meet_ref, by = "year")

  npv_summ <- nonmeet_jn %>%
    group_by(scenario) %>%
    summarise(
      npv_raw_pct = (npv_abs_ann(value, year) - meet_npv_raw) / meet_npv_raw * 100,
      .groups = "drop"
    )

  mc_list <- lapply(scens_nonmeet, function(scen) {
    d <- nonmeet_jn %>%
      filter(as.character(scenario) == scen,
             !is.na(p50), !is.na(p67), !is.na(meet_p50), !is.na(meet_p67)) %>%
      arrange(year)
    if (nrow(d) == 0) return(NULL)
    ann_y <- seq(BASE_YEAR, 2100L)
    disc  <- 1 / (1 + DEFAULT_DISC)^(ann_y - BASE_YEAR)
    gdp_s <- approx(d$year, d$value,    xout = ann_y, rule = 2)$y
    gdp_m <- approx(d$year, d$meet_raw, xout = ann_y, rule = 2)$y
    Ts50  <- pmax(approx(d$year, d$p50,      xout = ann_y, rule = 2)$y, 0)
    Ts67  <- pmax(approx(d$year, d$p67,      xout = ann_y, rule = 2)$y, 0)
    Tm50  <- pmax(approx(d$year, d$meet_p50, xout = ann_y, rule = 2)$y, 0)
    Tm67  <- pmax(approx(d$year, d$meet_p67, xout = ann_y, rule = 2)$y, 0)
    mc_npv <- function(T_s, T_m) {
      dam_s <- outer(T_s^1.5, mc_alpha_draws) + outer(T_s, mc_beta_draws)
      dam_m <- outer(T_m^1.5, mc_alpha_draws) + outer(T_m, mc_beta_draws)
      gmc_s <- sweep(1 - dam_s / 100, 1, gdp_s, `*`)
      gmc_m <- sweep(1 - dam_m / 100, 1, gdp_m, `*`)
      # Normalise by the DAMAGE-ADJUSTED Meet NPV, computed per Monte Carlo
      # draw so numerator and denominator share the same damage coefficients.
      # Both sides of the difference already carry damages.
      meet_npv_dam <- colSums(sweep(gmc_m, 1, disc, `*`))
      colSums(sweep(gmc_s - gmc_m, 1, disc, `*`)) / meet_npv_dam * 100
    }
    mc_v1 <- mc_npv(Ts50, Tm50)
    mc_v3 <- mc_npv(Ts67, Tm67)
    tibble(scenario = scen,
           q10_v1_pct = quantile(mc_v1, 0.10),
           q50_v1_pct = quantile(mc_v1, 0.50),
           q90_v1_pct = quantile(mc_v1, 0.90),
           q10_v3_pct = quantile(mc_v3, 0.10),
           q50_v3_pct = quantile(mc_v3, 0.50),
           q90_v3_pct = quantile(mc_v3, 0.90))
  })
  results_list_w[[mod]] <- bind_rows(mc_list) %>%
    left_join(npv_summ, by = "scenario") %>%
    mutate(model = mod, model_short = short_name(mod))
}
if (length(results_list_w) == 0) stop("No world NPV results.")

results_w <- bind_rows(results_list_w) %>%
  mutate(model_short = factor(model_short, levels = sapply(use_models, short_name)))

# ══════════════════════════════════════════════════════════════
# 5. CLUSTER NPV DATA  (panel c)
#    H&S MC uncertainty downscaled by OECD LTM C11 regional ratios
#    (Guillemette 2025, OECD Economic Policy Papers No. 36, Annex C)
#
#    θ_i     = -0.0182*(ycap_i − ycap_w) + 0.0037*(T̄_i − T̄_w)
#    scale_i = (0.010038 + θ_i / 6.25) / 0.010038          [C11 regional ratio]
#
#    Regional damage^k = H&S_mc_k(T) × scale_i
#      → MC uncertainty from H&S α/β draws; regional heterogeneity from C11
#
#    Cluster-level damaged GDP:
#      GDP_dam^k = GDP_total - H&S_mc_k(T)/100 × GDP_scaled
#    where GDP_scaled = Σ_i scale_i × GDP_i
#
#    Assumptions
#    ─────────────────────────────────────────────────────────
#    • ycap_i  = log(GDP|PPP per capita) at REF_YEAR 2015, Meet Aspirations
#      (time-invariant: reference-year calibration, not projected)
#    • T̄_i    = population-weighted annual-mean temperature per R10 (2015)
#    • World averages are GDP|PPP-weighted across available R10 regions
#    • Global MAGICC GSAT used for all regions (no regional warming pattern)
# ══════════════════════════════════════════════════════════════

# ── Baseline temperatures by R10 (population-weighted annual mean, 2015) ───────
# Source: Gortan et al. (2024), "A unified repository for pre-processed climate
#   data weighted by gridded economic activity", Scientific Data 11, 533.
#   CRU TS v4, population weights (2015 census), monthly data averaged to annual.
#   R10 aggregation: country-level values weighted by 2015 World Bank population.
#   Script: scripts/compute_r10_temps.py
# "Other (R10)" has no matching countries in GADM0 → NA (scale_i defaults to 1).
r10_baseline_temp <- c(
  "Africa (R10)"              = 23.82,
  "China+ (R10)"              = 15.38,
  "Europe (R10)"              = 12.03,
  "India+ (R10)"              = 25.55,
  "Latin America (R10)"       = 21.92,
  "Middle East (R10)"         = 21.32,
  "North America (R10)"       = 13.87,
  "Other (R10)"               = 19.5,        # no GADM0 match; retained from prior estimate
  "Pacific OECD (R10)"        = 14.69,
  "Reforming Economies (R10)" =  8.80,
  "Rest of Asia (R10)"        = 26.79
)

REF_YEAR <- 2015L   # reference year for ycap calibration

# ── OECD θ_i and scale_i per model × R10 region ────────────────
#   θ_i     = -0.0182*(ycap_i − ycap_w) + 0.0037*(T̄_i − T̄_w)
#   scale_i = (0.010038 + θ_i / 6.25) / 0.010038
#   Regional damage = H&S_global(T) × scale_i
#   (H&S MC uncertainty downscaled by C11 regional ratio)
compute_oecd_scale <- function(mod) {
  default_scales <- tibble(region = r10_regions, theta_i = 0.0, scale_i = 1.0)

  gdp_ref <- gdp_ppp_r10_raw %>%
    filter(as.character(model) == mod,
           as.character(scenario) == ma_scen,
           year == REF_YEAR, region %in% r10_regions, !is.na(value)) %>%
    select(region, gdp_ppp = value)

  if (nrow(gdp_ref) == 0) {
    message("  OECD scale: no GDP|PPP for ", mod, " at ", REF_YEAR,
            " \u2014 scale_i = 1 (uniform)")
    return(default_scales)
  }

  pop_ref <- if (!is.null(pop_r10_raw) && nrow(pop_r10_raw) > 0) {
    tmp <- pop_r10_raw %>%
      filter(as.character(model) == mod,
             as.character(scenario) == ma_scen,
             year == REF_YEAR, region %in% r10_regions, !is.na(value)) %>%
      select(region, pop = value)
    if (nrow(tmp) == 0) {
      tmp <- pop_r10_raw %>%
        filter(as.character(scenario) == ma_scen,
               year == REF_YEAR, region %in% r10_regions, !is.na(value)) %>%
        group_by(region) %>%
        summarise(pop = mean(value, na.rm = TRUE), .groups = "drop")
      if (nrow(tmp) > 0)
        message("  OECD scale: using cross-model mean population for ", mod)
    }
    tmp
  } else tibble(region = character(0), pop = numeric(0))

  if (nrow(pop_ref) == 0) {
    message("  OECD scale: no population for ", mod, " \u2014 scale_i = 1 (uniform)")
    return(default_scales)
  }

  r10_base <- gdp_ref %>%
    inner_join(pop_ref, by = "region") %>%
    mutate(
      region    = as.character(region),
      avgtemp_i = r10_baseline_temp[region],
      ycap_i    = log(gdp_ppp / pop * 1000)
    ) %>%
    filter(!is.na(avgtemp_i), is.finite(ycap_i), pop > 0)

  if (nrow(r10_base) < 3) {
    message("  OECD scale: too few regions for ", mod, " \u2014 scale_i = 1 (uniform)")
    return(default_scales)
  }

  total_gdp   <- sum(r10_base$gdp_ppp)
  wld_ycap    <- sum(r10_base$ycap_i    * r10_base$gdp_ppp) / total_gdp
  wld_avgtemp <- sum(r10_base$avgtemp_i * r10_base$gdp_ppp) / total_gdp

  r10_base <- r10_base %>%
    mutate(
      theta_i = -0.0182 * (ycap_i - wld_ycap) + 0.0037 * (avgtemp_i - wld_avgtemp),
      scale_i = (0.010038 + theta_i / 6.25) / 0.010038
    )

  message("  OECD scale for ", short_name(mod), ": ",
          paste(sprintf("%s=%.3f", r10_base$region, r10_base$scale_i), collapse = ", "))

  default_scales %>%
    left_join(r10_base %>% select(region, theta_new = theta_i, scale_new = scale_i),
              by = "region") %>%
    mutate(theta_i = if_else(!is.na(theta_new), theta_new, theta_i),
           scale_i = if_else(!is.na(scale_new), scale_new, scale_i)) %>%
    select(region, theta_i, scale_i)
}

oecd_scale_list <- setNames(lapply(use_models, compute_oecd_scale), use_models)

# ── Cluster NPV MC loop — H&S uncertainty × C11 scale_i ─────────
#   damage_i^k = H&S_mc_k(T) × scale_i   (MC uncertainty, C11 regional ratio)
#   GDP_dam_cluster^k = GDP_total - H&S_mc_k/100 × GDP_scaled
#   where GDP_scaled = Σ_i scale_i × GDP_i
results_list_cl <- list()

for (mod in use_models) {
  message("Cluster NPV (H&S MC × C11 scale): ", mod)
  web_gsat  <- web_gsat_all %>% filter(model == mod)
  gdp_r10_m <- gdp_r10_all  %>% filter(as.character(model) == mod)
  scale_df  <- oecd_scale_list[[mod]]

  if (nrow(web_gsat) == 0 || nrow(gdp_r10_m) == 0) next

  ann_y <- seq(BASE_YEAR, 2100L)
  disc  <- 1 / (1 + DEFAULT_DISC)^(ann_y - BASE_YEAR)
  gm    <- web_gsat %>% filter(scenario == ma_scen) %>% arrange(year)
  if (nrow(gm) < 2) next
  Tm50 <- pmax(approx(gm$year, gm$p50, xout = ann_y, rule = 2)$y, 0)
  Tm67 <- pmax(approx(gm$year, gm$p67, xout = ann_y, rule = 2)$y, 0)

  # Meet H&S MC damage matrices (global GSAT, shared across clusters)
  dam_m50 <- outer(Tm50^1.5, mc_alpha_draws) + outer(Tm50, mc_beta_draws)
  dam_m67 <- outer(Tm67^1.5, mc_alpha_draws) + outer(Tm67, mc_beta_draws)

  mod_rows <- bind_rows(lapply(cluster_order, function(cl) {
    cl_regions <- names(cluster_map)[cluster_map == cl & names(cluster_map) %in% r10_regions]

    # Meet Aspirations: total GDP and scale-weighted GDP
    gdp_total_m  <- numeric(length(ann_y))
    gdp_scaled_m <- numeric(length(ann_y))
    for (rg in cl_regions) {
      sc_i <- scale_df$scale_i[scale_df$region == rg]
      if (!length(sc_i) || !is.finite(sc_i)) sc_i <- 1.0
      rg_d <- gdp_r10_m %>%
        filter(as.character(scenario) == ma_scen, region == rg) %>% arrange(year)
      if (nrow(rg_d) < 2) next
      v <- approx(rg_d$year, rg_d$value, xout = ann_y, rule = 2)$y
      gdp_total_m  <- gdp_total_m  + v
      gdp_scaled_m <- gdp_scaled_m + sc_i * v
    }
    if (all(gdp_total_m == 0)) return(NULL)

    meet_npv_raw <- sum(gdp_total_m * disc)
    if (!is.finite(meet_npv_raw) || meet_npv_raw == 0) return(NULL)

    # GDP_dam^k = GDP_total - H&S_mc_k/100 × GDP_scaled
    gmc_m50 <- matrix(gdp_total_m, nrow = length(ann_y), ncol = N_MC) -
      sweep(dam_m50 / 100, 1, gdp_scaled_m, `*`)
    gmc_m67 <- matrix(gdp_total_m, nrow = length(ann_y), ncol = N_MC) -
      sweep(dam_m67 / 100, 1, gdp_scaled_m, `*`)
    meet_npv_dam50 <- colSums(sweep(gmc_m50, 1, disc, `*`))
    meet_npv_dam67 <- colSums(sweep(gmc_m67, 1, disc, `*`))

    bind_rows(lapply(scens_nonmeet, function(scen) {
      gs <- web_gsat %>% filter(as.character(scenario) == scen) %>% arrange(year)
      if (nrow(gs) < 2) return(NULL)
      Ts50 <- pmax(approx(gs$year, gs$p50, xout = ann_y, rule = 2)$y, 0)
      Ts67 <- pmax(approx(gs$year, gs$p67, xout = ann_y, rule = 2)$y, 0)
      dam_s50 <- outer(Ts50^1.5, mc_alpha_draws) + outer(Ts50, mc_beta_draws)
      dam_s67 <- outer(Ts67^1.5, mc_alpha_draws) + outer(Ts67, mc_beta_draws)

      gdp_total_s  <- numeric(length(ann_y))
      gdp_scaled_s <- numeric(length(ann_y))
      for (rg in cl_regions) {
        sc_i <- scale_df$scale_i[scale_df$region == rg]
        if (!length(sc_i) || !is.finite(sc_i)) sc_i <- 1.0
        rg_d <- gdp_r10_m %>%
          filter(as.character(scenario) == scen, region == rg) %>% arrange(year)
        if (nrow(rg_d) < 2) next
        v <- approx(rg_d$year, rg_d$value, xout = ann_y, rule = 2)$y
        gdp_total_s  <- gdp_total_s  + v
        gdp_scaled_s <- gdp_scaled_s + sc_i * v
      }
      if (all(gdp_total_s == 0)) return(NULL)

      npv_raw_pct <- (sum(gdp_total_s * disc) - meet_npv_raw) / meet_npv_raw * 100

      gmc_s50 <- matrix(gdp_total_s, nrow = length(ann_y), ncol = N_MC) -
        sweep(dam_s50 / 100, 1, gdp_scaled_s, `*`)
      gmc_s67 <- matrix(gdp_total_s, nrow = length(ann_y), ncol = N_MC) -
        sweep(dam_s67 / 100, 1, gdp_scaled_s, `*`)

      mc_v1 <- colSums(sweep(gmc_s50 - gmc_m50, 1, disc, `*`)) / meet_npv_dam50 * 100
      mc_v3 <- colSums(sweep(gmc_s67 - gmc_m67, 1, disc, `*`)) / meet_npv_dam67 * 100

      tibble(scenario = scen, cluster = cl,
             npv_raw_pct = npv_raw_pct,
             q10_v1_pct  = quantile(mc_v1, 0.10),
             q50_v1_pct  = quantile(mc_v1, 0.50),
             q90_v1_pct  = quantile(mc_v1, 0.90),
             q10_v3_pct  = quantile(mc_v3, 0.10),
             q50_v3_pct  = quantile(mc_v3, 0.50),
             q90_v3_pct  = quantile(mc_v3, 0.90))
    }))
  }))

  if (!is.null(mod_rows) && nrow(mod_rows) > 0)
    results_list_cl[[mod]] <- mod_rows %>%
      mutate(model = mod, model_short = short_name(mod))
}
if (length(results_list_cl) == 0) stop("No cluster NPV results.")

results_cl <- bind_rows(results_list_cl) %>%
  mutate(
    cluster     = factor(cluster, levels = cluster_order),
    model_short = factor(model_short, levels = sapply(use_models, short_name))
  )

# ══════════════════════════════════════════════════════════════
# 6. SHARED AESTHETICS
# ══════════════════════════════════════════════════════════════
mod_short_levels <- levels(results_w$model_short)
n_mods           <- length(mod_short_levels)
model_colors     <- setNames(scales::hue_pal(l = 55, c = 100)(n_mods), mod_short_levels)

spread_w  <- min(0.30, n_mods * 0.07)
offsets_w <- setNames(seq(-spread_w,  spread_w,  length.out = max(n_mods, 2)), mod_short_levels)
spread_cl <- min(0.28, n_mods * 0.07)
offsets_cl <- setNames(seq(-spread_cl, spread_cl, length.out = max(n_mods, 2)), mod_short_levels)

x_v1 <- 1L   # T_median column
x_v3 <- 2L   # T_67th column

# ══════════════════════════════════════════════════════════════
# 7. FIGURE FACTORY
# ══════════════════════════════════════════════════════════════
make_figure <- function(target_ssp2, label, out_file, pb_ylim) {
  message("\n\u2500\u2500 Building: ", label, " \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500")

  # ── Panel (a): GDP timeseries ─────────────────────────────────
  ts_s  <- ts_long %>% filter(scenario == target_ssp2)

  p_a <- ggplot(ts_s,
                aes(x = year, y = pct,
                    colour   = model_short,
                    linetype = line_type,
                    group    = interaction(model_short, type))) +
    geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = model_colors, name = NULL) +
    scale_linetype_manual(
      values = c("Undamaged GDP" = "dashed", "Damage-adjusted GDP" = "solid"),
      labels = c("Undamaged GDP"     = "Undamaged GDP",
                 "Damage-adjusted GDP" = "Damage-adjusted\nGDP for T67"),
      name   = NULL
    ) +
    scale_x_continuous(breaks = seq(2020, 2100, 20)) +
    labs(
      title = paste0("(a) ", label, " GDP timeseries vs Meet Aspirations"),
      x = NULL, y = "Annual GDP vs Meet Aspirations (%)"
    ) +
    guides(
      colour   = guide_legend(ncol = 1, order = 1,
                              override.aes = list(linewidth = 1.5, linetype = "solid")),
      linetype = guide_legend(ncol = 1, order = 2,
                              keywidth = unit(2.5, "cm"))
    ) +
    theme(legend.position  = "right",
          axis.text.x      = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(colour = "grey85", linewidth = 0.3))

  # ── Panel (b): World NPV (T_median | T_67th x-axis) ──────────
  pdb <- results_w %>%
    filter(scenario == target_ssp2) %>%
    mutate(
      x_dot_v1 = x_v1 + offsets_w[as.character(model_short)],
      x_dot_v3 = x_v3 + offsets_w[as.character(model_short)],
      lbl_v1   = sprintf("%+.1f%%", q50_v1_pct),
      lbl_v3   = sprintf("%+.1f%%", q50_v3_pct)
    )
  p_b <- ggplot() +
    annotate("rect", xmin = 0.45, xmax = 1.55, ymin = -Inf, ymax = Inf,
             fill = "#FFF9C4", alpha = 0.65) +
    annotate("rect", xmin = 1.55, xmax = 2.55, ymin = -Inf, ymax = Inf,
             fill = "#FFB6C1", alpha = 0.55) +
    geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +
    # Per-model: T_median column
    geom_errorbar(data = pdb,
                  aes(x = x_dot_v1, ymin = q10_v1_pct, ymax = q90_v1_pct,
                      colour = model_short),
                  width = 0.05, linewidth = 0.9) +
    geom_point(data = pdb, aes(x = x_dot_v1, y = q50_v1_pct, colour = model_short),
               shape = 16, size = 4.0) +
    geom_text(data  = pdb, aes(x = x_dot_v1, y = q50_v1_pct, label = lbl_v1,
                                colour = model_short),
              hjust = 1.30, size = 4.8, show.legend = FALSE) +
    # Per-model: T_67th column
    geom_errorbar(data = pdb,
                  aes(x = x_dot_v3, ymin = q10_v3_pct, ymax = q90_v3_pct,
                      colour = model_short),
                  width = 0.05, linewidth = 0.9) +
    geom_point(data = pdb, aes(x = x_dot_v3, y = q50_v3_pct, colour = model_short),
               shape = 16, size = 4.0) +
    geom_text(data  = pdb, aes(x = x_dot_v3, y = q50_v3_pct, label = lbl_v3,
                                colour = model_short),
              hjust = -0.30, size = 4.8, show.legend = FALSE) +
    scale_colour_manual(values = model_colors, name = NULL) +
    scale_x_continuous(breaks = c(x_v1, x_v3),
                       labels = c("T\u2085\u2080\n(median)", "T\u2086\u2087\n(67th pct)"),
                       expand = expansion(add = 0.55)) +
    coord_cartesian(ylim = pb_ylim) +
    labs(
      title = "(b) GDP NPV global level",
      x = NULL, y = "NPV vs Meet Aspirations\n(% of damage-adjusted Meet NPV)"
    ) +
    guides(colour = "none") +
    theme(legend.position = "none",
          axis.text.x     = element_text(size = 15.5, face = "bold"))

  # ── Panel (c): Cluster NPV (T_median | T_67th x-axis) ────────
  pdc <- results_cl %>%
    filter(scenario == target_ssp2) %>%
    mutate(
      x_dot_v1 = x_v1 + offsets_cl[as.character(model_short)],
      x_dot_v3 = x_v3 + offsets_cl[as.character(model_short)],
      lbl_v1   = sprintf("%+.1f%%", q50_v1_pct),
      lbl_v3   = sprintf("%+.1f%%", q50_v3_pct)
    )
  p_c <- ggplot() +
    annotate("rect", xmin = 0.45, xmax = 1.55, ymin = -Inf, ymax = Inf,
             fill = "#FFF9C4", alpha = 0.65) +
    annotate("rect", xmin = 1.55, xmax = 2.55, ymin = -Inf, ymax = Inf,
             fill = "#FFB6C1", alpha = 0.55) +
    geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
    # Per-model: T_median column
    geom_errorbar(data = pdc,
                  aes(x = x_dot_v1, ymin = q10_v1_pct, ymax = q90_v1_pct,
                      colour = model_short),
                  width = 0.05, linewidth = 0.8) +
    geom_point(data = pdc, aes(x = x_dot_v1, y = q50_v1_pct, colour = model_short),
               shape = 16, size = 3.5) +
    geom_text(data  = pdc, aes(x = x_dot_v1, y = q50_v1_pct, label = lbl_v1,
                                colour = model_short),
              hjust = 1.25, size = 4.3, show.legend = FALSE) +
    # Per-model: T_67th column
    geom_errorbar(data = pdc,
                  aes(x = x_dot_v3, ymin = q10_v3_pct, ymax = q90_v3_pct,
                      colour = model_short),
                  width = 0.05, linewidth = 0.8) +
    geom_point(data = pdc, aes(x = x_dot_v3, y = q50_v3_pct, colour = model_short),
               shape = 16, size = 3.5) +
    geom_text(data  = pdc, aes(x = x_dot_v3, y = q50_v3_pct, label = lbl_v3,
                                colour = model_short),
              hjust = -0.25, size = 4.3, show.legend = FALSE) +
    facet_wrap(~ cluster, ncol = 3, scales = "fixed") +
    scale_colour_manual(values = model_colors, name = NULL) +
    scale_x_continuous(breaks = c(x_v1, x_v3),
                       labels = c("T\u2085\u2080\n(median)", "T\u2086\u2087\n(67th pct)"),
                       expand = expansion(add = 0.55)) +
    labs(
      title = "(c) GDP NPV by regional cluster",
      x = NULL, y = "NPV vs Meet Aspirations\n(% of cluster damage-adjusted Meet NPV)"
    ) +
    guides(colour = "none") +
    theme(legend.position = "none",
          axis.text.x     = element_text(size = 14.3, face = "bold"))

  # ── Combine & save ────────────────────────────────────────────
  fig <- (p_a | p_b) / p_c +
    plot_layout(heights = c(1, 1), widths = c(2, 1))

  out_path <- file.path(graphs_dir, out_file)
  ggsave(out_path, fig, width = 15, height = 11, dpi = 300)
  message("Saved: ", out_path)
}

# ══════════════════════════════════════════════════════════════
# 8. PRODUCE ALL THREE FIGURES
# ══════════════════════════════════════════════════════════════

# Shared y-axis range for panel (b) — same across all three scenarios
# so figures fig3 / SM3 / SM4 are directly comparable.
pb_raw  <- range(c(results_w$q10_v1_pct, results_w$q90_v1_pct,
                   results_w$q10_v3_pct, results_w$q90_v3_pct), na.rm = TRUE)
pb_ypad <- diff(pb_raw) * 0.08
pb_ylim <- pb_raw + c(-pb_ypad, pb_ypad)

make_figure("SSP2 - Asymmetric Roll-Back", "Shifted Ambition", "fig3.png",    pb_ylim)
make_figure("SSP2 - Late Reawakening",     "Late Reawakening", "fig_sm7.png", pb_ylim)
make_figure("SSP2 - Staying Alive",        "Tech Momentum",    "fig_sm8.png", pb_ylim)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig3", graphs_dir)
  }
})
