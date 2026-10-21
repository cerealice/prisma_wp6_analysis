library(tidyverse)
library(arrow)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 supplementary figure SM2
#   GDP NPV sensitivity to discount rate — T_median (p50) only.
#   Panel (b) style from fig2.R replicated at four discount
#   rates (1 %–4 %).  x-axis = discount rate.
#   3 subplots (columns), one per non-Meet scenario.
#
# Output: graphs/post_oct15/fig_sm2.png
# ══════════════════════════════════════════════════════════════

# ── Constants ──────────────────────────────────────────────────
CAT_WEIGHT    <- 0.621
GROWTH_WEIGHT <- 0.602
ALPHA_C       <- 0.622 + 1.775 * CAT_WEIGHT
BETA_C        <- 1.997 * GROWTH_WEIGHT
SE_ALPHA_C    <- sqrt(0.265^2 + (CAT_WEIGHT * 1.858)^2)
SE_BETA_C     <- GROWTH_WEIGHT * 1.130
N_MC          <- 5000L
BASE_YEAR     <- 2020L
ENDOGENOUS_THRESHOLD <- 0.005

DISC_RATES  <- c(0.01, 0.02, 0.03, 0.04)
DISC_LABELS <- c("1%", "2%\n(default)", "3%", "4%")

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
  theme_bw(base_size = 15, base_family = "Arial") +
    theme(
      plot.title         = element_text(size = 17.0, face = "bold"),
      plot.caption       = element_text(size = 11.5, colour = "black", hjust = 0),
      axis.title         = element_text(size = 13.6),
      axis.text          = element_text(size = 12.4),
      strip.background   = element_blank(),
      strip.text         = element_text(size = 14.3, face = "bold"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.title       = element_text(size = 13.6),
      legend.text        = element_text(size = 13.0)
    )
)

# ── Download / load helpers ────────────────────────────────────
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

# ══════════════════════════════════════════════════════════════
# 1. LOAD DATA  (same parquets as fig2.R)
# ══════════════════════════════════════════════════════════════
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

ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run fig2.R first.")
web_raw <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)])

# ══════════════════════════════════════════════════════════════
# 2. MODEL SELECTION  (T50 / p50 only)
# ══════════════════════════════════════════════════════════════
models_gsat <- web_raw %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         region == "World", scenario %in% scenarios) %>%
  mutate(pct_col = case_when(
    grepl("Median|50th", variable, ignore.case = TRUE) ~ "p50",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value) %>%
  filter(!is.na(p50)) %>%
  pull(model) %>% as.character() %>% unique()

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
use_models <- filter_latest_remind(sort(intersect(cands_w, endo_w)))
message("Models: ", paste(use_models, collapse = ", "))

gdp_all <- gdp_all %>% filter(model %in% use_models)

web_gsat_all <- web_raw %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         model %in% use_models, region == "World",
         scenario %in% scenarios, year >= 2020, year <= 2100) %>%
  mutate(pct_col = case_when(
    grepl("Median|50th", variable, ignore.case = TRUE) ~ "p50",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value)

# ══════════════════════════════════════════════════════════════
# 3. COMPUTE NPV FOR EACH DISCOUNT RATE (T50 only)
#    Same MC draws for all rates — only discounting changes.
# ══════════════════════════════════════════════════════════════
set.seed(42)
mc_alpha_draws <- rnorm(N_MC, ALPHA_C, SE_ALPHA_C)
mc_beta_draws  <- rnorm(N_MC, BETA_C,  SE_BETA_C)

ann_y_full <- seq(BASE_YEAR, 2100L)

results_dr <- bind_rows(lapply(seq_along(DISC_RATES), function(di) {
  dr   <- DISC_RATES[di]
  dlbl <- DISC_LABELS[di]
  message("\nDiscount rate: ", dlbl)
  disc_v <- 1 / (1 + dr)^(ann_y_full - BASE_YEAR)

  bind_rows(lapply(use_models, function(mod) {
    gdp_raw  <- gdp_all      %>% filter(model == mod)
    web_gsat <- web_gsat_all %>% filter(model == mod)
    if (nrow(gdp_raw) == 0 || nrow(web_gsat) == 0) return(NULL)
    gdp_filled <- gdp_raw %>% fill_sparse_w()

    joined <- gdp_filled %>%
      filter(year %in% gdp_years_std) %>%
      mutate(scenario = as.character(scenario)) %>%
      inner_join(
        web_gsat %>% filter(year %in% gdp_years_std) %>%
          mutate(scenario = as.character(scenario)),
        by = c("model", "scenario", "year")
      )
    meet_ref <- joined %>% filter(scenario == ma_scen) %>%
      select(year, meet_raw = value, meet_p50 = p50)
    if (nrow(meet_ref) < 2) return(NULL)

    nonmeet_jn <- joined %>% filter(scenario %in% scens_nonmeet) %>%
      left_join(meet_ref, by = "year")

    bind_rows(lapply(scens_nonmeet, function(scen) {
      d <- nonmeet_jn %>%
        filter(as.character(scenario) == scen,
               !is.na(p50), !is.na(meet_p50)) %>%
        arrange(year)
      if (nrow(d) == 0) return(NULL)

      gdp_s <- approx(d$year, d$value,    xout = ann_y_full, rule = 2)$y
      gdp_m <- approx(d$year, d$meet_raw, xout = ann_y_full, rule = 2)$y
      Ts50  <- pmax(approx(d$year, d$p50,      xout = ann_y_full, rule = 2)$y, 0)
      Tm50  <- pmax(approx(d$year, d$meet_p50, xout = ann_y_full, rule = 2)$y, 0)

      mc_npv50 <- function(T_s, T_m) {
        dam_s <- outer(T_s^1.5, mc_alpha_draws) + outer(T_s, mc_beta_draws)
        dam_m <- outer(T_m^1.5, mc_alpha_draws) + outer(T_m, mc_beta_draws)
        gmc_s <- sweep(1 - dam_s / 100, 1, gdp_s, `*`)
        gmc_m <- sweep(1 - dam_m / 100, 1, gdp_m, `*`)
        meet_npv_dam <- colSums(sweep(gmc_m, 1, disc_v, `*`))
        colSums(sweep(gmc_s - gmc_m, 1, disc_v, `*`)) / meet_npv_dam * 100
      }
      mc_v1 <- mc_npv50(Ts50, Tm50)

      tibble(
        model       = mod,
        model_short = short_name(mod),
        scenario    = scen,
        disc_rate   = dr,
        disc_label  = dlbl,
        q10_v1_pct  = quantile(mc_v1, 0.10),
        q50_v1_pct  = quantile(mc_v1, 0.50),
        q90_v1_pct  = quantile(mc_v1, 0.90)
      )
    }))
  }))
}))

# ── Factors ────────────────────────────────────────────────────
results_dr <- results_dr %>%
  mutate(
    disc_label     = factor(disc_label, levels = DISC_LABELS),
    scenario_label = factor(scen_labels[scenario], levels = unname(scen_labels)),
    model_short    = factor(model_short, levels = sapply(use_models, short_name))
  )

# ══════════════════════════════════════════════════════════════
# 4. PLOT
# ══════════════════════════════════════════════════════════════
mod_short_levels <- levels(results_dr$model_short)
n_mods           <- length(mod_short_levels)
model_colors_sm  <- setNames(scales::hue_pal(l = 55, c = 100)(n_mods), mod_short_levels)

p <- ggplot(results_dr,
            aes(x = disc_label, y = q50_v1_pct,
                colour = model_short, group = model_short)) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = q10_v1_pct, ymax = q90_v1_pct),
                width = 0.15, linewidth = 0.65, alpha = 0.75) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.8) +
  facet_wrap(~ scenario_label, nrow = 1) +
  scale_colour_manual(values = model_colors_sm, name = NULL) +
  labs(
    title   = "GDP NPV \u2014 discount rate sensitivity (T\u2085\u2080 median GSAT)",
    x       = "Discount rate",
    y       = "NPV vs Meet Aspirations (% of damage-adjusted Meet NPV)",
    caption = paste0(
      "Error bars = Q10\u2013Q90 H&S damage uncertainty (MC N\u202f=\u202f", format(N_MC, big.mark = ","), ").  ",
      "Discount rate '2% (default)' matches panel (b) of fig2.\n",
      sprintf(
        "\u03b1\u202f=\u202f%.3f (SE\u202f%.3f), \u03b2\u202f=\u202f%.3f (SE\u202f%.3f).  ",
        ALPHA_C, SE_ALPHA_C, BETA_C, SE_BETA_C
      ),
      "D(T)\u202f=\u202f\u03b1\u00b7T^1.5\u202f+\u202f\u03b2\u00b7T.  Endogenous GDP models only."
    )
  ) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 1.5))) +
  theme(legend.position = "right",
        axis.text.x     = element_text(size = 13.0, face = "bold"))

n_scens  <- length(scens_nonmeet)
out_file <- file.path(graphs_dir, "fig_sm3.png")
ggsave(out_file, p,
       width  = 4.2 * n_scens + 2.5,
       height = 5.5,
       dpi    = 300,
       limitsize = FALSE)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig_sm3", graphs_dir)
  }
})
