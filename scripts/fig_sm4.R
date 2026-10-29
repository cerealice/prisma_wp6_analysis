library(tidyverse)
library(arrow)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — damage function sensitivity (post-Oct-15)
#
# Replicates panel (b) of fig3 exactly — GDP NPV vs damage-adjusted
# Meet Aspirations — but for ALL 9 Howard & Sterner (2025) damage
# functions instead of the single preferred specification.
#
# Layout : rows = 3 non-Meet scenarios (Shifted Ambition / Late
#                 Reawakening / Tech Momentum)
#          columns = 9 H&S damage functions
# Each cell = panel (b) of fig3:
#   x-axis : T₅₀ (GSAT median) | T₆₇ (GSAT 67th pct)
#   y-axis : % NPV change vs damage-adjusted Meet Aspirations
#   points : one per model (colored), slightly dodged
#   errbar : q10–q90 MC band for fn2–fn7 (H&S 2025 regression SE)
#   labels : q50 NPV change value
#
# MC uncertainty: draws from H&S (2025) Table 3 Spec 3 regression
#   α    ~ N(0.622, 0.265²) — T^1.5 non-catastrophic coefficient
#   β_cat ~ N(1.775, 1.858²) — additional T^1.5 catastrophic term
#   β_grow ~ N(1.997, 1.130²) — growth-effects T term
# fn1 (no-controls, different spec) and fn8/fn9 (H&S 2017 quadratic)
# use fixed coefficients — no uncertainty band drawn.
#
# ── H&S (2025) damage functions ────────────────────────────────
# General equation (Eq. 5):
#   D = α·T^1.5 + β_cat·cat·T^1.5 + β_grow·Growth·T
#
# cat    = share of catastrophic damage studies (0, 0.621, or 1)
# Growth = share capturing growth effects (0, 0.602, or 1)
#
#  (1) No-controls        0.861·T^1.5     Different regression spec; no SE
#  (2) Non-cat., Level    0.622·T^1.5     cat=0, G=0         [MC: α only]
#  (3) Non-cat., Growth   0.622·T^1.5+1.997·T   cat=0, G=1  [MC: α + β_grow]
#  (4) Cat., Level        2.397·T^1.5     cat=1, G=0         [MC: α+β_cat]
#  (5) Cat., Growth       2.397·T^1.5+1.997·T   cat=1, G=1  [MC: α+β_cat + β_grow]
#  (6) Primary synth. ★  1.724·T^1.5+1.202·T   PREFERRED    [MC: full weighted]
#  (7) Alt. synthetic     0.691·T^1.5+1.178·T   Alt. weights [MC: full weighted]
#  (8) H&S 2017 Simple    0.622·T²        H&S 2017 quadratic; no SE
#  (9) H&S 2017 Extended  0.440·T²        H&S 2017 quadratic; no SE
#
# Output: graphs/damage_fn_sensitivity.png
# ══════════════════════════════════════════════════════════════

# ── Constants (match fig3.R) ───────────────────────────────────
DEFAULT_DISC         <- 0.02
BASE_YEAR            <- 2020L
ENDOGENOUS_THRESHOLD <- 0.005   # >0.5% GDP diff vs Meet required
N_MC                 <- 5000L

# H&S (2025) Table 3 Spec 3 — raw regression parameters
ALPHA_RAW    <- 0.622; SE_ALPHA    <- 0.265
BETA_CAT     <- 1.775; SE_BETA_CAT <- 1.858
BETA_GROW    <- 1.997; SE_BETA_GROW <- 1.130

# fn7 alternative weights — back-computed so that
#   0.622 + 1.775·w_cat = 0.691  →  w_cat ≈ 0.0389
#   1.997·w_grow = 1.178          →  w_grow ≈ 0.5899
W_CAT_7  <- (0.691 - ALPHA_RAW) / BETA_CAT
W_GROW_7 <- 1.178 / BETA_GROW

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
ma_scen       <- "SSP2 - Meet Aspirations"
all_scenarios <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive"
)
scens_nonmeet <- c(
  "SSP2 - Asymmetric Roll-Back",  # row 1: Shifted Ambition
  "SSP2 - Late Reawakening",      # row 2: Late Reawakening
  "SSP2 - Staying Alive"          # row 3: Tech Momentum
)
scen_labels <- c(
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition",
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Staying Alive"        = "Tech Momentum"
)

# ── Model short names (match fig3.R) ───────────────────────────
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
short_name <- function(x) ifelse(x %in% names(model_short_names),
                                  model_short_names[x], x)

# ── H&S (2025) damage functions ────────────────────────────────
# Each spec: id, label, fn (fixed-coefficient), has_mc, w_cat, w_grow
# w_cat / w_grow define how the composite MC coefficient is assembled
# from (mc_alpha_raw, mc_beta_cat, mc_beta_grow):
#   a_mc = mc_alpha_raw + w_cat * mc_beta_cat
#   b_mc = w_grow * mc_beta_grow  (NULL if w_grow == 0)
damage_fns <- list(
  list(id = "fn1", has_mc = FALSE, w_cat = NA,      w_grow = NA,
       label = "(1) No-controls\n0.861\u00d7T^1.5",
       fn = function(T) 0.861 * T^1.5),

  list(id = "fn2", has_mc = TRUE,  w_cat = 0,       w_grow = 0,
       label = "(2) Non-cat., Level\n0.622\u00d7T^1.5",
       fn = function(T) 0.622 * T^1.5),

  list(id = "fn3", has_mc = TRUE,  w_cat = 0,       w_grow = 1,
       label = "(3) Non-cat., Growth\n0.622\u00d7T^1.5+1.997\u00d7T",
       fn = function(T) 0.622 * T^1.5 + 1.997 * T),

  list(id = "fn4", has_mc = TRUE,  w_cat = 1,       w_grow = 0,
       label = "(4) Cat., Level\n2.397\u00d7T^1.5",
       fn = function(T) 2.397 * T^1.5),

  list(id = "fn5", has_mc = TRUE,  w_cat = 1,       w_grow = 1,
       label = "(5) Cat., Growth\n2.397\u00d7T^1.5+1.997\u00d7T",
       fn = function(T) 2.397 * T^1.5 + 1.997 * T),

  list(id = "fn6", has_mc = TRUE,  w_cat = 0.621,   w_grow = 0.602,
       label = "(6) Primary synth. \u2605\n1.724\u00d7T^1.5+1.202\u00d7T",
       fn = function(T) 1.724 * T^1.5 + 1.202 * T),

  list(id = "fn7", has_mc = TRUE,  w_cat = W_CAT_7, w_grow = W_GROW_7,
       label = "(7) Alt. synthetic\n0.691\u00d7T^1.5+1.178\u00d7T",
       fn = function(T) 0.691 * T^1.5 + 1.178 * T),

  list(id = "fn8", has_mc = FALSE, w_cat = NA,      w_grow = NA,
       label = "(8) H&S 2017 Simple\n0.622\u00d7T\u00b2",
       fn = function(T) 0.622 * T^2),

  list(id = "fn9", has_mc = FALSE, w_cat = NA,      w_grow = NA,
       label = "(9) H&S 2017 Extended\n0.440\u00d7T\u00b2",
       fn = function(T) 0.440 * T^2)
)
fn_ids    <- map_chr(damage_fns, "id")
fn_labels <- map_chr(damage_fns, "label")

# ── Pre-draw MC samples (once, shared across all functions) ────
set.seed(42L)
mc_alpha_raw  <- rnorm(N_MC, ALPHA_RAW, SE_ALPHA)
mc_beta_cat   <- rnorm(N_MC, BETA_CAT,  SE_BETA_CAT)
mc_beta_grow  <- rnorm(N_MC, BETA_GROW, SE_BETA_GROW)

# ── MC NPV delta helper ────────────────────────────────────────
# Returns N_MC NPV-change values (%).
# mc_a: N_MC composite T^1.5 coefficient draws
# mc_b: N_MC composite T coefficient draws, or NULL
mc_npv_delta <- function(T_s, T_m, gdp_s, gdp_m, mc_a, mc_b = NULL) {
  dam_s <- outer(T_s^1.5, mc_a)
  dam_m <- outer(T_m^1.5, mc_a)
  if (!is.null(mc_b)) {
    dam_s <- dam_s + outer(T_s, mc_b)
    dam_m <- dam_m + outer(T_m, mc_b)
  }
  gmc_s    <- sweep(1 - dam_s / 100, 1, gdp_s, `*`)
  gmc_m    <- sweep(1 - dam_m / 100, 1, gdp_m, `*`)
  meet_npv <- colSums(sweep(gmc_m, 1, disc_vec, `*`))
  colSums(sweep(gmc_s - gmc_m, 1, disc_vec, `*`)) / meet_npv * 100
}

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
    "  scenarios=json.loads(r'''", toJSON(all_scenarios, auto_unbox = FALSE), "'''),\n",
    "  regions=json.loads(r'''",  toJSON(regions_vec, auto_unbox = FALSE), "'''),\n",
    "  prefix='", prefix, "',\n",
    "  outdir=r'''", normalizePath(direct_dir, winslash = "/", mustWork = FALSE), "'''\n)\n"
  )
  tmp <- tempfile(fileext = ".py")
  writeLines(py_code, tmp)
  status <- system2(python_exe, tmp)
  if (status != 0) { message("Retrying..."); status <- system2(python_exe, tmp) }
  if (status != 0) warning("Download failed for '", prefix, "'.")
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

# ── World GDP (MER preferred, PPP fallback) ────────────────────
run_python_download("GDP|MER", "gdp_mer")
run_python_download("GDP|PPP", "gdp_ppp")

gdp_mer_raw <- tryCatch(
  load_pq("gdp_mer") %>%
    filter(variable == "GDP|MER", region == "World", scenario %in% all_scenarios),
  error = function(e) NULL
)
gdp_ppp_raw <- tryCatch(
  load_pq("gdp_ppp") %>%
    filter(variable == "GDP|PPP", region == "World", scenario %in% all_scenarios),
  error = function(e) NULL
)
models_mer <- if (!is.null(gdp_mer_raw)) unique(as.character(gdp_mer_raw$model)) else character(0)

gdp_raw <- bind_rows(
  gdp_mer_raw,
  if (!is.null(gdp_ppp_raw)) filter(gdp_ppp_raw, !model %in% models_mer) else NULL
) %>% filter(!is.na(value))
gdp_raw <- fix_gem_e3_years(gdp_raw)

# ── GSAT (p50 and p67, reuse climate_assess_full parquet) ─────
ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run fig2.R first to generate it.")

gsat_raw <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)]) %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         region == "World", scenario %in% all_scenarios,
         year >= 2020, year <= 2100) %>%
  mutate(pct_col = case_when(
    grepl("Median|50th", variable, ignore.case = TRUE) ~ "p50",
    grepl("67th",        variable, ignore.case = TRUE) ~ "p67",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value) %>%
  filter(!is.na(p50), !is.na(p67))

# ══════════════════════════════════════════════════════════════
# 2. MODEL SELECTION — endogenous GDP filter (matches fig3.R)
#
# A model must pass THREE gates:
#   (a) Has GSAT p50+p67 for the scenarios needed
#   (b) Has World GDP for the scenarios needed
#   (c) Its World GDP actually CHANGES across scenarios vs Meet
#       (endogenous GDP). Models that report identical GDP in all
#       scenarios are using an exogenous SSP2 baseline — pairing a
#       scenario-varying temperature with a flat GDP would be
#       meaningless, so they are excluded.
#
# Gate (c) threshold: at least one non-Meet scenario/year must
# differ from Meet GDP by > ENDOGENOUS_THRESHOLD (0.5%).
# IMACLIM 2.0 is missing Staying Alive; it survives if it passes
# on the scenarios it did submit (inner_join below handles this).
# ══════════════════════════════════════════════════════════════

models_gsat <- gsat_raw %>%
  filter(!is.na(p50), !is.na(p67)) %>%
  pull(model) %>% as.character() %>% unique()
models_gsat <- models_gsat[models_gsat != "WITCH 6.0-EU28"]
models_gsat <- filter_latest_remind(sort(models_gsat))

# (a+b): intersection of GSAT and GDP model sets
candidates <- filter_latest_remind(
  sort(intersect(models_gsat, unique(as.character(gdp_raw$model)))))

# (b): must have Meet Aspirations in GDP
has_meet_gdp <- function(m)
  any(gdp_raw$model == m & gdp_raw$scenario == ma_scen & !is.na(gdp_raw$value))
candidates <- candidates[sapply(candidates, has_meet_gdp)]

# (c): endogenous GDP — World level
meet_w <- gdp_raw %>%
  filter(model %in% candidates, scenario == ma_scen, !is.na(value)) %>%
  select(model, year, meet_val = value)

endo_w <- gdp_raw %>%
  filter(model %in% candidates, scenario != ma_scen, !is.na(value)) %>%
  inner_join(meet_w, by = c("model", "year")) %>%
  mutate(pct_diff = abs(value - meet_val) / meet_val) %>%
  filter(is.finite(pct_diff), pct_diff > ENDOGENOUS_THRESHOLD) %>%
  pull(model) %>% as.character() %>% unique()

use_models <- filter_latest_remind(sort(intersect(candidates, endo_w)))

message("Candidates with GSAT+GDP: ", paste(candidates, collapse = ", "))
message("After endogenous GDP filter: ", paste(use_models, collapse = ", "))

gsat_all <- gsat_raw %>% filter(model %in% use_models)
gdp_all  <- gdp_raw  %>% filter(model %in% use_models)

# ══════════════════════════════════════════════════════════════
# 3. COMPUTE NPV CHANGE vs DAMAGE-ADJUSTED MEET  (panel b logic)
#
# For each (model, comparison scenario, damage fn):
#   Interpolate GDP and GSAT to annual grid (2020:2100).
#   Fixed-coefficient NPV: GDP(t)×(1 − D(T(t))/100), discounted.
#   NPV_change = (NPV_adj_scen − NPV_adj_meet) / NPV_adj_meet × 100
#   For fn2–fn7: MC draws over regression coefficients → q10/q50/q90.
#   For fn1/fn8/fn9: fixed only → q10=q50=q90=fixed estimate.
# ══════════════════════════════════════════════════════════════

ann_grid <- seq(BASE_YEAR, 2100L)
disc_vec <- 1 / (1 + DEFAULT_DISC)^(ann_grid - BASE_YEAR)

interp <- function(years, vals, xout = ann_grid)
  approx(years, vals, xout = xout, rule = 2)$y

results_list <- list()

for (mod in use_models) {
  message("Computing: ", mod)
  gm  <- gsat_all %>% filter(model == mod, scenario == ma_scen) %>% arrange(year)
  gdm <- gdp_all  %>% filter(model == mod, scenario == ma_scen) %>% arrange(year)
  if (nrow(gm) < 2 || nrow(gdm) < 2) next

  Tm50 <- pmax(interp(gm$year, gm$p50), 0)
  Tm67 <- pmax(interp(gm$year, gm$p67), 0)
  GDPm <- interp(gdm$year, gdm$value)

  for (scen in scens_nonmeet) {
    gs  <- gsat_all %>% filter(model == mod, scenario == scen) %>% arrange(year)
    gds <- gdp_all  %>% filter(model == mod, scenario == scen) %>% arrange(year)
    if (nrow(gs) < 2 || nrow(gds) < 2) next

    Ts50 <- pmax(interp(gs$year, gs$p50), 0)
    Ts67 <- pmax(interp(gs$year, gs$p67), 0)
    GDPs <- interp(gds$year, gds$value)

    for (spec in damage_fns) {
      fn <- spec$fn

      # Fixed-coefficient point estimates
      NPVs50 <- sum(GDPs * (1 - fn(Ts50) / 100) * disc_vec)
      NPVm50 <- sum(GDPm * (1 - fn(Tm50) / 100) * disc_vec)
      fix_v1 <- (NPVs50 - NPVm50) / NPVm50 * 100

      NPVs67 <- sum(GDPs * (1 - fn(Ts67) / 100) * disc_vec)
      NPVm67 <- sum(GDPm * (1 - fn(Tm67) / 100) * disc_vec)
      fix_v3 <- (NPVs67 - NPVm67) / NPVm67 * 100

      if (spec$has_mc) {
        # Composite MC coefficient draws for this function
        mc_a  <- mc_alpha_raw + spec$w_cat * mc_beta_cat
        mc_b  <- if (spec$w_grow != 0) spec$w_grow * mc_beta_grow else NULL

        mc_v1 <- mc_npv_delta(Ts50, Tm50, GDPs, GDPm, mc_a, mc_b)
        mc_v3 <- mc_npv_delta(Ts67, Tm67, GDPs, GDPm, mc_a, mc_b)

        q10_v1 <- quantile(mc_v1, 0.10, names = FALSE)
        q50_v1 <- quantile(mc_v1, 0.50, names = FALSE)
        q90_v1 <- quantile(mc_v1, 0.90, names = FALSE)
        q10_v3 <- quantile(mc_v3, 0.10, names = FALSE)
        q50_v3 <- quantile(mc_v3, 0.50, names = FALSE)
        q90_v3 <- quantile(mc_v3, 0.90, names = FALSE)
      } else {
        # No SE available — point estimate only
        q10_v1 <- q50_v1 <- q90_v1 <- fix_v1
        q10_v3 <- q50_v3 <- q90_v3 <- fix_v3
      }

      results_list[[length(results_list) + 1]] <- tibble(
        model       = mod,
        model_short = short_name(mod),
        scenario    = scen,
        fn_id       = spec$id,
        fn_lbl      = spec$label,
        has_mc      = spec$has_mc,
        q10_v1      = q10_v1, q50_v1 = q50_v1, q90_v1 = q90_v1,
        q10_v3      = q10_v3, q50_v3 = q50_v3, q90_v3 = q90_v3
      )
    }
  }
}

results <- bind_rows(results_list) %>%
  mutate(
    scenario_lbl = factor(scen_labels[scenario], levels = unname(scen_labels[scens_nonmeet])),
    fn_f         = factor(fn_lbl, levels = fn_labels),
    model_short  = factor(model_short, levels = sapply(use_models, short_name))
  )

message(sprintf("Results: %d rows (%d models \u00d7 %d scenarios \u00d7 %d fns)",
                nrow(results),
                n_distinct(results$model),
                n_distinct(results$scenario),
                n_distinct(results$fn_id)))

# ══════════════════════════════════════════════════════════════
# 4. AESTHETICS  (match fig3.R)
# ══════════════════════════════════════════════════════════════

mod_short_levels <- levels(results$model_short)
n_mods           <- length(mod_short_levels)
model_colors     <- setNames(
  scales::hue_pal(l = 55, c = 100)(n_mods),
  mod_short_levels
)

spread  <- min(0.28, n_mods * 0.07)
offsets <- setNames(
  seq(-spread, spread, length.out = max(n_mods, 2)),
  mod_short_levels
)

x_v1 <- 1L   # T₅₀
x_v3 <- 2L   # T₆₇

plot_df <- results %>%
  mutate(
    x_v1   = x_v1 + offsets[as.character(model_short)],
    x_v3   = x_v3 + offsets[as.character(model_short)],
    lbl_v1 = sprintf("%+.1f%%", q50_v1),
    lbl_v3 = sprintf("%+.1f%%", q50_v3)
  )

# Split: functions with vs without MC uncertainty
plot_mc    <- filter(plot_df, has_mc)
plot_no_mc <- filter(plot_df, !has_mc)

# ══════════════════════════════════════════════════════════════
# 5. THEME
# ══════════════════════════════════════════════════════════════

theme_set(
  theme_bw(base_size = 16, base_family = "Arial") +
    theme(
      plot.title         = element_text(size = 18, face = "bold"),
      axis.title         = element_text(size = 15),
      axis.text.y        = element_text(size = 14),
      axis.text.x        = element_text(size = 15, face = "bold"),
      strip.background   = element_blank(),
      strip.text.x       = element_text(size = 13.5, face = "bold", lineheight = 1.25),
      strip.text.y       = element_text(size = 15,   face = "bold", angle = 0),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(size = 14.5),
      legend.text        = element_text(size = 13.5)
    )
)

# ══════════════════════════════════════════════════════════════
# 6. PLOT
# ══════════════════════════════════════════════════════════════

p <- ggplot() +
  # Background rectangles (same as fig3 panel b)
  annotate("rect", xmin = 0.45, xmax = 1.55, ymin = -Inf, ymax = Inf,
           fill = "#FFF9C4", alpha = 0.65) +
  annotate("rect", xmin = 1.55, xmax = 2.55, ymin = -Inf, ymax = Inf,
           fill = "#FFB6C1", alpha = 0.55) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +

  # T₅₀ column — error bars for MC functions (fn2–fn7)
  geom_errorbar(data = plot_mc,
                aes(x = x_v1, ymin = q10_v1, ymax = q90_v1, colour = model_short),
                width = 0.05, linewidth = 0.8) +
  geom_point(data = plot_mc,
             aes(x = x_v1, y = q50_v1, colour = model_short),
             shape = 16, size = 3.2) +
  geom_text(data = plot_mc,
            aes(x = x_v1, y = q50_v1, label = lbl_v1, colour = model_short),
            hjust = 1.30, size = 5.5, show.legend = FALSE) +

  # T₅₀ column — point only for fixed functions (fn1/fn8/fn9)
  geom_point(data = plot_no_mc,
             aes(x = x_v1, y = q50_v1, colour = model_short),
             shape = 16, size = 3.2) +
  geom_text(data = plot_no_mc,
            aes(x = x_v1, y = q50_v1, label = lbl_v1, colour = model_short),
            hjust = 1.30, size = 5.5, show.legend = FALSE) +

  # T₆₇ column — error bars for MC functions
  geom_errorbar(data = plot_mc,
                aes(x = x_v3, ymin = q10_v3, ymax = q90_v3, colour = model_short),
                width = 0.05, linewidth = 0.8) +
  geom_point(data = plot_mc,
             aes(x = x_v3, y = q50_v3, colour = model_short),
             shape = 16, size = 3.2) +
  geom_text(data = plot_mc,
            aes(x = x_v3, y = q50_v3, label = lbl_v3, colour = model_short),
            hjust = -0.30, size = 5.5, show.legend = FALSE) +

  # T₆₇ column — point only for fixed functions
  geom_point(data = plot_no_mc,
             aes(x = x_v3, y = q50_v3, colour = model_short),
             shape = 16, size = 3.2) +
  geom_text(data = plot_no_mc,
            aes(x = x_v3, y = q50_v3, label = lbl_v3, colour = model_short),
            hjust = -0.30, size = 5.5, show.legend = FALSE) +

  facet_grid(scenario_lbl ~ fn_f, scales = "fixed", switch = "y") +
  scale_colour_manual(values = model_colors, name = NULL) +
  scale_x_continuous(
    breaks = c(x_v1, x_v3),
    labels = c("T\u2085\u2080\n(median)", "T\u2086\u2087\n(67th pct)"),
    expand = expansion(add = 0.55)
  ) +
  labs(
    title = "SM7: Damage function sensitivity - GDP NPV vs damage-adjusted Meet Aspirations",
    x     = NULL,
    y     = "NPV vs Meet Aspirations (% of damage-adjusted Meet NPV)"
  ) +
  guides(colour = guide_legend(
    nrow = 2, byrow = TRUE,
    override.aes = list(size = 3)
  ))

# ══════════════════════════════════════════════════════════════
# 7. SAVE
# ══════════════════════════════════════════════════════════════

out_file <- file.path(graphs_dir, "fig_sm4.png")
ggsave(out_file, p, width = 22, height = 9, dpi = 300)
message("Saved: ", out_file)
