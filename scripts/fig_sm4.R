library(tidyverse)
library(arrow)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — supplementary figure SM4
#   GDP NPV sensitivity to discount rate (T₅₀ median GSAT).
#   Continuous x-axis 1 %–10 % (0.5 % steps) with zero-crossing
#   markers and "all models positive" breakpoint per scenario.
#   3 panels (columns), one per non-Meet scenario.
#
# Output: graphs/fig_sm4.png
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

DISC_RATES_FINE <- seq(0.01, 0.10, by = 0.005)   # 19 points

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
# 1. LOAD DATA  (same parquets as fig2.R / fig3.R)
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

# Fallback: aggregate world total from R10 regions if world-level parquet is empty
if (nrow(gdp_all) == 0) {
  r10_regions_sm4 <- c(
    "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
    "Latin America (R10)", "Middle East (R10)", "North America (R10)",
    "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
    "Rest of Asia (R10)"
  )
  load_gdp_r10 <- function(prefix, varname) {
    tryCatch(
      load_pq(prefix) %>%
        filter(variable == varname, region %in% r10_regions_sm4,
               scenario %in% scenarios),
      error = function(e) NULL
    )
  }
  gdp_mer_r10_raw <- load_gdp_r10("gdp_mer_r10", "GDP|MER")
  gdp_ppp_r10_raw <- load_gdp_r10("gdp_ppp_r10", "GDP|PPP")
  models_mer_r10  <- if (!is.null(gdp_mer_r10_raw))
    unique(as.character(gdp_mer_r10_raw$model)) else character(0)
  gdp_r10_raw <- bind_rows(
    gdp_mer_r10_raw,
    if (!is.null(gdp_ppp_r10_raw)) filter(gdp_ppp_r10_raw, !model %in% models_mer_r10) else NULL
  ) %>% filter(!is.na(value))
  gdp_r10_raw <- fix_gem_e3_years(gdp_r10_raw)
  if (nrow(gdp_r10_raw) > 0) {
    message("World GDP parquet empty \u2014 aggregating world total from R10 regions.")
    gdp_all <- gdp_r10_raw %>%
      group_by(model, scenario, variable, year) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(region = "World")
  }
}

ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run fig2.R first.")
web_raw <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)])

# ══════════════════════════════════════════════════════════════
# 2. MODEL SELECTION  (T67 / p67 only)
# ══════════════════════════════════════════════════════════════
models_gsat <- web_raw %>%
  filter(grepl("Surface Temperature.*GSAT", variable, ignore.case = TRUE),
         region == "World", scenario %in% scenarios) %>%
  mutate(pct_col = case_when(
    grepl("67th", variable, ignore.case = TRUE) ~ "p67",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value) %>%
  filter(!is.na(p67)) %>%
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
    grepl("67th", variable, ignore.case = TRUE) ~ "p67",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(pct_col)) %>%
  select(model, scenario, year, pct_col, value) %>%
  pivot_wider(names_from = pct_col, values_from = value)

# ══════════════════════════════════════════════════════════════
# 3. PRE-COMPUTE DAMAGE MATRICES  (once per model × scenario)
#    gmc_s / gmc_m don't depend on the discount rate — only
#    disc_v changes, giving ~18× speedup over the per-rate loop.
# ══════════════════════════════════════════════════════════════
set.seed(42)
mc_alpha_draws <- rnorm(N_MC, ALPHA_C, SE_ALPHA_C)
mc_beta_draws  <- rnorm(N_MC, BETA_C,  SE_BETA_C)

ann_y_full <- seq(BASE_YEAR, 2100L)

message("Pre-computing damage matrices ...")
precomp <- list()

for (mod in use_models) {
  gdp_raw  <- gdp_all      %>% filter(model == mod)
  web_gsat <- web_gsat_all %>% filter(model == mod)
  if (nrow(gdp_raw) == 0 || nrow(web_gsat) == 0) next
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
    select(year, meet_raw = value, meet_p67 = p67)
  if (nrow(meet_ref) < 2) next

  # Meet Aspirations damage matrix (T67)
  gdp_m <- approx(meet_ref$year, meet_ref$meet_raw, xout = ann_y_full, rule = 2)$y
  Tm67  <- pmax(approx(meet_ref$year, meet_ref$meet_p67, xout = ann_y_full, rule = 2)$y, 0)
  gmc_m <- sweep(1 - (outer(Tm67^1.5, mc_alpha_draws) + outer(Tm67, mc_beta_draws)) / 100,
                 1, gdp_m, `*`)

  nonmeet_jn <- joined %>%
    filter(scenario %in% scens_nonmeet) %>%
    left_join(meet_ref, by = "year")

  for (scen in scens_nonmeet) {
    d <- nonmeet_jn %>%
      filter(as.character(scenario) == scen,
             !is.na(p67), !is.na(meet_p67)) %>%
      arrange(year)
    if (nrow(d) == 0) next
    gdp_s <- approx(d$year, d$value, xout = ann_y_full, rule = 2)$y
    Ts67  <- pmax(approx(d$year, d$p67, xout = ann_y_full, rule = 2)$y, 0)
    gmc_s <- sweep(1 - (outer(Ts67^1.5, mc_alpha_draws) + outer(Ts67, mc_beta_draws)) / 100,
                   1, gdp_s, `*`)

    key <- paste(mod, scen, sep = "\x01")
    precomp[[key]] <- list(mod = mod, scen = scen, gmc_s = gmc_s, gmc_m = gmc_m)
  }
}

if (length(precomp) == 0)
  stop("precomp is empty — check that GDP and GSAT data loaded correctly.")

# ══════════════════════════════════════════════════════════════
# 4. COMPUTE NPV OVER FINE DISCOUNT RATE GRID
# ══════════════════════════════════════════════════════════════
message("Computing NPV over ", length(DISC_RATES_FINE), " discount rates ...")

results_dr <- bind_rows(lapply(DISC_RATES_FINE, function(dr) {
  disc_v <- 1 / (1 + dr)^(ann_y_full - BASE_YEAR)
  bind_rows(lapply(precomp, function(pc) {
    meet_dam <- colSums(sweep(pc$gmc_m, 1, disc_v, `*`))
    mc_v1    <- colSums(sweep(pc$gmc_s - pc$gmc_m, 1, disc_v, `*`)) / meet_dam * 100
    tibble(
      model       = pc$mod,
      model_short = short_name(pc$mod),
      scenario    = pc$scen,
      disc_rate   = dr,
      q10_pct     = quantile(mc_v1, 0.10),
      q50_pct     = quantile(mc_v1, 0.50),
      q90_pct     = quantile(mc_v1, 0.90)
    )
  }))
}))

mod_short_levels <- sapply(use_models, short_name)
results_dr <- results_dr %>%
  mutate(
    scenario_label = factor(
      scen_labels[match(scenario, names(scen_labels))],
      levels = unname(scen_labels)
    ),
    model_short = factor(model_short, levels = mod_short_levels)
  )

# ══════════════════════════════════════════════════════════════
# 5. FIND ZERO-CROSSINGS  (interpolated rate where median → 0)
# ══════════════════════════════════════════════════════════════
find_cross <- function(disc_rate, q50) {
  if (!is.na(q50[1]) && q50[1] > 0) return(tibble(disc_cross = 0))
  i <- which(q50[-length(q50)] < 0 & q50[-1] > 0)
  if (length(i) == 0) return(tibble(disc_cross = NA_real_))
  j  <- i[1]
  x1 <- disc_rate[j];      y1 <- q50[j]
  x2 <- disc_rate[j + 1L]; y2 <- q50[j + 1L]
  tibble(disc_cross = x1 + (0 - y1) * (x2 - x1) / (y2 - y1))
}

crossing_pts <- results_dr %>%
  group_by(model_short, scenario, scenario_label) %>%
  arrange(disc_rate) %>%
  group_modify(~ find_cross(.x$disc_rate, .x$q50_pct)) %>%
  ungroup()

n_mods <- n_distinct(results_dr$model_short)

all_pos_df <- crossing_pts %>%
  group_by(scenario, scenario_label) %>%
  summarise(
    all_pos_rate = if (sum(!is.na(disc_cross)) == n_mods)
                     max(disc_cross, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(!is.na(all_pos_rate)) %>%
  mutate(lbl = sprintf("%.1f%%", all_pos_rate * 100))

message("\nBreakpoints (% discount rate where model median NPV = 0):")
print(
  crossing_pts %>%
    mutate(disc_cross_pct = round(disc_cross * 100, 1)) %>%
    select(model_short, scenario_label, disc_cross_pct) %>%
    arrange(scenario_label, model_short),
  n = Inf
)
if (nrow(all_pos_df) > 0) {
  message("\nAll-models-positive rate:")
  print(all_pos_df %>% mutate(pct = round(all_pos_rate * 100, 1)) %>%
          select(scenario_label, pct))
}

# ══════════════════════════════════════════════════════════════
# 6. PLOT
# ══════════════════════════════════════════════════════════════
model_colors_sm <- setNames(
  scales::hue_pal(l = 55, c = 100)(n_mods), mod_short_levels
)

cross_in_range <- crossing_pts %>%
  filter(!is.na(disc_cross), disc_cross >= min(DISC_RATES_FINE))

p <- ggplot(results_dr,
            aes(x = disc_rate, y = q50_pct,
                colour = model_short, group = model_short)) +
  # "All positive" breakpoint vertical line per scenario
  geom_vline(data = all_pos_df,
             aes(xintercept = all_pos_rate),
             inherit.aes = FALSE,
             linewidth = 0.8, colour = "grey25", linetype = "solid") +
  # Zero line
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey55") +
  # Default-rate reference (2 %)
  geom_vline(xintercept = 0.02, linetype = "dotted",
             linewidth = 0.5, colour = "grey65") +
  # MC uncertainty band (Q10–Q90)
  geom_ribbon(aes(ymin = q10_pct, ymax = q90_pct, fill = model_short),
              alpha = 0.07, colour = NA) +
  # Median lines
  geom_line(linewidth = 0.9) +
  # Zero-crossing markers (✕ at y = 0)
  geom_point(data = cross_in_range,
             aes(x = disc_cross, y = 0, colour = model_short),
             shape = 4, size = 3.5, stroke = 1.5, inherit.aes = FALSE) +
  # "All positive" rate label at top of panel
  geom_text(data = all_pos_df,
            aes(x = all_pos_rate, y = Inf, label = lbl),
            inherit.aes = FALSE,
            hjust = -0.12, vjust = 1.6, size = 3.8,
            fontface = "bold", colour = "grey25") +
  # "default (2 %)" label
  annotate("text", x = 0.02, y = Inf,
           label = "default\n(2%)", vjust = 1.6, hjust = -0.1,
           size = 3.2, colour = "grey55", lineheight = 0.9) +
  facet_wrap(~ scenario_label, nrow = 1, scales = "fixed") +
  scale_colour_manual(values = model_colors_sm, name = NULL) +
  scale_fill_manual(  values = model_colors_sm, name = NULL, guide = "none") +
  scale_x_continuous(
    breaks = seq(0.01, 0.10, by = 0.01),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.06))
  ) +
  labs(
    title   = "GDP NPV: discount rate sensitivity 1\u201310 % (T\u2086\u2087 GSAT, 67th percentile)",
    x       = "Discount rate",
    y       = "NPV vs Meet Aspirations (% of damage-adjusted Meet NPV)"
  ) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 1.5))) +
  theme(legend.position  = "right",
        axis.text.x      = element_text(size = 11.5, angle = 45, hjust = 1),
        axis.title.y     = element_text(size = 11.0))

out_file <- file.path(graphs_dir, "fig_sm4.png")
ggsave(out_file, p, width = 16, height = 6.5, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig_sm4", graphs_dir)
  }
})
