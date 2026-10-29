library(tidyverse)
library(arrow)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 supplementary figure SM5
#   GDP timeseries vs Meet Aspirations, disaggregated by
#   PRISMA cluster (columns) × non-Meet scenario (rows).
#
#   Same format as panel (a) of fig2.R:
#     Solid lines  = undamaged GDP (% diff from cluster Meet)
#     Dashed lines = damage-adjusted GDP (global GSAT T50, H&S fn)
#     Coloured     = per model
#     Bold black   = cross-model mean
#
# Output: graphs/fig_sm5.png
# ══════════════════════════════════════════════════════════════

# ── Constants (same as fig2.R) ─────────────────────────────────
CAT_WEIGHT    <- 0.621
GROWTH_WEIGHT <- 0.602
ALPHA_C       <- 0.622 + 1.775 * CAT_WEIGHT
BETA_C        <- 1.997 * GROWTH_WEIGHT
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
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive",
  "SSP2 - Asymmetric Roll-Back"
)
scen_labels <- c(
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Staying Alive"        = "Tech Momentum",
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition"
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
  "Middle East (R10)"          = "Fossil-Dependent",
  "North America (R10)"        = "Diversifying Economies",
  "Other (R10)"                = "Diversifying Economies",
  "Pacific OECD (R10)"         = "Transition Leaders",
  "Reforming Economies (R10)"  = "Fossil-Dependent",
  "Rest of Asia (R10)"         = "Diversifying Economies"
)
cluster_order <- c("Transition Leaders", "Diversifying Economies", "Fossil-Dependent")

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
      axis.title         = element_text(size = 13.6),
      axis.text          = element_text(size = 12.4),
      axis.text.x        = element_text(angle = 45, hjust = 1),
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

# ══════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════

# ── R10 GDP (MER > PPP, same parquets as fig2.R) ──────────────
run_python_download("GDP|MER", "gdp_mer_r10", r10_regions)
run_python_download("GDP|PPP", "gdp_ppp_r10", r10_regions)

load_gdp_var <- function(prefix, varname, regions) {
  tryCatch(
    load_pq(prefix) %>%
      filter(variable == varname, region %in% regions, scenario %in% scenarios),
    error = function(e) NULL
  )
}
gdp_mer_r10_raw <- load_gdp_var("gdp_mer_r10", "GDP|MER", r10_regions)
gdp_ppp_r10_raw <- load_gdp_var("gdp_ppp_r10", "GDP|PPP", r10_regions)
models_mer_r10  <- if (!is.null(gdp_mer_r10_raw)) unique(as.character(gdp_mer_r10_raw$model)) else character(0)

gdp_r10_all <- bind_rows(
  gdp_mer_r10_raw,
  if (!is.null(gdp_ppp_r10_raw)) filter(gdp_ppp_r10_raw, !model %in% models_mer_r10) else NULL
) %>% filter(!is.na(value))
gdp_r10_all <- fix_gem_e3_years(gdp_r10_all)

# ── GSAT (same parquet as fig2.R / fig_sm1.R) ─────────────────
ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run fig2.R first.")
web_raw <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)])

# ══════════════════════════════════════════════════════════════
# 2. MODEL SELECTION  (mirrors fig2.R logic)
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

cands <- filter_latest_remind(
  sort(intersect(unique(as.character(gdp_r10_all$model)), models_gsat)))
cands <- cands[cands != "WITCH 6.0-EU28"]

meet_cl_sum <- gdp_r10_all %>%
  filter(model %in% cands, scenario == ma_scen, !is.na(value)) %>%
  group_by(model, year) %>%
  summarise(meet_val = sum(value, na.rm = TRUE), .groups = "drop")

endo_mods <- gdp_r10_all %>%
  filter(model %in% cands, scenario != ma_scen, !is.na(value)) %>%
  group_by(model, scenario, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  inner_join(meet_cl_sum, by = c("model", "year")) %>%
  mutate(pct_diff = abs(value - meet_val) / meet_val) %>%
  filter(is.finite(pct_diff), pct_diff > ENDOGENOUS_THRESHOLD) %>%
  pull(model) %>% as.character() %>% unique()

use_models <- filter_latest_remind(sort(intersect(cands, endo_mods)))
message("Models (cluster GDP + GSAT + endogenous): ", paste(use_models, collapse = ", "))

gdp_r10_all <- gdp_r10_all %>% filter(model %in% use_models)

gdp_cluster_all <- gdp_r10_all %>%
  mutate(cluster = cluster_map[region]) %>%
  filter(!is.na(cluster)) %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

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
  pivot_wider(names_from = pct_col, values_from = value) %>%
  filter(!is.na(p50))

# ══════════════════════════════════════════════════════════════
# 3. COMPUTE CLUSTER TIMESERIES
# ══════════════════════════════════════════════════════════════
ts_rows <- list()

for (mod in use_models) {
  gdp_cl   <- gdp_cluster_all %>% filter(model == mod) %>% fill_sparse_cl()
  web_gsat <- web_gsat_all    %>% filter(model == mod)
  if (nrow(gdp_cl) == 0 || nrow(web_gsat) == 0) {
    message("Skipping ", mod, " \u2014 missing data"); next
  }

  gsat_m <- web_gsat %>% filter(scenario == ma_scen) %>%
    select(year, p50_m = p50)

  for (cl in cluster_order) {
    meet_cl <- gdp_cl %>% filter(scenario == ma_scen, cluster == cl) %>%
      select(year, gdp_m = value)
    if (nrow(meet_cl) < 2) next

    for (scen in scens_nonmeet) {
      scen_cl <- gdp_cl %>% filter(scenario == scen, cluster == cl) %>%
        select(year, gdp_s = value)
      gsat_s  <- web_gsat %>% filter(scenario == scen) %>%
        select(year, p50_s = p50)
      if (nrow(scen_cl) < 2 || nrow(gsat_s) < 2) next

      jn <- scen_cl %>%
        inner_join(meet_cl, by = "year") %>%
        inner_join(gsat_s,  by = "year") %>%
        inner_join(gsat_m,  by = "year") %>%
        filter(year %in% gdp_years_std, gdp_m > 0) %>%
        mutate(
          gdp_s_dam = gdp_s * (1 - damage_fn(p50_s) / 100),
          gdp_m_dam = gdp_m * (1 - damage_fn(p50_m) / 100),
          pct_raw   = (gdp_s     - gdp_m)     / gdp_m     * 100,
          pct_dam   = (gdp_s_dam - gdp_m_dam) / gdp_m_dam * 100
        )
      if (nrow(jn) == 0) next

      ts_rows[[length(ts_rows) + 1]] <- jn %>%
        select(year, pct_raw, pct_dam) %>%
        mutate(model = mod, scenario = scen, cluster = cl)
    }
  }
}

if (length(ts_rows) == 0) stop("No cluster timeseries data computed.")

ts_cl <- bind_rows(ts_rows) %>%
  pivot_longer(c(pct_raw, pct_dam), names_to = "type", values_to = "pct") %>%
  mutate(
    line_type   = factor(
      if_else(type == "pct_raw", "Undamaged GDP", "Damage-adjusted GDP"),
      levels = c("Undamaged GDP", "Damage-adjusted GDP")
    ),
    model_short = short_name(model),
    scen_label  = factor(scen_labels[scenario], levels = unname(scen_labels)),
    cluster     = factor(cluster, levels = cluster_order)
  )

# ══════════════════════════════════════════════════════════════
# 4. PLOT
# ══════════════════════════════════════════════════════════════
mod_short_levels <- sort(unique(ts_cl$model_short))
n_mods           <- length(mod_short_levels)
model_colors     <- setNames(scales::hue_pal(l = 55, c = 100)(n_mods), mod_short_levels)

p <- ggplot(ts_cl,
            aes(x = year, y = pct,
                colour   = model_short,
                linetype = line_type,
                group    = interaction(model_short, type))) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.75, alpha = 0.85) +
  facet_grid(scen_label ~ cluster) +
  scale_colour_manual(values = model_colors, name = NULL) +
  scale_linetype_manual(
    values = c("Undamaged GDP" = "solid", "Damage-adjusted GDP" = "dashed"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = seq(2020, 2100, 20)) +
  labs(
    x = NULL,
    y = "Annual GDP vs Meet Aspirations (%)"
  ) +
  guides(
    colour   = guide_legend(ncol = 1, order = 1,
                            override.aes = list(linewidth = 1.5, linetype = "solid")),
    linetype = guide_legend(ncol = 1, order = 2)
  ) +
  theme(legend.position = "right")

n_rows <- length(scens_nonmeet)
n_cols <- length(cluster_order)
out_file <- file.path(graphs_dir, "fig_sm5.png")
ggsave(out_file, p,
       width  = 4.5 * n_cols + 2.5,
       height = 3.5 * n_rows + 2.0,
       dpi    = 300,
       limitsize = FALSE)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig_sm5", graphs_dir)
  }
})
