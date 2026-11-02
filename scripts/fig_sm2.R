library(tidyverse)
library(arrow)
library(patchwork)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 supplementary figure SM2
#
#   (a) CO₂ emissions by PRISMA cluster (median + min/max band)
#   (b) CH₄ emissions by PRISMA cluster (median + min/max band)
#
#   Both panels use R10 regions summed within cluster.
#   Free y-axis scale per cluster (magnitudes differ).
#
# Output: graphs/fig_sm2.png
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

# ── R10 regions & cluster map ──────────────────────────────────
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
all_regions   <- c("World", r10_regions)

all_years <- c(seq(2020, 2050, by = 5), seq(2060, 2100, by = 10))

# ── Theme ──────────────────────────────────────────────────────
theme_set(
  theme_bw(base_size = 20, base_family = "Arial") +
    theme(
      plot.title       = element_text(size = 20.0, face = "bold", hjust = 0.5),
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

# ── Python / parquet helpers ───────────────────────────────────
miniconda_envs        <- file.path(Sys.getenv("USERPROFILE"), "miniconda3", "envs")
env_dirs              <- list.dirs(miniconda_envs, recursive = FALSE)
python_exe_candidates <- Filter(file.exists, file.path(env_dirs, "python.exe"))
if (length(python_exe_candidates) == 0) stop("No python.exe found under ", miniconda_envs)
python_exe <- python_exe_candidates[1]

run_python_download <- function(variable, prefix, regions_vec = all_regions) {
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

# ── Shared helpers ─────────────────────────────────────────────
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

make_cluster_band <- function(raw, models_ok, value_scale = 1) {
  raw %>%
    filter(model %in% models_ok, region %in% r10_regions) %>%
    mutate(
      value    = value * value_scale,
      scenario = unname(scenario_labels[as.character(scenario)]),
      cluster  = cluster_map[region]
    ) %>%
    filter(!is.na(cluster)) %>%
    group_by(model, scenario, cluster, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    interp_band(c("model", "scenario", "cluster")) %>%
    mutate(value_b = if_else(.rep, value, NA_real_)) %>%
    group_by(scenario, cluster, year) %>%
    summarise(
      med = median(value_b, na.rm = TRUE),
      lo  = min(value_b,    na.rm = TRUE),
      hi  = max(value_b,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      scenario = factor(scenario, levels = scen_lbl_levels),
      cluster  = factor(cluster,  levels = cluster_order)
    )
}

# ══════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════

# ── CO₂ (reuses shared parquet, downloaded with R10 regions) ──
run_python_download("Emissions|CO2", "post_co2_total", all_regions)

co2_raw <- load_pq("post_co2_total") %>%
  filter(variable == "Emissions|CO2", scenario %in% scenarios, year %in% all_years)
co2_raw <- fix_gem_e3_years(co2_raw)

m_all_co2 <- sort(unique(as.character(co2_raw$model)))
m_all_co2 <- m_all_co2[m_all_co2 != "WITCH 6.0-EU28"]
m_all_co2 <- filter_latest_remind(m_all_co2)

m_ok_co2 <- co2_raw %>%
  filter(model %in% m_all_co2, region == "World", !is.na(value)) %>%
  distinct(model, scenario) %>%
  group_by(model) %>%
  summarise(has_meet = any(scenario == ma_scen),
            n_scen   = n_distinct(scenario), .groups = "drop") %>%
  filter(has_meet, n_scen >= min(3L, length(scenarios))) %>%
  pull(model)

# Only keep models with R10 data for cluster panel
m_ok_co2 <- intersect(m_ok_co2,
  co2_raw %>% filter(region %in% r10_regions, !is.na(value)) %>%
    pull(model) %>% as.character() %>% unique())
message("CO\u2082 models (cluster): ", paste(sort(m_ok_co2), collapse = ", "))

# ── CH₄ (World + R10) ─────────────────────────────────────────
run_python_download("Emissions|CH4", "sm2_ch4_r10", all_regions)

ch4_raw <- load_pq("sm2_ch4_r10") %>%
  filter(variable == "Emissions|CH4", scenario %in% scenarios, year %in% all_years)
ch4_raw <- fix_gem_e3_years(ch4_raw)

m_all_ch4 <- sort(unique(as.character(ch4_raw$model)))
m_all_ch4 <- m_all_ch4[m_all_ch4 != "WITCH 6.0-EU28"]
m_all_ch4 <- filter_latest_remind(m_all_ch4)

m_ok_ch4 <- ch4_raw %>%
  filter(model %in% m_all_ch4, region == "World", !is.na(value)) %>%
  distinct(model, scenario) %>%
  group_by(model) %>%
  summarise(has_meet = any(scenario == ma_scen),
            n_scen   = n_distinct(scenario), .groups = "drop") %>%
  filter(has_meet, n_scen >= min(3L, length(scenarios))) %>%
  pull(model)

m_ok_ch4 <- intersect(m_ok_ch4,
  ch4_raw %>% filter(region %in% r10_regions, !is.na(value)) %>%
    pull(model) %>% as.character() %>% unique())
message("CH\u2084 models (cluster): ", paste(sort(m_ok_ch4), collapse = ", "))

# ══════════════════════════════════════════════════════════════
# 2. AGGREGATE TO CLUSTER
# ══════════════════════════════════════════════════════════════

# CO₂: convert Mt → Gt (divide by 1000)
co2_band <- make_cluster_band(co2_raw, m_ok_co2, value_scale = 1 / 1000)

# CH₄: keep as Mt CH₄/yr (no conversion needed)
ch4_band <- make_cluster_band(ch4_raw, m_ok_ch4, value_scale = 1)

# ══════════════════════════════════════════════════════════════
# 3. BUILD PANELS
# ══════════════════════════════════════════════════════════════

x_breaks <- c(2020, seq(2040, 2100, by = 20))

legend_guide <- guides(
  colour = guide_legend(ncol = 4, override.aes = list(fill = NA)),
  fill   = "none"
)

# ── Panel (a): CO₂ by cluster ─────────────────────────────────
p_a <- ggplot(co2_band,
              aes(x = year, colour = scenario, fill = scenario, group = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(aes(y = med), linewidth = 0.9) +
  facet_wrap(~ cluster, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = scen_col_lbl, name = NULL) +
  scale_fill_manual(  values = scen_col_lbl, name = NULL) +
  scale_x_continuous(breaks = x_breaks) +
  labs(title = "(a) CO\u2082 emissions by regional cluster",
       x = NULL, y = "Gt CO\u2082/yr") +
  legend_guide +
  theme(legend.position = "none", aspect.ratio = 1)

# ── Panel (b): CH₄ by cluster ─────────────────────────────────
p_b <- ggplot(ch4_band,
              aes(x = year, colour = scenario, fill = scenario, group = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(aes(y = med), linewidth = 0.9) +
  facet_wrap(~ cluster, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = scen_col_lbl, name = NULL) +
  scale_fill_manual(  values = scen_col_lbl, name = NULL) +
  scale_x_continuous(breaks = x_breaks) +
  labs(title = "(b) CH\u2084 emissions by regional cluster",
       x = NULL, y = "Mt CH\u2084/yr") +
  legend_guide +
  theme(legend.position = "bottom", aspect.ratio = 1)

# ══════════════════════════════════════════════════════════════
# 4. COMBINE AND SAVE
# ══════════════════════════════════════════════════════════════
fig <- p_a / p_b +
  plot_layout(heights = c(1, 1))

out_file <- file.path(graphs_dir, "fig_sm2.png")
ggsave(out_file, fig, width = 18, height = 14, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig_sm2", graphs_dir)
  }
})
