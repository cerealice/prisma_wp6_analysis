library(tidyverse)
library(arrow)
library(patchwork)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE  — post-Oct-15 figure 2  (2 panels)
#
#   (a) World CO₂ emissions        (median line + min/max ribbon)
#   (b) Temperature boxplots       (peak T and T@2100 per scenario;
#                                   whiskers Q10/Q90, box Q33/Q67,
#                                   statistics across models' T50)
#
# Output: graphs/fig2.png
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

scen_labels_lbl <- c(
  "SSP2 - Meet Aspirations"     = "Meet Aspirations",
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Staying Alive"        = "Tech Momentum",
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition"
)
scen_labels_wrap <- c(
  "SSP2 - Meet Aspirations"     = "Meet\nAspirations",
  "SSP2 - Late Reawakening"     = "Late\nReawakening",
  "SSP2 - Staying Alive"        = "Tech\nMomentum",
  "SSP2 - Asymmetric Roll-Back" = "Shifted\nAmbition"
)

scen_col_ssp2 <- c(
  "SSP2 - Meet Aspirations"     = "#5BB85C",
  "SSP2 - Late Reawakening"     = "#D62728",
  "SSP2 - Staying Alive"        = "#87CEEB",
  "SSP2 - Asymmetric Roll-Back" = "#999999"
)
scen_col_lbl <- setNames(unname(scen_col_ssp2), unname(scen_labels_lbl))
scen_lbl_levels <- unname(scen_labels_lbl[scen_plot_order])

# ── R10 regions ────────────────────────────────────────────────
r10_regions <- c(
  "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)"
)
all_regions <- c("World", r10_regions)

# ── Theme ──────────────────────────────────────────────────────
theme_set(
  theme_bw(base_size = 17, base_family = "Arial") +
    theme(
      plot.title       = element_text(size = 16.8, face = "bold", hjust = 0.5),
      axis.title       = element_text(size = 15.5),
      axis.text        = element_text(size = 14.3),
      strip.background = element_blank(),
      strip.text       = element_text(size = 15.5, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title     = element_text(size = 14.3),
      legend.text      = element_text(size = 13.6),
      plot.caption     = element_text(size = 12.5, colour = "black", hjust = 0)
    )
)

# ── Python / parquet helpers ───────────────────────────────────
MAGICC_TAG <- "[MAGICC v7.6.0a3]"
ca_vars <- c(
  paste0("Climate Assessment|Surface Temperature (GSAT)|33rd Percentile ", MAGICC_TAG),
  paste0("Climate Assessment|Surface Temperature (GSAT)|Median ",          MAGICC_TAG),
  paste0("Climate Assessment|Surface Temperature (GSAT)|67th Percentile ", MAGICC_TAG)
)

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
    files <- files[!grepl("_meta\.parquet$", files)]
    if (length(files) > 0)
      message("Using backup data for '", prefix, "' (no fresh download found).")
  }
  if (length(files) == 0) stop("No data found for '", prefix, "'. Run download manually.")
  read_parquet(files[which.max(file.info(files)$mtime)])
}

# ══════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════

all_years_co2 <- c(seq(2020, 2050, by = 5), seq(2060, 2100, by = 10))

# ── CO₂  (reuse post_co2_total parquet shared with post_oct13) ─
run_python_download("Emissions|CO2", "post_co2_total", all_regions)

co2_raw <- load_pq("post_co2_total") %>%
  filter(variable == "Emissions|CO2",
         scenario %in% scenarios,
         year     %in% all_years_co2)
co2_raw <- fix_gem_e3_years(co2_raw)

m_co2 <- sort(unique(as.character(co2_raw$model)))
m_co2 <- m_co2[m_co2 != "WITCH 6.0-EU28"]
m_co2 <- filter_latest_remind(m_co2)

m_co2_ok <- co2_raw %>%
  filter(model %in% m_co2, region == "World", !is.na(value)) %>%
  distinct(model, scenario) %>%
  group_by(model) %>%
  summarise(has_meet = any(scenario == ma_scen),
            n_scen   = n_distinct(scenario), .groups = "drop") %>%
  filter(has_meet, n_scen >= min(3L, length(scenarios))) %>%
  pull(model)
message("CO\u2082 models: ", paste(m_co2_ok, collapse = ", "))

# ── GSAT  (reuse climate_assess_full parquet) ─────────────────
run_python_download(ca_vars, "climate_assess_full")

ca_backup_dir <- file.path(project_dir, "data", "climate_assessment")
ca_files <- list.files(direct_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
ca_files <- ca_files[!grepl("_meta\\.parquet$", ca_files)]
if (length(ca_files) == 0)
  ca_files <- list.files(ca_backup_dir, "^climate_assess_full_.*\\.parquet$", full.names = TRUE)
if (length(ca_files) == 0)
  stop("climate_assess_full parquet not found. Run a GSAT script first.")

gsat <- read_parquet(ca_files[which.max(file.info(ca_files)$mtime)]) %>%
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
  filter(!is.na(p50))

mods_gsat <- filter_latest_remind(sort(unique(as.character(gsat$model))))
mods_gsat <- mods_gsat[mods_gsat != "WITCH 6.0-EU28"]
gsat <- gsat %>% filter(model %in% mods_gsat)
message("GSAT models (", length(mods_gsat), "): ", paste(mods_gsat, collapse = ", "))


# ══════════════════════════════════════════════════════════════
# 2. PANEL (a) — CO₂ world band
# ══════════════════════════════════════════════════════════════

interp_band <- function(df, group_vars) {
  if (nrow(df) == 0) return(df)
  df %>%
    group_by(across(all_of(group_vars))) %>%
    complete(year = all_years_co2) %>%
    arrange(year) %>%
    mutate(value = {
      ok <- !is.na(value)
      if (sum(ok) >= 2) approx(year[ok], value[ok], xout = year, rule = 1)$y
      else value
    }) %>%
    filter(!is.na(value)) %>%
    ungroup()
}

co2_world <- co2_raw %>%
  filter(model %in% m_co2_ok, region == "World") %>%
  mutate(
    value    = value / 1000,
    scenario = unname(scen_labels_lbl[as.character(scenario)])
  ) %>%
  select(model, scenario, year, value)

co2_band <- co2_world %>%
  interp_band(c("model", "scenario")) %>%
  group_by(scenario, year) %>%
  summarise(
    med = median(value, na.rm = TRUE),
    lo  = min(value,    na.rm = TRUE),
    hi  = max(value,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(scenario = factor(scenario, levels = scen_lbl_levels))

# ══════════════════════════════════════════════════════════════
# 3. PANEL (b) — Temperature boxplots
#    Use all three MAGICC percentiles (T33, T50, T67) from every
#    model, so the box statistics reflect both model spread and
#    MAGICC config uncertainty.
#    Box: Q33–Q67 across (model × percentile). Whiskers: Q10–Q90.
# ══════════════════════════════════════════════════════════════

# Pivot the three reported percentiles to long form
gsat_long <- gsat %>%
  pivot_longer(c(p33, p50, p67), names_to = "pct", values_to = "temp_val") %>%
  filter(!is.na(temp_val))

# Peak: maximum across years for each model × scenario × percentile
peak_all <- gsat_long %>%
  group_by(model, scenario, pct) %>%
  summarise(temp_val = max(temp_val, na.rm = TRUE), .groups = "drop") %>%
  mutate(time_type = "Peak temperature")

# T@2100: value at year 2100 for each model × scenario × percentile
end_all <- gsat_long %>%
  filter(year == 2100) %>%
  select(model, scenario, pct, temp_val) %>%
  mutate(time_type = "Temperature at 2100")

# Box statistics computed across all (model × percentile) combinations
box_stats <- bind_rows(peak_all, end_all) %>%
  mutate(
    scen_label = factor(
      unname(scen_labels_lbl[as.character(scenario)]),
      levels = scen_lbl_levels
    ),
    time_type = factor(time_type, levels = c("Peak temperature", "Temperature at 2100"))
  ) %>%
  group_by(scen_label, time_type) %>%
  summarise(
    ymin   = quantile(temp_val, 0.10, na.rm = TRUE),
    lower  = quantile(temp_val, 0.33, na.rm = TRUE),
    middle = quantile(temp_val, 0.50, na.rm = TRUE),
    upper  = quantile(temp_val, 0.67, na.rm = TRUE),
    ymax   = quantile(temp_val, 0.90, na.rm = TRUE),
    .groups = "drop"
  )


# ══════════════════════════════════════════════════════════════
# 6. BUILD PANELS
# ══════════════════════════════════════════════════════════════

x_breaks <- c(2020, seq(2040, 2100, by = 20))

# ── Panel (a): CO₂ world band ─────────────────────────────────
p_a <- ggplot(co2_band,
              aes(x = year, colour = scenario, fill = scenario, group = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, linewidth = 0.25) +
  geom_line(aes(y = med), linewidth = 0.9) +
  scale_colour_manual(values = scen_col_lbl, name = NULL) +
  scale_fill_manual(  values = scen_col_lbl, name = NULL) +
  scale_x_continuous(breaks = x_breaks) +
  labs(title = "(a) World CO\u2082 emissions", x = NULL, y = "Gt CO\u2082/yr") +
  guides(colour = guide_legend(ncol = 2, override.aes = list(fill = NA)), fill = "none") +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 45, hjust = 1))

# Colours for x-axis labels (one per scenario, in scen_lbl_levels order)
# Tech Momentum (#87CEEB) is too light for text — use the darker line colour instead
scen_col_text <- c(
  "Meet Aspirations" = "#2E7D32",
  "Late Reawakening" = "#D62728",
  "Tech Momentum"    = "#1565C0",
  "Shifted Ambition" = "#666666"
)
x_label_colours <- scen_col_text[scen_lbl_levels]

# ── Panel (b): temperature boxplots ───────────────────────────
p_b <- ggplot(box_stats,
              aes(x = scen_label,
                  fill  = time_type,
                  ymin  = ymin, lower = lower, middle = middle,
                  upper = upper, ymax  = ymax)) +
  geom_hline(yintercept = 2.0, linewidth = 0.6, linetype = "dotted", colour = "black") +
  annotate("text", x = Inf, y = 2.0, label = "2.0\u00b0C",
           hjust = 1.08, vjust = -0.45, size = 5.0, colour = "black") +
  geom_boxplot(aes(group = interaction(scen_label, time_type)),
               stat     = "identity",
               position = position_dodge(width = 0.65),
               width    = 0.55, alpha = 0.75,
               colour   = "grey20", linewidth = 0.55) +
  geom_text(data = box_stats,
            aes(x     = scen_label,
                y     = middle,
                group = interaction(scen_label, time_type),
                label = sprintf("%.1f\u00b0C", middle)),
            position     = position_dodge(width = 0.65),
            vjust        = -0.55, size = 3.5, colour = "grey20",
            fontface     = "bold",
            inherit.aes  = FALSE) +
  scale_fill_manual(
    values = c("Peak temperature"    = "#FFF9C4",
               "Temperature at 2100" = "#FFB6C1"),
    name   = NULL
  ) +
  scale_x_discrete(labels = scen_labels_wrap) +
  scale_y_continuous(limits = c(1.3, NA),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(title = "(b) Global surface temperature",
       x = NULL, y = "\u00b0C above 1850\u20131900") +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x        = element_text(size = 11, colour = x_label_colours, face = "bold"))


# ══════════════════════════════════════════════════════════════
# 7. COMBINE AND SAVE
# ══════════════════════════════════════════════════════════════

fig <- (p_a | p_b)

out_file <- file.path(graphs_dir, "fig2.png")
ggsave(out_file, fig, width = 14, height = 6.5, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig2", graphs_dir)
  }
})
