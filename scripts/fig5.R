library(tidyverse)
library(arrow)
library(jsonlite)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 figure 5
#   Two rows × three clusters (no Late Reawakening):
#     Row (a): Renewable share in primary energy (%)
#     Row (b): Electricity share in final energy (%)
#   Bar = median across models; points = individual models.
#   Labels on Shifted Ambition and Tech Momentum bars show
#   % change in median relative to Meet Aspirations.
#   Exact point values at 2030, 2040, 2050.
#   R10 regions summed within cluster before computing share.
#
# Output: graphs/fig5.png
# ══════════════════════════════════════════════════════════════

# ── Paths ──────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
direct_dir  <- file.path(project_dir, "data", "direct")
graphs_dir  <- file.path(project_dir, "graphs")
dir.create(direct_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(scripts_dir, "prisma_styles.R"))
source(file.path(scripts_dir, "model_fixes.R"))

if (!exists("DOWNLOAD_DATA", inherits = FALSE)) DOWNLOAD_DATA <- TRUE

# ── Scenarios (no Late Reawakening) ───────────────────────────
all_scenarios <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Staying Alive"
)
ma_scen <- "SSP2 - Meet Aspirations"

scenario_levels <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Staying Alive",
  "SSP2 - Asymmetric Roll-Back"
)
scenario_labels <- c(
  "SSP2 - Meet Aspirations"     = "Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition",
  "SSP2 - Staying Alive"        = "Tech Momentum"
)
scenario_colors <- c(
  "SSP2 - Meet Aspirations"     = "#5BB85C",
  "SSP2 - Asymmetric Roll-Back" = "#999999",
  "SSP2 - Staying Alive"        = "#87CEEB"
)

# ── Cluster map ────────────────────────────────────────────────
r10_regions <- c(
  "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)"
)
prisma_map <- tibble(
  region = r10_regions,
  cluster = c(
    "Diversifying economies",      # Africa
    "Diversifying economies",      # China+
    "Transition leaders",          # Europe
    "Diversifying economies",      # India+
    "Diversifying economies",      # Latin America
    "Fossil-dependent economies",  # Middle East
    "Diversifying economies",      # North America
    "Diversifying economies",      # Other
    "Transition leaders",          # Pacific OECD
    "Fossil-dependent economies",  # Reforming Economies
    "Diversifying economies"       # Rest of Asia
  )
)
cluster_levels <- c("Transition leaders", "Diversifying economies", "Fossil-dependent economies")

# ── Variables ──────────────────────────────────────────────────
pe_renew_vars <- c(
  "Primary Energy|Wind", "Primary Energy|Solar", "Primary Energy|Hydro",
  "Primary Energy|Geothermal", "Primary Energy|Bioenergy"
)
pe_total_var <- "Primary Energy"
fe_elec_var  <- "Final Energy|Electricity"
fe_total_var <- "Final Energy"

all_years  <- c(seq(2020, 2050, by = 5), seq(2060, 2100, by = 10))
plot_years <- c(2030L, 2040L, 2050L)

# ── Python download wrapper ────────────────────────────────────
miniconda_envs        <- file.path(Sys.getenv("USERPROFILE"), "miniconda3", "envs")
env_dirs              <- list.dirs(miniconda_envs, recursive = FALSE)
python_exe_candidates <- Filter(file.exists, file.path(env_dirs, "python.exe"))
if (length(python_exe_candidates) == 0) stop("No python.exe found under ", miniconda_envs)
python_exe <- python_exe_candidates[1]

run_python_download <- function(variable, prefix, regions_vec = r10_regions) {
  existing <- list.files(direct_dir, paste0("^", prefix, "_.*\\.parquet$"), full.names = TRUE)
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
    "  variables=json.loads(r'''", toJSON(variable,       auto_unbox = FALSE), "'''),\n",
    "  scenarios=json.loads(r'''", toJSON(all_scenarios,  auto_unbox = FALSE), "'''),\n",
    "  regions=json.loads(r'''",   toJSON(regions_vec,    auto_unbox = FALSE), "'''),\n",
    "  prefix='", prefix, "',\n",
    "  outdir=r'''", normalizePath(direct_dir, winslash = "/", mustWork = FALSE), "'''\n",
    ")\n"
  )
  tmp <- tempfile(fileext = ".py")
  writeLines(py_code, tmp)
  status <- system2(python_exe, tmp)
  if (status != 0) { message("Retrying..."); status <- system2(python_exe, tmp) }
  if (status != 0) warning("Download of '", prefix, "' failed.")
  else             message("Download of '", prefix, "' completed.")
}

load_pq <- function(prefix) {
  files <- list.files(direct_dir, paste0("^", prefix, "_.*\\.parquet$"), full.names = TRUE)
  files <- files[!grepl("_meta\\.parquet$", files)]
  # Fallback to committed backup data if no fresh download available
  if (length(files) == 0) {
    backup_dir <- file.path(project_dir, "data", "backup")
    files <- list.files(backup_dir, paste0("^", prefix, "_.*\\.parquet$"), full.names = TRUE)
    files <- files[!grepl("_meta\\.parquet$", files)]
    if (length(files) > 0)
      message("Using backup data for '", prefix, "' (no fresh download found).")
  }
  if (length(files) == 0) stop("No data found for '", prefix, "'. Run download manually.")
  read_parquet(files[which.max(file.info(files)$mtime)])
}

# ── Theme ──────────────────────────────────────────────────────
theme_set(
  theme_bw(base_size = 17, base_family = "Arial") +
    theme(
      axis.title        = element_text(size = 15.5),
      axis.title.y      = element_text(size = 12.5),
      axis.text.x       = element_text(size = 14.3),
      axis.text.y       = element_text(size = 14.3),
      strip.text        = element_text(size = 15.5, face = "bold"),
      strip.background  = element_blank(),
      strip.placement   = "outside",
      legend.title      = element_text(size = 15.5),
      legend.text       = element_text(size = 14.3),
      panel.grid.minor  = element_blank()
    )
)

# ══════════════════════════════════════════════════════════════
# 1. DOWNLOAD / LOAD
# ══════════════════════════════════════════════════════════════
run_python_download(c(pe_total_var, pe_renew_vars), "fig4alt_pe_r10", r10_regions)
run_python_download(c(fe_elec_var, fe_total_var),   "fig4alt_fe_r10", r10_regions)

pe_raw <- load_pq("fig4alt_pe_r10") %>%
  filter(variable %in% c(pe_total_var, pe_renew_vars),
         scenario %in% all_scenarios,
         region   %in% r10_regions,
         year     %in% all_years) %>%
  fix_gem_e3_years()

fe_raw <- load_pq("fig4alt_fe_r10") %>%
  filter(variable %in% c(fe_elec_var, fe_total_var),
         scenario %in% all_scenarios,
         region   %in% r10_regions,
         year     %in% all_years) %>%
  fix_gem_e3_years()

# ── Short model name lookup (same abbreviations as fig4.R) ────
model_short_names <- c(
  "GEM-E3 V2026"                       = "GEM-E3",
  "IMACLIM 2.0"                        = "IMACLIM",
  "IMAGE 3.4"                          = "IMAGE",
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "MESSAGEix",
  "OPEN-PROM 2.2"                      = "OPEN-PROM",
  "REMIND-MAgPIE 3.6-4.14"            = "REMIND",
  "REMIND-MAgPIE 3.7-4.14"            = "REMIND",
  "WITCH 6.0"                          = "WITCH"
)
short_name <- function(x) ifelse(x %in% names(model_short_names), model_short_names[x], x)

# ══════════════════════════════════════════════════════════════
# 2. MODEL FILTERING
# ══════════════════════════════════════════════════════════════
get_models <- function(df) {
  df %>% pull(model) %>% as.character() %>% unique() %>%
    setdiff("WITCH 6.0-EU28") %>% filter_latest_remind() %>% sort()
}
all_models_pe <- get_models(pe_raw)
all_models_fe <- get_models(fe_raw)
all_models    <- intersect(all_models_pe, all_models_fe)
message("Models: ", paste(all_models, collapse = ", "))

# Renewable vars common to all models
vars_per_model <- lapply(all_models, function(mod)
  unique(pe_raw$variable[pe_raw$model == mod & pe_raw$variable %in% pe_renew_vars]))
pe_renew_final <- Reduce(intersect, vars_per_model)
pe_renew_final <- intersect(pe_renew_vars, pe_renew_final)
message("Renewable PE vars (common): ", paste(pe_renew_final, collapse = ", "))
if (length(pe_renew_final) == 0) stop("No renewable PE vars common to all models.")

# ══════════════════════════════════════════════════════════════
# 3. COMPUTE SHARES PER CLUSTER
# ══════════════════════════════════════════════════════════════

# Row (a): renewable share in primary energy
pe_renew_cl <- pe_raw %>%
  filter(model %in% all_models, variable %in% pe_renew_final) %>%
  left_join(prisma_map, by = "region") %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(renew = sum(value, na.rm = TRUE), .groups = "drop")

pe_total_cl <- pe_raw %>%
  filter(model %in% all_models, variable == pe_total_var) %>%
  left_join(prisma_map, by = "region") %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

pe_share <- pe_renew_cl %>%
  inner_join(pe_total_cl, by = c("model", "scenario", "cluster", "year")) %>%
  mutate(value = renew / total * 100) %>%
  filter(is.finite(value), total > 0) %>%
  select(model, scenario, cluster, year, value) %>%
  mutate(metric = "Renewable in primary energy (%)")

# Row (b): electricity share in final energy
fe_elec_cl <- fe_raw %>%
  filter(model %in% all_models, variable == fe_elec_var) %>%
  left_join(prisma_map, by = "region") %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(elec = sum(value, na.rm = TRUE), .groups = "drop")

fe_total_cl <- fe_raw %>%
  filter(model %in% all_models, variable == fe_total_var) %>%
  left_join(prisma_map, by = "region") %>%
  group_by(model, scenario, cluster, year) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

fe_share <- fe_elec_cl %>%
  inner_join(fe_total_cl, by = c("model", "scenario", "cluster", "year")) %>%
  mutate(value = elec / total * 100) %>%
  filter(is.finite(value), total > 0) %>%
  select(model, scenario, cluster, year, value) %>%
  mutate(metric = "Electricity in final energy (%)")

# ══════════════════════════════════════════════════════════════
# 4. EXTRACT POINT VALUES AT PLOT YEARS
# ══════════════════════════════════════════════════════════════
metric_levels <- c(
  "Renewable in primary energy (%)",
  "Electricity in final energy (%)"
)

all_shares <- bind_rows(pe_share, fe_share) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_levels),
    cluster  = factor(cluster,  levels = cluster_levels),
    metric   = factor(metric,   levels = metric_levels)
  )

dots <- all_shares %>%
  filter(year %in% plot_years) %>%
  mutate(year_f    = factor(year, levels = as.character(plot_years)),
         model_lbl = factor(short_name(model), levels = sort(unique(short_name(model)))))

summary_df <- dots %>%
  group_by(year_f, scenario, cluster, metric) %>%
  summarise(median_val = median(value, na.rm = TRUE), .groups = "drop")

# ══════════════════════════════════════════════════════════════
# 5. LABELS — % CHANGE VS MEET ASPIRATIONS
#    All 3 scenarios kept so position_dodge aligns with bars;
#    Meet Aspirations gets an empty string (not drawn).
# ══════════════════════════════════════════════════════════════
meet_ref <- summary_df %>%
  filter(as.character(scenario) == ma_scen) %>%
  select(year_f, cluster, metric, meet_val = median_val)

labels_df <- summary_df %>%
  left_join(meet_ref, by = c("year_f", "cluster", "metric")) %>%
  mutate(
    pct_chg    = (median_val - meet_val) / meet_val * 100,
    label_text = ifelse(as.character(scenario) == ma_scen, "",
                        sprintf("%+.0f%%", pct_chg))
  )

# ══════════════════════════════════════════════════════════════
# 6. PLOT
# ══════════════════════════════════════════════════════════════
model_shapes <- resolve_model_shapes(sort(unique(dots$model_lbl)))
dodge <- position_dodge(width = 0.8)

p <- ggplot(summary_df,
            aes(x = year_f, y = median_val, fill = scenario, group = scenario)) +
  geom_col(position = dodge, width = 0.7, alpha = 0.85) +
  geom_point(data = dots,
             aes(y = value, shape = model_lbl, group = scenario),
             colour = "black", size = 2.8, alpha = 0.45, position = dodge) +
  geom_text(data    = labels_df,
            aes(x = year_f, y = median_val, group = scenario, label = label_text),
            position  = position_dodge(width = 0.8),
            vjust     = -0.4, hjust = -0.1, size = 4.9, fontface = "bold", colour = "grey20",
            inherit.aes = FALSE) +
  scale_fill_manual(values = scenario_colors, labels = scenario_labels, name = "Scenario") +
  scale_shape_manual(values = model_shapes, name = "Model",
                     labels = names(model_shapes)) +
  facet_grid(metric ~ cluster, scales = "free_y", switch = "y") +
  labs(x = NULL, y = NULL) +
  guides(
    fill  = guide_legend(order = 1, ncol = 1),
    shape = guide_legend(order = 2, ncol = ceiling(length(model_shapes) / 4), byrow = TRUE)
  ) +
  theme(legend.position = "bottom", legend.box = "horizontal")

out_file <- file.path(graphs_dir, "fig5.png")
ggsave(out_file, p, width = 14, height = 9, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig5", graphs_dir)
  }
})
