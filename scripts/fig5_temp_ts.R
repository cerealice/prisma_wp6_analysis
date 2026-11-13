library(tidyverse)
library(arrow)
library(jsonlite)
library(ggh4x)

# ══════════════════════════════════════════════════════════════
# PURPOSE — TEMP: electricity share in final energy (fig5 panel b)
#   as timeseries by model, faceted by scenario × cluster.
#   Includes IMACLIM. Cross-model median overlaid in grey.
#
# Output: graphs/fig5_temp_ts.png
# ══════════════════════════════════════════════════════════════

# ── Paths ──────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
direct_dir  <- file.path(project_dir, "data", "direct")
graphs_dir  <- file.path(project_dir, "graphs")
dir.create(direct_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graphs_dir,  recursive = TRUE, showWarnings = FALSE)

source(file.path(scripts_dir, "prisma_styles.R"))
source(file.path(scripts_dir, "model_fixes.R"))

if (!exists("DOWNLOAD_DATA", inherits = FALSE)) DOWNLOAD_DATA <- TRUE

# ── Scenarios (no Late Reawakening) ───────────────────────────
all_scenarios <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Staying Alive"
)

scenario_labels <- c(
  "SSP2 - Asymmetric Roll-Back" = "Shifted Ambition",
  "SSP2 - Staying Alive"        = "Tech Momentum",
  "SSP2 - Meet Aspirations"     = "Meet Aspirations"
)
# row order: Shifted top, Tech middle, Meet bottom
scen_lbl_levels <- c("Shifted Ambition", "Tech Momentum", "Meet Aspirations")

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
  region  = r10_regions,
  cluster = c(
    "Diversifying Economies",
    "Diversifying Economies",
    "Transition Leaders",
    "Diversifying Economies",
    "Diversifying Economies",
    "Fossil-Dependent Economies",
    "Diversifying Economies",
    "Diversifying Economies",
    "Transition Leaders",
    "Fossil-Dependent Economies",
    "Diversifying Economies"
  )
)
cluster_levels <- c("Global", "Transition Leaders", "Diversifying Economies", "Fossil-Dependent Economies")

# ── Variables ──────────────────────────────────────────────────
fe_elec_var  <- "Final Energy|Electricity"
fe_total_var <- "Final Energy"
all_years    <- c(seq(2020, 2050, by = 5), seq(2060, 2100, by = 10))

# ── Model short names ──────────────────────────────────────────
model_short_names <- c(
  "GEM-E3 V2026"                       = "GEM-E3",
  "IMACLIM 2.0"                        = "IMACLIM",
  "IMAGE 3.4"                          = "IMAGE",
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "MESSAGE",
  "OPEN-PROM 2.2"                      = "OPEN-PROM",
  "REMIND-MAgPIE 3.6-4.14"            = "REMIND",
  "REMIND-MAgPIE 3.7-4.14"            = "REMIND",
  "WITCH 6.0"                          = "WITCH"
)
short_name <- function(x) ifelse(x %in% names(model_short_names), model_short_names[x], x)

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
    "  variables=json.loads(r'''", toJSON(variable,      auto_unbox = FALSE), "'''),\n",
    "  scenarios=json.loads(r'''", toJSON(all_scenarios, auto_unbox = FALSE), "'''),\n",
    "  regions=json.loads(r'''",   toJSON(regions_vec,   auto_unbox = FALSE), "'''),\n",
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
      axis.title       = element_text(size = 15.5),
      axis.text.x      = element_text(size = 13.0, angle = 45, hjust = 1),
      axis.text.y      = element_text(size = 13.0),
      strip.text       = element_text(size = 14.5, face = "bold"),
      strip.background = element_blank(),
      legend.title     = element_text(size = 14.5),
      legend.text      = element_text(size = 13.0),
      panel.grid.minor = element_blank()
    )
)

# ══════════════════════════════════════════════════════════════
# 1. DOWNLOAD / LOAD
# ══════════════════════════════════════════════════════════════
run_python_download(c(fe_elec_var, fe_total_var), "fig4alt_fe_r10")

fe_raw <- load_pq("fig4alt_fe_r10") %>%
  filter(variable %in% c(fe_elec_var, fe_total_var),
         scenario %in% all_scenarios,
         region   %in% r10_regions,
         year     %in% all_years) %>%
  fix_gem_e3_years()

# ══════════════════════════════════════════════════════════════
# 2. MODEL FILTERING
# ══════════════════════════════════════════════════════════════
all_models <- fe_raw %>% pull(model) %>% as.character() %>% unique() %>%
  setdiff("WITCH 6.0-EU28") %>% filter_latest_remind() %>% sort()
message("Models: ", paste(all_models, collapse = ", "))

all_model_lbls <- sort(unique(short_name(all_models)))
custom_pal     <- c("#A41623", "#FF7F0E", "#5B1865", "#E2C044",
                    "#718F94", "#2CA02C", "#1F77B4", "#8C564B")
model_colors   <- setNames(rep_len(custom_pal, length(all_model_lbls)), all_model_lbls)
model_shapes   <- resolve_model_shapes(all_model_lbls)

# ══════════════════════════════════════════════════════════════
# 3. COMPUTE ELECTRICITY SHARE IN FINAL ENERGY PER CLUSTER + GLOBAL
# ══════════════════════════════════════════════════════════════

# Per cluster (R10 regions summed within cluster)
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

fe_share_cl <- fe_elec_cl %>%
  inner_join(fe_total_cl, by = c("model", "scenario", "cluster", "year")) %>%
  mutate(value = elec / total * 100) %>%
  filter(is.finite(value), total > 0) %>%
  select(model, scenario, cluster, year, value)

# Global (all R10 regions summed)
fe_elec_gl <- fe_raw %>%
  filter(model %in% all_models, variable == fe_elec_var) %>%
  group_by(model, scenario, year) %>%
  summarise(elec = sum(value, na.rm = TRUE), .groups = "drop")

fe_total_gl <- fe_raw %>%
  filter(model %in% all_models, variable == fe_total_var) %>%
  group_by(model, scenario, year) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

fe_share_gl <- fe_elec_gl %>%
  inner_join(fe_total_gl, by = c("model", "scenario", "year")) %>%
  mutate(value = elec / total * 100, cluster = "Global") %>%
  filter(is.finite(value), total > 0) %>%
  select(model, scenario, cluster, year, value)

# Combine and factorise
fe_share <- bind_rows(fe_share_gl, fe_share_cl) %>%
  mutate(
    model_lbl    = factor(short_name(as.character(model)), levels = all_model_lbls),
    scenario_lbl = factor(scenario_labels[as.character(scenario)], levels = scen_lbl_levels),
    cluster      = factor(cluster, levels = cluster_levels)
  )

# Cross-model median per scenario × cluster × year
fe_med <- fe_share %>%
  group_by(scenario_lbl, cluster, year) %>%
  summarise(med = median(value, na.rm = TRUE), .groups = "drop")

# Labels at 2030, 2050, 2080
label_years <- c(2030, 2035, 2050, 2080)
fe_med_lbl  <- fe_med %>% filter(year %in% label_years)

# ══════════════════════════════════════════════════════════════
# 4. PLOT
# ══════════════════════════════════════════════════════════════
x_breaks <- c(2020, 2030, 2040, 2050, 2060, 2080, 2100)

p <- ggplot(fe_share, aes(x = year, y = value, colour = model_lbl, group = model_lbl)) +
  geom_line(linewidth = 0.8, alpha = 0.75) +
  geom_line(data = fe_med, aes(x = year, y = med, group = 1),
            colour = "grey30", linewidth = 1.6, inherit.aes = FALSE) +
  geom_text(data = fe_med_lbl,
            aes(x = year, y = med, label = sprintf("%.0f%%", med)),
            colour = "grey20", fontface = "bold", size = 3.8,
            vjust = -0.6, hjust = 0.5, inherit.aes = FALSE) +
  facet_grid(scenario_lbl ~ cluster) +
  scale_colour_manual(values = model_colors, name = "Model") +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    title = "Electricity share in final energy — by model and cluster",
    x     = NULL,
    y     = "% of final energy demand"
  ) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 1.5))) +
  theme(legend.position = "right")

# ══════════════════════════════════════════════════════════════
# 5. SAVE
# ══════════════════════════════════════════════════════════════
out_file <- file.path(graphs_dir, "fig5_temp_ts.png")
ggsave(out_file, p, width = 20, height = 11, dpi = 300)
message("Saved: ", out_file)

# ══════════════════════════════════════════════════════════════
# 6. BOXPLOT — R10 electricity share at 2035 (IMACLIM excluded)
# ══════════════════════════════════════════════════════════════
region_short <- c(
  "Africa (R10)"               = "Africa",
  "China+ (R10)"               = "China+",
  "Europe (R10)"               = "Europe",
  "India+ (R10)"               = "India+",
  "Latin America (R10)"        = "Lat. Am.",
  "Middle East (R10)"          = "Mid. East",
  "North America (R10)"        = "N. Am.",
  "Other (R10)"                = "Other",
  "Pacific OECD (R10)"         = "Pac. OECD",
  "Reforming Economies (R10)"  = "Reform. Ec.",
  "Rest of Asia (R10)"         = "Rest Asia"
)

fe_elec_r10 <- fe_raw %>%
  filter(model %in% all_models,
         !as.character(model) %in% c("IMACLIM 2.0"),
         variable == fe_elec_var, year == 2035) %>%
  select(model, scenario, region, elec = value)

fe_total_r10 <- fe_raw %>%
  filter(model %in% all_models,
         !as.character(model) %in% c("IMACLIM 2.0"),
         variable == fe_total_var, year == 2035) %>%
  select(model, scenario, region, total = value)

box_r10 <- fe_elec_r10 %>%
  inner_join(fe_total_r10, by = c("model", "scenario", "region")) %>%
  mutate(
    value        = elec / total * 100,
    model_lbl    = factor(short_name(as.character(model)), levels = all_model_lbls),
    scenario_lbl = factor(scenario_labels[as.character(scenario)], levels = scen_lbl_levels),
    region_short = factor(region_short[region], levels = unname(region_short))
  ) %>%
  filter(is.finite(value), total > 0)

p_box_r10 <- ggplot(box_r10,
                    aes(x = region_short, y = value, fill = scenario_lbl)) +
  geom_hline(yintercept = 35, linetype = "dashed", colour = "red", linewidth = 0.7) +
  geom_boxplot(alpha = 0.6, width = 0.65, outlier.shape = NA,
               position = position_dodge(width = 0.75)) +
  geom_point(aes(colour = scenario_lbl, shape = model_lbl),
             size = 3.0, alpha = 0.9, stroke = 0.8,
             position = position_jitterdodge(jitter.width = 0.08,
                                             dodge.width  = 0.75, seed = 42)) +
  scale_fill_manual(values = setNames(
    c("#999999", "#87CEEB", "#5BB85C"), scen_lbl_levels), name = NULL) +
  scale_colour_manual(values = setNames(
    c("#999999", "#87CEEB", "#5BB85C"), scen_lbl_levels), guide = "none") +
  scale_shape_manual(values = model_shapes, name = "Model") +
  labs(
    title = "Electricity share in final energy by R10 region at 2035 (excl. IMACLIM)",
    x     = NULL,
    y     = "% of final energy demand"
  ) +
  guides(fill  = guide_legend(ncol = 1, order = 1),
         shape = guide_legend(ncol = 1, order = 2)) +
  theme(legend.position  = "right",
        axis.text.x      = element_text(size = 11, angle = 35, hjust = 1))

out_box_r10 <- file.path(graphs_dir, "fig5_temp_box2035_r10.png")
ggsave(out_box_r10, p_box_r10, width = 16, height = 7, dpi = 300)
message("Saved: ", out_box_r10)