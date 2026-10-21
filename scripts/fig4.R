library(tidyverse)
library(arrow)
library(jsonlite)
library(grid)
library(gtable)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15  fig4.R
#   Dot-plot alternative to the Brutschin et al. (2021) table
#   for SSP2 - Late Reawakening.
#   One panel per indicator; one dot per model (worst decade).
#   Green = low concern, yellow = medium, red = high.
#
# Output: graphs/post_oct15/fig4.png
# ══════════════════════════════════════════════════════════════

# ── Paths ──────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
direct_dir  <- file.path(project_dir, "data", "direct")
graphs_dir  <- file.path(project_dir, "graphs")
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(direct_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(scripts_dir, "prisma_styles.R"))
source(file.path(scripts_dir, "model_fixes.R"))
source(file.path(scripts_dir, "carbon_price_loader.R"))

if (!exists("DOWNLOAD_DATA", inherits = FALSE)) DOWNLOAD_DATA <- TRUE

# ── Settings ──────────────────────────────────────────────────
scenarios     <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive"
)
BASELINE_SCEN <- "SSP2 - Meet Aspirations"
TARGET_SCEN   <- "SSP2 - Late Reawakening"
CP_DISCOUNT   <- 0.05

SELECTED_INDICATORS <- c("2.1", "3.1", "3.2", "4.1", "5.1")

dec_end <- seq(2030, 2100, by = 10)
need    <- function(ids) is.null(SELECTED_INDICATORS) || any(SELECTED_INDICATORS %in% ids)

r10_regions <- c(
  "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)"
)

model_short_names <- c(
  "GEM-E3 V2026"                       = "GEM-E3",
  "IMACLIM 2.0"                        = "IMACLIM",
  "IMAGE 3.4"                          = "IMAGE",
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "MESSAGEix",
  "OPEN-PROM 2.2"                      = "OPEN-PROM",
  "REMIND-MAgPIE 3.6-4.14"            = "REMIND 3.6",
  "REMIND-MAgPIE 3.7-4.14"            = "REMIND 3.7",
  "WITCH 6.0"                          = "WITCH"
)
short_name <- function(x) ifelse(x %in% names(model_short_names), model_short_names[x], x)

# Dimension strip colours
dim_palette <- c(
  "Economic"       = "#FFF0D9",
  "Technological"  = "#DDE8F5",
  "Socio-cultural" = "#EDE8F5",
  "Institutional"  = "#F5DDE8"
)

# ── Python download wrapper ────────────────────────────────────
miniconda_envs        <- file.path(Sys.getenv("USERPROFILE"), "miniconda3", "envs")
env_dirs              <- list.dirs(miniconda_envs, recursive = FALSE)
python_exe_candidates <- Filter(file.exists, file.path(env_dirs, "python.exe"))
if (length(python_exe_candidates) == 0) stop("No python.exe found under ", miniconda_envs)
python_exe <- python_exe_candidates[1]

run_python_download <- function(variable, prefix, regions_vec = "World") {
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
    "  variables=json.loads(r'''", toJSON(variable, auto_unbox = FALSE), "'''),\n",
    "  scenarios=json.loads(r'''", toJSON(scenarios, auto_unbox = FALSE), "'''),\n",
    "  regions=json.loads(r'''",  toJSON(regions_vec, auto_unbox = FALSE), "'''),\n",
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
  if (length(files) == 0) return(NULL)
  df <- read_parquet(files[which.max(file.info(files)$mtime)])
  df <- fix_gem_e3_years(df)
  df$model    <- as.character(df$model)
  df$scenario <- as.character(df$scenario)
  df$region   <- as.character(df$region)
  df$variable <- as.character(df$variable)
  df
}

# ══════════════════════════════════════════════════════════════
# 1. DOWNLOAD / LOAD DATA
# ══════════════════════════════════════════════════════════════
elec_vars <- c(
  "Secondary Energy|Electricity",
  "Secondary Energy|Electricity|Wind",
  "Secondary Energy|Electricity|Solar"
)
fe_vars  <- c("Final Energy")
reg_vars <- c("Emissions|CO2", "Population")

if (need(c("3.1","3.2"))) run_python_download(elec_vars, "bru_elec")
if (need("4.1"))          run_python_download(fe_vars,   "bru_fe")
run_python_download(reg_vars, "bru_reg", r10_regions)

elec_raw <- if (need(c("3.1","3.2"))) load_pq("bru_elec") else NULL
fe_raw   <- if (need("4.1"))                load_pq("bru_fe")   else NULL
reg_raw  <- load_pq("bru_reg")

cp_raw <- tryCatch(
  load_cp_weighted(regions = c("World", r10_regions)),
  error = function(e) { message("Carbon price not available: ", conditionMessage(e)); NULL }
)
if (!is.null(cp_raw)) {
  if (!"variable" %in% names(cp_raw)) cp_raw$variable <- "Price|Carbon"
  cp_raw$model    <- as.character(cp_raw$model)
  cp_raw$scenario <- as.character(cp_raw$scenario)
  cp_raw$region   <- as.character(cp_raw$region)
  cp_raw <- fix_gem_e3_years(cp_raw)
}

# ══════════════════════════════════════════════════════════════
# 2. HELPERS
# ══════════════════════════════════════════════════════════════
YEARS <- 2015:2100

get_series <- function(df, mod, scen, vars, region = "World") {
  na_out <- setNames(rep(NA_real_, length(YEARS)), YEARS)
  if (is.null(df)) return(na_out)
  for (v in vars) {
    d <- df[df$model == mod & df$scenario == scen & df$variable == v & df$region == region, ]
    d <- d[!is.na(d$value), ]
    d <- d[order(d$year), ]
    d <- d[!duplicated(d$year), ]
    if (nrow(d) >= 2)
      return(setNames(approx(d$year, d$value, xout = YEARS, rule = 1)$y, YEARS))
  }
  na_out
}

at          <- function(x, yrs) unname(x[as.character(yrs)])
dec_delta   <- function(x) at(x, dec_end) - at(x, dec_end - 10)
dec_pct_chg <- function(x) 100 * (at(x, dec_end) - at(x, dec_end - 10)) / at(x, dec_end - 10)

share_series <- function(df, mod, scen, num_vars, den_vars) {
  n <- get_series(df, mod, scen, num_vars)
  d <- get_series(df, mod, scen, den_vars)
  out <- 100 * n / d
  out[!is.na(d) & d <= 0] <- NA_real_
  out
}

lvl    <- function(v, med, high) ifelse(is.na(v), NA_integer_, ifelse(v >= high, 2L, ifelse(v >= med, 1L, 0L)))
na_res <- function() list(value = rep(NA_real_, length(dec_end)), level = rep(NA_integer_, length(dec_end)))

simple_ind <- function(id, dim, name, unit, med, high, fn)
  list(id = id, dim = dim, name = name, unit = unit, med = med, high = high,
       fn = function(m, s) { v <- fn(m, s); list(value = v, level = lvl(v, med, high)) })

# ══════════════════════════════════════════════════════════════
# 3. INDICATORS  (same logic as fig3.R)
# ══════════════════════════════════════════════════════════════
indicators <- list(

  list(id = "2.1", dim = "Economic", name = "Carbon price growth",
       unit = "\u00d7 per decade", med = 5, high = 5,
       fn = function(m, s) {
         if (is.null(cp_raw)) return(na_res())
         p <- get_series(cp_raw, m, s, "Price|Carbon", region = "World")
         if (all(is.na(p))) {
           ps <- lapply(r10_regions, function(r) get_series(cp_raw, m, s, "Price|Carbon", region = r))
           if (all(sapply(ps, function(x) all(is.na(x))))) return(na_res())
           p <- do.call(pmax, c(ps, na.rm = TRUE)); names(p) <- YEARS
         }
         pd    <- at(p, dec_end)
         pprev <- at(p, dec_end - 10)
         ratio <- pd / (pprev + 1)
         level <- ifelse(is.na(ratio), NA_integer_, ifelse(ratio >= 5, 2L, 0L))
         list(value = ratio, level = level)
       }),

  simple_ind("3.1", "Technological", "Wind scale-up", "pp/decade", 10, 20,
    function(m, s) dec_delta(share_series(elec_raw, m, s,
      "Secondary Energy|Electricity|Wind", "Secondary Energy|Electricity"))),

  simple_ind("3.2", "Technological", "Solar scale-up", "pp/decade", 10, 20,
    function(m, s) dec_delta(share_series(elec_raw, m, s,
      "Secondary Energy|Electricity|Solar", "Secondary Energy|Electricity"))),

  simple_ind("4.1", "Socio-cultural", "Energy demand decline", "%/decade", 10, 20,
    function(m, s) -dec_pct_chg(get_series(fe_raw, m, s, "Final Energy"))),

  list(id = "5.1", dim = "Institutional", name = "Per-capita CO\u2082 decline",
       unit = "%/decade", med = 10, high = 20,
       fn = function(m, s) {
         if (is.null(reg_raw)) return(na_res())
         co_mat <- sapply(r10_regions, function(r) get_series(reg_raw, m, s, "Emissions|CO2",  region = r))
         po_mat <- sapply(r10_regions, function(r) get_series(reg_raw, m, s, "Population",      region = r))
         co_w <- rowSums(co_mat, na.rm = TRUE)
         po_w <- rowSums(po_mat, na.rm = TRUE)
         po_w[po_w == 0] <- NA_real_
         pc  <- setNames(co_w / po_w, YEARS)
         v   <- 100 * (at(pc, dec_end - 10) - at(pc, dec_end)) / at(pc, dec_end - 10)
         list(value = v, level = lvl(v, 10, 20))
       })
)
names(indicators) <- sapply(indicators, "[[", "id")

# ══════════════════════════════════════════════════════════════
# 4. COMPUTE  (Late Reawakening only; worst decade per model)
# ══════════════════════════════════════════════════════════════
extract_mods <- function(df, scen) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  sort(unique(df$model[df$scenario == scen]))
}

run_one <- function(spec, m, s) {
  r <- tryCatch(spec$fn(m, s), error = function(e) {
    warning(spec$id, " / ", m, ": ", conditionMessage(e)); na_res()
  })
  tibble(model = m, id = spec$id, decade = dec_end,
         value = as.numeric(r$value), level = as.integer(r$level))
}

mods <- Reduce(union, lapply(list(elec_raw, fe_raw, reg_raw, cp_raw),
                             extract_mods, scen = TARGET_SCEN))
mods <- filter_latest_remind(mods)
mods <- sort(mods[mods != "WITCH 6.0-EU28"])
# Rename whichever REMIND version survived the filter to the simple label "REMIND"
remind_survivor <- grep("^REMIND-MAgPIE", mods, value = TRUE)
if (length(remind_survivor) > 0)
  model_short_names[remind_survivor] <- "REMIND"
message("Models: ", paste(mods, collapse = ", "))

all_rows <- list()
for (m in mods)
  for (spec in indicators)
    all_rows[[length(all_rows) + 1]] <- run_one(spec, m, TARGET_SCEN)

long_lr <- bind_rows(all_rows)

# Worst decade per model × indicator
cell_lr <- long_lr %>%
  group_by(model, id) %>%
  group_modify(function(d, key) {
    if (all(is.na(d$value)))
      return(tibble(value = NA_real_, level = NA_integer_, peak_decade = NA_integer_))
    lmax <- max(d$level, na.rm = TRUE)
    cand <- which(d$level == lmax & !is.na(d$value))
    j    <- cand[which.max(d$value[cand])]
    tibble(value = d$value[j], level = lmax, peak_decade = d$decade[j])
  }) %>%
  ungroup()

# ══════════════════════════════════════════════════════════════
# 5. BUILD DOT PLOT
# ══════════════════════════════════════════════════════════════

# Indicator metadata for strip labels and thresholds
ind_meta_dot <- tibble(
  id   = sapply(indicators, "[[", "id"),
  dim  = sapply(indicators, "[[", "dim"),
  name = sapply(indicators, "[[", "name"),
  unit = sapply(indicators, "[[", "unit"),
  med  = sapply(indicators, "[[", "med"),
  high = sapply(indicators, "[[", "high")
) %>%
  mutate(
    thr_txt     = if_else(id == "2.1",
                          "H \u2265 5\u00d7",
                          paste0("M \u2265 ", med, " \u00b7 H \u2265 ", high)),
    strip_label = paste0(name, "\n(", unit, ")\n", thr_txt),
    strip_label = factor(strip_label, levels = strip_label)  # lock order
  )

# Join cell with metadata
mod_order <- sort(short_name(mods))

dot_df <- cell_lr %>%
  left_join(ind_meta_dot, by = "id") %>%
  mutate(
    model_short = factor(short_name(model), levels = mod_order),
    fill_col    = case_when(
      is.na(level) ~ "grey70",
      level == 2L  ~ "#E8595A",
      level == 1L  ~ "#F4D03F",
      TRUE         ~ "#5BB85C"
    ),
    val_label   = case_when(
      is.na(value)      ~ NA_character_,
      abs(value) >= 1e3 ~ paste0(round(value / 1000, 1), "k"),
      abs(value) >= 100 ~ as.character(round(value, 0)),
      TRUE              ~ formatC(value, format = "f", digits = 1)
    )
  )

# Threshold data frames for geom_hline
thresh_high_df <- ind_meta_dot %>%
  select(strip_label, thr_val = high)

thresh_med_df <- ind_meta_dot %>%
  filter(id != "2.1", med != high) %>%   # skip 2.1 (single threshold)
  select(strip_label, thr_val = med)

# Model shapes
shape_vals <- c(15, 16, 17, 18, 8, 1, 0, 2, 5, 6, 3, 4)
shape_vec  <- setNames(shape_vals[seq_along(mod_order)], mod_order)

# ── Build ggplot ───────────────────────────────────────────────
p_dot <- ggplot(dot_df, aes(x = model_short, y = value)) +

  # Medium threshold (yellow dashed)
  geom_hline(data = thresh_med_df,
             aes(yintercept = thr_val),
             linetype = "dashed", linewidth = 0.7, colour = "#C8A400",
             inherit.aes = FALSE) +
  geom_text(data = thresh_med_df,
            aes(x = -Inf, y = thr_val, label = paste0("M: ", thr_val)),
            hjust = -0.15, vjust = 1.5, size = 5.5,
            colour = "#8B6914", fontface = "italic",
            inherit.aes = FALSE) +

  # High threshold (red dotted)
  geom_hline(data = thresh_high_df,
             aes(yintercept = thr_val),
             linetype = "dotted", linewidth = 0.9, colour = "#C94040",
             inherit.aes = FALSE) +
  geom_text(data = thresh_high_df,
            aes(x = -Inf, y = thr_val, label = paste0("H: ", thr_val)),
            hjust = -0.15, vjust = -0.45, size = 5.5,
            colour = "#C94040", fontface = "italic",
            inherit.aes = FALSE) +

  # Dots
  geom_point(aes(colour = fill_col, shape = model_short),
             size = 6.5, stroke = 0.7, na.rm = TRUE) +

  # Value labels (only for non-NA values)
  geom_text(data = filter(dot_df, !is.na(value)),
            aes(label  = val_label,
                colour = fill_col,
                vjust  = if_else(level >= 1L, -0.75, 1.9)),
            size = 6.5, fontface = "bold") +

  facet_wrap(~ strip_label, nrow = 1, scales = "free_y") +
  scale_colour_identity() +
  scale_shape_manual(values = shape_vec, name = "Model") +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.22))) +
  labs(
    title = "Brutschin et al. (2021, Table 1) feasibility indicators for Late Reawakening",
    x     = NULL,
    y     = "Value (worst decade)"
  ) +
  guides(shape = guide_legend(ncol = 1, override.aes = list(size = 3.5))) +
  theme_bw(base_size = 17, base_family = "Arial") +
  theme(
    plot.title         = element_text(size = 18, face = "bold"),
    axis.text.x        = element_text(size = 14, angle = 35, hjust = 1),
    axis.text.y        = element_text(size = 14),
    strip.text         = element_text(size = 14, face = "bold", lineheight = 1.2),
    strip.background   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(size = 14),
    legend.text        = element_text(size = 13)
  )

# ── Colour strip backgrounds by dimension ─────────────────────
strip_dims  <- ind_meta_dot$dim[match(levels(dot_df$strip_label),
                                       as.character(ind_meta_dot$strip_label))]
strip_fills <- dim_palette[strip_dims]

grob      <- ggplotGrob(p_dot)
strip_idx <- grep("strip-t", grob$layout$name)

for (i in seq_along(strip_idx)) {
  fill_col   <- strip_fills[i]
  if (is.na(fill_col)) next
  panel_grob <- grob$grobs[[strip_idx[i]]]
  rect_idx   <- grep("rect", names(panel_grob$grobs[[1]]$children))
  if (length(rect_idx) > 0)
    panel_grob$grobs[[1]]$children[[rect_idx]]$gp$fill <- fill_col
  grob$grobs[[strip_idx[i]]] <- panel_grob
}

n_inds   <- nrow(ind_meta_dot)
out_file <- file.path(graphs_dir, "fig4.png")
ggsave(out_file, grob,
       width  = 2.9 * n_inds + 2.8,
       height = 6.5,
       dpi    = 300)
message("Saved: ", out_file)

# ── Update captions ────────────────────────────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig4", graphs_dir)
  }
})
