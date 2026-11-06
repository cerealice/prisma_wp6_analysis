library(tidyverse)
library(arrow)
library(jsonlite)
library(ggh4x)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15  fig_sm9.R
#   Dot-plot alternative to the Brutschin et al. (2021) table
#   for three PRISMA scenarios (Meet Aspirations, Shifted
#   Ambition, Tech Momentum).
#   Layout: facet_grid(scenario × indicator), one dot per model
#   (worst decade).  Green = low, yellow = medium, red = high.
#
# Output: graphs/fig_sm9.png
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
scenarios <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Late Reawakening",
  "SSP2 - Staying Alive"
)
BASELINE_SCEN <- "SSP2 - Meet Aspirations"
CP_DISCOUNT   <- 0.05   # Brutschin SM 3.2: NPV at 5% for the level condition

scen_labels <- c(
  "SSP2 - Meet Aspirations"     = "(a) Meet\nAspirations",
  "SSP2 - Asymmetric Roll-Back" = "(b) Shifted\nAmbition",
  "SSP2 - Staying Alive"        = "(c) Tech\nMomentum",
  "SSP2 - Late Reawakening"     = "Late Reawakening"
)
plot_scens <- names(scen_labels)          # all four: limits computed once
SM9_SCENS  <- plot_scens[1:3]             # Extended Data figure
FIG4_SCEN  <- "SSP2 - Late Reawakening"   # main figure

# TEMPORARY COMPARISON BUILD.
#   2.1  carbon price growth, undiscounted level condition
#   2.1b carbon price growth, level condition on NPV at 5% to 2020
#   5.1  per-capita CO2 decline, fixed screen (EU / Pac OECD / N. America out)
#   5.1b per-capita CO2 decline, time-varying governance screen (Andrijevic)
# Drop 2.1b and 5.1b once the comparison is settled.
SELECTED_INDICATORS <- c("2.1", "3.1", "3.2", "4.1", "5.1b", "P1")  # all at cluster level

# Headroom below and above the plotted values, as a fraction of their range,
# so the value labels are not clipped. Applied in ind_y_lims.
EXPAND_LO <- 0.28
EXPAND_HI <- 0.24
# The carbon price panel starts at zero, so it needs less room underneath.
EXPAND_LO_CP <- 0.10

# Carbon price growth has one extreme value (IMAGE under Late Reawakening).
# Values above this ratio do not set the axis: the marker is drawn near the top
# of the panel with an upward arrow and its true value printed beside it, so the
# remaining models are not compressed into the bottom of the panel.
CP_IDS   <- c("2.1", "2.1b")
CP_Y_CAP <- Inf   # Inf disables the off-scale arrows
# Fixed upper limit for the carbon price panel.
CP_Y_MAX <- 75

# The last panel (max annual CO2 decline) is drawn wider than the others so its
# leftmost value labels are not clipped by the panel edge.
P1_WIDEN <- 1.0
# The carbon price panel is widened too: its labels are the longest.
CP_WIDEN <- 1.0
# Width in inches of one indicator panel.
PANEL_W <- 3.4

# First year of the first decade assessed. The retreat in the scenario design
# begins in 2025, so decades are counted from there: 2025-2035, 2035-2045 and
# so on to 2085-2095. Set to 2020 to recover the 2020-2030 to 2090-2100 grid.
DEC_FIRST_START <- 2025
# Gap between panels, in lines: tight everywhere except before the last panel,
# which is not a Brutschin indicator and carries the divider.
SPACING_X    <- 0.5
SPACING_LAST <- 2.0

# Largest single-year fall in global CO2 on record: 2020 pandemic shutdown.
COVID_DECLINE <- 2.23

# Brutschin et al. Table 1, indicator 2.1: a fivefold rise per decade.
# Check SM section 3 for the currency year and whether the level is discounted.
CP_RATIO     <- 5
# Denominator floor: growth is measured against at least this price, so that a
# near-zero starting price does not produce an arbitrarily large ratio.
CP_FLOOR     <- 10    # USD2010/tCO2. Brutschin et al. set no denominator floor:
                      # their level condition (NPV >= 120 USD at 5%) does that work.
                      # 10 USD is the World Bank 2020 global average carbon price
                      # quoted in their own SM 3.2, i.e. the level below which a
                      # "decadal increase" carries no meaning.
CP_MIN_LEVEL <- 120   # Brutschin's level condition, kept for panel 2.1b only
# The 120$ level is applied to the price in NPV discounted to 2020 at
# CP_DISCOUNT. The ratio uses undiscounted prices.

# Brutschin indicator 5.1. High concern requires a fast per-capita CO2
# reduction AND a projected governance level below 0.6. Governance is applied
# here as a regional screen: the three R10 regions that clear 0.6 under SSP2
# (Europe, Pacific OECD, North America) are excluded, and the indicator reports
# the worst decline among the remaining regions. The 20% rate threshold is the
# reduction the EU-28 achieved over 2010-2019.
PC_RATE    <- 20
PC_EXCLUDE <- c("Europe (R10)", "Pacific OECD (R10)", "North America (R10)")
PC_GOV_THR <- 0.6
# Indicator 5.1 is evaluated on the PRISMA clusters rather than on R10 regions.
# Population-weighted SSP2 governance (Andrijevic et al.) puts Transition
# Leaders above the 0.6 threshold for the whole century (0.78 in 2020 rising to
# 0.89 in 2100) and the other two clusters below it in 2020 (0.51 and 0.41), so
# only those two are eligible, and only in the decades in which they are
# still projected below it. See data/governance_cluster_ssp2.csv.
PC_CLUSTER_MAP <- c(
  "Africa (R10)"              = "Diversifying Economies",
  "China+ (R10)"              = "Diversifying Economies",
  "Europe (R10)"              = "Transition Leaders",
  "India+ (R10)"              = "Diversifying Economies",
  "Latin America (R10)"       = "Diversifying Economies",
  "Middle East (R10)"         = "Fossil-Dependent",
  "North America (R10)"       = "Diversifying Economies",
  "Other (R10)"               = "Diversifying Economies",
  "Pacific OECD (R10)"        = "Transition Leaders",
  "Reforming Economies (R10)" = "Fossil-Dependent",
  "Rest of Asia (R10)"        = "Diversifying Economies"
)
PC_CLUSTERS <- c("Diversifying Economies", "Fossil-Dependent")
   # Brutschin SM 6.4 governance threshold

# Population-weighted SSP2 governance by R10 region and decade, aggregated from
# Andrijevic et al. (2019) country projections using the IAMC common-definitions
# R10 membership. Built by scripts/build_governance_r10.py; the country mapping
# is in data/country_to_r10.csv.
gov_cl <- local({
  f <- file.path(project_dir, "data", "governance_cluster_ssp2.csv")
  if (!file.exists(f)) { message("governance_cluster_ssp2.csv not found"); return(NULL) }
  readr::read_csv(f, show_col_types = FALSE)
})

# projected governance of cluster cl in the decade ending at year y
gov_at <- function(cl, y) {
  if (is.null(gov_cl)) return(NA_real_)
  d <- gov_cl[gov_cl$cluster == cl, ]
  if (nrow(d) == 0) return(NA_real_)
  # The projections are on a decadal grid. Decade ends need not fall on it
  # (they do not when DEC_FIRST_START is 2025), so interpolate, clamped to
  # the first and last year available.
  yy <- min(max(y, min(d$year)), max(d$year))
  stats::approx(d$year, d$governance, xout = yy)$y
}

dec_end <- seq(DEC_FIRST_START + 10, 2100, by = 10)
need    <- function(ids) is.null(SELECTED_INDICATORS) || any(SELECTED_INDICATORS %in% ids)

r10_regions <- c(
  "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)"
)

# Regions over which indicator 5.1 is evaluated (see PC_EXCLUDE above).
PC_REGIONS <- setdiff(r10_regions, PC_EXCLUDE)

# Short label for a region, used to tag "worst region" values in the plot.
short_region <- function(r) sub(" \\(R10\\)$", "", r)

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

# Dimension strip colours.
# The first four are the Brutschin et al. dimensions. "Transition pace"
# is not one of theirs, so it gets a neutral grey strip to keep the
# distinction visible at a glance.
dim_palette <- c(
  "Economic"         = "#FFF0D9",
  "Technological"    = "#DDE8F5",
  "Socio-cultural"   = "#EDE8F5",
  "Institutional"    = "#F5DDE8",
  "Transition pace"  = "#DCDCDC"
)

dec_end <- seq(DEC_FIRST_START + 10, 2100, by = 10)
need    <- function(ids) is.null(SELECTED_INDICATORS) || any(SELECTED_INDICATORS %in% ids)

r10_regions <- c(
  "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
  "Latin America (R10)", "Middle East (R10)", "North America (R10)",
  "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)"
)

# Regions over which indicator 5.1 is evaluated (see PC_EXCLUDE above).
PC_REGIONS <- setdiff(r10_regions, PC_EXCLUDE)

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

# Dimension strip colours.
# The first four are the Brutschin et al. dimensions. "Transition pace"
# is not one of theirs, so it gets a neutral grey strip to keep the
# distinction visible at a glance.
dim_palette <- c(
  "Economic"         = "#FFF0D9",
  "Technological"    = "#DDE8F5",
  "Socio-cultural"   = "#EDE8F5",
  "Institutional"    = "#F5DDE8",
  "Transition pace"  = "#DCDCDC"
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

if (need(c("3.1","3.2"))) run_python_download(elec_vars, "bru_elec_r10", r10_regions)
if (need("4.1"))          run_python_download(fe_vars,   "bru_fe_r10", r10_regions)
run_python_download(reg_vars, "bru_reg", r10_regions)

elec_raw <- if (need(c("3.1","3.2"))) load_pq("bru_elec_r10") else NULL
fe_raw   <- if (need("4.1"))                load_pq("bru_fe_r10") else NULL
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
# CLUSTER HELPERS  (temporary all-cluster build)
# ══════════════════════════════════════════════════════════════
cl_regions <- function(cl) names(PC_CLUSTER_MAP)[PC_CLUSTER_MAP == cl]
ALL_CLUSTERS <- c("Transition Leaders", "Diversifying Economies", "Fossil-Dependent")
CL_CODE <- c("Transition Leaders" = "T", "Diversifying Economies" = "D",
             "Fossil-Dependent"   = "F")

# Sum a variable over the R10 regions of one cluster.
cl_sum <- function(df, mod, scen, vars, cl) {
  regs <- cl_regions(cl)
  m <- sapply(regs, function(r) get_series(df, mod, scen, vars, region = r))
  v <- rowSums(m, na.rm = TRUE)
  v[apply(m, 1, function(x) all(is.na(x)))] <- NA_real_
  setNames(v, YEARS)
}

# Share of one variable in another, both summed over the cluster.
cl_share <- function(df, mod, scen, num_vars, den_vars, cl) {
  n <- cl_sum(df, mod, scen, num_vars, cl)
  d <- cl_sum(df, mod, scen, den_vars, cl)
  out <- 100 * n / d
  out[!is.na(d) & d <= 0] <- NA_real_
  out
}

# Emissions-weighted carbon price for one cluster, as in compute_carbon_price_r10.
cl_price <- function(mod, scen, cl) {
  regs <- cl_regions(cl)
  ps <- sapply(regs, function(r) get_series(cp_raw,  mod, scen, "Price|Carbon",  region = r))
  es <- sapply(regs, function(r) get_series(reg_raw, mod, scen, "Emissions|CO2", region = r))
  es <- pmax(es, 0)            # negative emissions are not weights
  es[is.na(ps)] <- NA_real_    # weight only where a price is reported
  num <- rowSums(ps * es, na.rm = TRUE)
  den <- rowSums(es,      na.rm = TRUE)
  v <- num / den
  v[!is.finite(v) | den <= 0] <- NA_real_
  setNames(v, YEARS)
}

# Worst (largest) value across clusters per decade, returning the value and the
# two-letter code of the cluster it came from.
worst_cluster <- function(fn_cl, clusters = ALL_CLUSTERS, mask = NULL) {
  nd <- length(dec_end)
  vs <- sapply(clusters, function(cl) {
    x <- fn_cl(cl)
    if (!is.null(mask)) x[!mask(cl)] <- NA_real_
    x
  })
  vs <- matrix(vs, nrow = nd)
  v  <- rep(NA_real_, nd); tg <- rep(NA_character_, nd)
  for (i in seq_len(nd)) {
    ok <- which(!is.na(vs[i, ]))
    if (!length(ok)) next
    j <- ok[which.max(vs[i, ok])]
    v[i] <- vs[i, j]; tg[i] <- unname(CL_CODE[clusters[j]])
  }
  list(value = v, tag = tg)
}

gov_eligible <- function(cl) sapply(dec_end, function(y) {
  g <- gov_at(cl, y); !is.na(g) && g < PC_GOV_THR
})

# ══════════════════════════════════════════════════════════════
# 3. INDICATORS  (same as fig3_dot.R)
# ══════════════════════════════════════════════════════════════
indicators <- list(

  # 2.1 Carbon price growth, emissions-weighted cluster price.
  # Growth measured against a floor of CP_FLOOR on the starting price.
  # High concern on the rate of increase alone: no level condition.
  list(id = "2.1", dim = "Economic", name = "Carbon price growth",
       unit = "\u00d7 per decade", med = CP_RATIO, high = CP_RATIO,
       fn = function(m, s) {
         if (is.null(cp_raw) || is.null(reg_raw)) return(na_res())
         nd <- length(dec_end)
         rr <- ll <- matrix(NA_real_, nrow = nd, ncol = length(ALL_CLUSTERS))
         for (k in seq_along(ALL_CLUSTERS)) {
           p <- cl_price(m, s, ALL_CLUSTERS[k])
           if (all(is.na(p))) next
           pd <- at(p, dec_end); pprev <- at(p, dec_end - 10)
           ratio <- ifelse(is.na(pprev) | is.na(pd), NA_real_,
                           pd / pmax(pprev, CP_FLOOR))
           rr[, k] <- ratio
           ll[, k] <- ifelse(is.na(ratio), NA_real_,
                      ifelse(ratio >= CP_RATIO, 2, 0))
         }
         v <- rep(NA_real_, nd); l <- rep(NA_integer_, nd)
         tg <- rep(NA_character_, nd)
         for (i in seq_len(nd)) {
           ok <- which(!is.na(rr[i, ]))
           if (!length(ok)) next
           lmax <- max(ll[i, ok]); cand <- ok[ll[i, ok] == lmax]
           j <- cand[which.max(rr[i, cand])]
           v[i] <- rr[i, j]; l[i] <- as.integer(lmax)
           tg[i] <- unname(CL_CODE[ALL_CLUSTERS[j]])
         }
         list(value = v, level = l, tag = tg)
       }),

  list(id = "3.1", dim = "Technological", name = "Wind scale-up",
       unit = "%/decade", med = 10, high = 20,
       fn = function(m, s) {
         if (is.null(elec_raw)) return(na_res())
         w <- worst_cluster(function(cl)
                dec_delta(cl_share(elec_raw, m, s,
                          "Secondary Energy|Electricity|Wind",
                          "Secondary Energy|Electricity", cl)))
         list(value = w$value, tag = w$tag, level = lvl(w$value, 10, 20))
       }),

  list(id = "3.2", dim = "Technological", name = "Solar scale-up",
       unit = "%/decade", med = 10, high = 20,
       fn = function(m, s) {
         if (is.null(elec_raw)) return(na_res())
         w <- worst_cluster(function(cl)
                dec_delta(cl_share(elec_raw, m, s,
                          "Secondary Energy|Electricity|Solar",
                          "Secondary Energy|Electricity", cl)))
         list(value = w$value, tag = w$tag, level = lvl(w$value, 10, 20))
       }),

  list(id = "4.1", dim = "Socio-cultural", name = "Energy demand decline",
       unit = "%/decade", med = 10, high = 20,
       fn = function(m, s) {
         if (is.null(fe_raw)) return(na_res())
         w <- worst_cluster(function(cl)
                -dec_pct_chg(cl_sum(fe_raw, m, s, "Final Energy", cl)))
         list(value = w$value, tag = w$tag, level = lvl(w$value, 10, 20))
       }),

  # 5.1 with the governance screen: only clusters still below PC_GOV_THR in
  # that decade are eligible.
  list(id = "5.1b", dim = "Institutional", name = "Per-capita CO\u2082 decline",
       unit = "%/decade", med = PC_RATE, high = PC_RATE,
       fn = function(m, s) {
         if (is.null(reg_raw)) return(na_res())
         w <- worst_cluster(function(cl) {
           co <- cl_sum(reg_raw, m, s, "Emissions|CO2", cl)
           po <- cl_sum(reg_raw, m, s, "Population",    cl)
           po[!is.na(po) & po == 0] <- NA_real_
           pc     <- setNames(co / po, YEARS)
           pc_srt <- at(pc, dec_end - 10)
           pc_end <- at(pc, dec_end)
           x      <- 100 * (pc_srt - pc_end) / pc_srt
           # A percentage decline is defined only while per-capita emissions
           # stay positive at both ends of the decade. Decades that start at or
           # below zero, or end below zero, are not reported. No arbitrary
           # floor is applied.
           x[is.na(pc_srt) | is.na(pc_end) | pc_srt <= 0 | pc_end < 0] <- NA_real_
           x
         }, clusters = PC_CLUSTERS, mask = gov_eligible)
         list(value = w$value, tag = w$tag, level = lvl(w$value, PC_RATE, PC_RATE))
       }),

  # P1 stays GLOBAL: the 2.23 Gt reference is a world figure.
  list(id = "P1", dim = "Transition pace", name = "Max annual CO\u2082 decline",
       unit = "Gt CO\u2082/yr", med = COVID_DECLINE, high = COVID_DECLINE,
       fn = function(m, s) {
         if (is.null(reg_raw)) return(na_res())
         co_mat <- sapply(r10_regions, function(r)
                     get_series(reg_raw, m, s, "Emissions|CO2", region = r))
         co_w   <- rowSums(co_mat, na.rm = TRUE)
         co_w[co_w == 0] <- NA_real_
         co_gt  <- setNames(co_w / 1000, YEARS)
         slope  <- setNames(c(NA_real_, -diff(co_gt)), YEARS)
         v <- sapply(dec_end, function(y) {
           w <- slope[as.character((y - 9):y)]
           if (all(is.na(w))) NA_real_ else max(w, na.rm = TRUE)
         })
         list(value = v, level = lvl(v, COVID_DECLINE, COVID_DECLINE))
       })
)

names(indicators) <- sapply(indicators, "[[", "id")

# ══════════════════════════════════════════════════════════════
# 4. COMPUTE  (worst decade per model × scenario × indicator)
# ══════════════════════════════════════════════════════════════
extract_mods <- function(df, scen) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  sort(unique(df$model[df$scenario == scen]))
}

run_one <- function(spec, m, s) {
  r <- tryCatch(spec$fn(m, s), error = function(e) {
    warning(spec$id, " / ", m, " / ", s, ": ", conditionMessage(e)); na_res()
  })
  tg <- if (is.null(r$tag)) rep(NA_character_, length(dec_end)) else as.character(r$tag)
  tibble(model = m, scenario = s, id = spec$id, decade = dec_end,
         value = as.numeric(r$value), level = as.integer(r$level), tag = tg)
}

mods_all <- Reduce(union, lapply(plot_scens, function(s)
  Reduce(union, lapply(list(elec_raw, fe_raw, reg_raw, cp_raw), extract_mods, scen = s))
))
mods_all <- filter_latest_remind(mods_all)
mods_all <- sort(mods_all[mods_all != "WITCH 6.0-EU28"])
message("Models (union across scenarios): ", paste(mods_all, collapse = ", "))

all_rows <- list()
for (s in plot_scens)
  for (m in mods_all)
    for (spec in indicators)
      all_rows[[length(all_rows) + 1]] <- run_one(spec, m, s)

long_sm6 <- bind_rows(all_rows)

cell_sm6 <- long_sm6 %>%
  group_by(model, scenario, id) %>%
  group_modify(function(d, key) {
    if (all(is.na(d$value)))
      return(tibble(value = NA_real_, level = NA_integer_, peak_decade = NA_integer_,
                    tag = NA_character_))
    lmax <- max(d$level, na.rm = TRUE)
    cand <- which(d$level == lmax & !is.na(d$value))
    j    <- cand[which.max(d$value[cand])]
    tibble(value = d$value[j], level = lmax, peak_decade = d$decade[j],
           tag = d$tag[j])
  }) %>%
  ungroup()


# ══════════════════════════════════════════════════════════════
# 5. BUILD DOT PLOT
# ══════════════════════════════════════════════════════════════
ind_meta_dot <- tibble(
  id   = sapply(indicators, "[[", "id"),
  dim  = sapply(indicators, "[[", "dim"),
  name = sapply(indicators, "[[", "name"),
  unit = sapply(indicators, "[[", "unit"),
  med  = sapply(indicators, "[[", "med"),
  high = sapply(indicators, "[[", "high")
) %>%
  mutate(
    thr_txt     = case_when(
      id == "P1"  ~ "global",
      id == "2.1"  ~ paste0("H \u2265 ", CP_RATIO, "\u00d7"),
      TRUE        ~ paste0("M \u2265 ", med, " \u00b7 H \u2265 ", high)
    ),
    thr_lab     = if_else(id == "P1",
                          paste0(COVID_DECLINE, " Gt/yr"),
                          paste0("H: ", high)),
    strip_label = paste0(name, "\n(", unit, ")\n", thr_txt),
    strip_label = factor(strip_label, levels = strip_label)
  )

mod_order       <- sort(short_name(mods_all))
scen_lbl_levels <- unname(scen_labels[plot_scens])

dot_df <- cell_sm6 %>%
  left_join(ind_meta_dot, by = "id") %>%
  mutate(
    scenario_label = factor(scen_labels[scenario], levels = scen_lbl_levels),
    model_short    = factor(short_name(model), levels = mod_order),
    fill_col       = case_when(
      is.na(level) ~ "grey70",
      level == 2L  ~ "#E8595A",
      level == 1L  ~ "#F4D03F",
      TRUE         ~ "#5BB85C"
    ),
    val_num   = case_when(
      is.na(value)      ~ NA_character_,
      abs(value) >= 1e3 ~ paste0(round(value / 1000, 1), "k"),
      abs(value) >= 100 ~ as.character(round(value, 0)),
      TRUE              ~ formatC(value, format = "f", digits = 1)
    ),
    val_label = val_num,
    tag_label = if_else(is.na(tag), NA_character_, paste0("(", tag, ")"))
  )

thresh_high_df <- ind_meta_dot %>%
  # On the carbon price panel the markers crowd the left of the 5x line, so
  # that label goes to the right of the panel and below the line.
  mutate(cp = id %in% c("2.1", "2.1b"),
         vj = if_else(cp, 1.45,  -0.45),
         hj = if_else(cp, 1.12,  -0.12),
         xp = if_else(cp,  Inf,   -Inf)) %>%
  select(strip_label, thr_val = high, thr_lab, vj, hj, xp)
thresh_med_df  <- ind_meta_dot %>%
  filter(med != high) %>%                # 2.1 now has two distinct thresholds
  select(strip_label, thr_val = med)

shape_vals <- c(15, 16, 17, 18, 4, 1, 0, 2, 5, 6, 3, 8)
shape_vec  <- setNames(shape_vals[seq_along(mod_order)], mod_order)

# Axis labels carry the model's own marker, so no legend is needed.
shape_glyphs <- c("15" = "■", "16" = "●", "17" = "▲",
                  "18" = "◆", "8"  = "*",      "1"  = "○",
                  "0"  = "□", "2"  = "△", "5"  = "◇",
                  "6"  = "▽", "3"  = "+",      "4"  = "×")
mod_labels <- setNames(
  paste0(shape_glyphs[as.character(shape_vec)], "  ", names(shape_vec)),
  names(shape_vec)
)

# ── Shared y range per indicator column ───────────────────────
# ══════════════════════════════════════════════════════════════
# SHARED Y LIMITS
# Taken from cell_sm6, i.e. the worst-decade values that are actually
# plotted, across all four scenarios, so the two figures share an axis per
# indicator and no unplotted decade can stretch it. Threshold lines are
# folded in so they always stay visible.
# ══════════════════════════════════════════════════════════════
ind_y_lims <- cell_sm6 %>%
  filter(!is.na(value)) %>%
  # off-scale carbon price values are excluded from the axis range
  filter(!(id %in% CP_IDS & value > CP_Y_CAP)) %>%
  group_by(id) %>%
  summarise(ylo = min(value), yhi = max(value), .groups = "drop") %>%
  left_join(ind_meta_dot %>% select(id, strip_label, med, high), by = "id") %>%
  mutate(ylo = pmin(ylo, med, high, na.rm = TRUE),
         yhi = pmax(yhi, med, high, na.rm = TRUE),
         # The headroom for the value labels is added here rather than by
         # scale_y_continuous(expand = ...), because a proportional expansion
         # cannot be stopped at zero on one panel only.
         rng     = pmax(yhi - ylo, 1e-9),
         lo_mult = if_else(id %in% CP_IDS, EXPAND_LO_CP, EXPAND_LO),
         ylo     = ylo - lo_mult * rng,
         yhi     = yhi + EXPAND_HI * rng,
         # carbon price growth is a ratio: that axis does not go below zero
         ylo     = if_else(id %in% CP_IDS, pmax(ylo, 0), ylo),
         yhi     = if_else(id %in% CP_IDS, CP_Y_MAX, yhi)) %>%
  select(id, strip_label, ylo, yhi)

message("y limits per indicator:")
print(as.data.frame(ind_y_lims %>% select(id, ylo, yhi)))

# Clamp the off-scale carbon price markers to just inside the top of the panel
# and record where their arrow starts and ends.
dot_df <- dot_df %>%
  left_join(ind_y_lims %>% select(id, p_lo = ylo, p_hi = yhi), by = "id") %>%
  mutate(
    off_scale = id %in% CP_IDS & !is.na(value) & value > CP_Y_CAP,
    p_rng     = p_hi - p_lo,
    y_plot    = if_else(off_scale, p_lo + 0.86 * p_rng, value),
    y_arr0    = y_plot + 0.03 * p_rng,
    y_arr1    = y_plot + 0.11 * p_rng
  )

over <- dot_df %>% filter(id %in% CP_IDS, !is.na(value), value > CP_Y_MAX)
if (nrow(over) > 0) {
  message("WARNING: carbon price values above CP_Y_MAX, clipped off the panel:")
  print(as.data.frame(over %>% select(model, scenario, tag, value)))
}

if (any(dot_df$off_scale, na.rm = TRUE)) {
  message("off-scale carbon price values drawn with an arrow:")
  print(as.data.frame(dot_df %>% filter(off_scale) %>%
                      select(model, scenario, tag, value)))
}
library(grid)
library(gtable)
# ══════════════════════════════════════════════════════════════
# BUILD AND SAVE  (one function, two figures, shared y limits)
# ══════════════════════════════════════════════════════════════
build_fig <- function(scens, out_png, height, n_row = 1, fs = 1) {
  # n_row: panel rows (Fig. 4 uses 2 so each panel is wider)
  # fs:    font scale applied to every text size in the plot

  dd <- dot_df %>% filter(scenario %in% scens) %>% droplevels()
  one <- length(scens) == 1

  bl <- ind_y_lims %>%
    select(strip_label, ylo, yhi) %>%
    pivot_longer(c(ylo, yhi), names_to = ".drop", values_to = "value") %>%
    select(strip_label, value) %>%
    mutate(model_short = factor(levels(dd$model_short)[1],
                                levels = levels(dd$model_short)))
  if (!one) bl <- crossing(bl, scenario_label = levels(dd$scenario_label))

  p <- ggplot(dd, aes(x = model_short, y = y_plot)) +
    geom_blank(data = bl, aes(x = model_short, y = value), inherit.aes = FALSE) +
    geom_hline(data = thresh_med_df, aes(yintercept = thr_val),
               linetype = "dashed", linewidth = 0.65, colour = "#C8A400",
               inherit.aes = FALSE) +
    geom_text(data = thresh_med_df,
              aes(x = -Inf, y = thr_val, label = paste0("M: ", thr_val)),
              hjust = -0.12, vjust = 1.5, size = 5.6 * fs,
              colour = "#8B6914", fontface = "italic", inherit.aes = FALSE) +
    geom_hline(data = thresh_high_df, aes(yintercept = thr_val),
               linetype = "dotted", linewidth = 0.85, colour = "#C94040",
               inherit.aes = FALSE) +
    geom_text(data = thresh_high_df,
              aes(x = xp, y = thr_val, label = thr_lab,
                  vjust = vj, hjust = hj),
              size = 5.6 * fs,
              colour = "#C94040", fontface = "italic", inherit.aes = FALSE) +
    geom_point(aes(colour = fill_col, shape = model_short),
               size = 5.0 * fs, stroke = 0.65, na.rm = TRUE) +
    # value above CP_Y_CAP: marker clamped, arrow pointing off the panel
    geom_segment(data = filter(dd, off_scale),
                 aes(x = model_short, xend = model_short,
                     y = y_arr0, yend = y_arr1, colour = fill_col),
                 arrow = grid::arrow(length = unit(0.20, "cm"),
                                     type = "closed"),
                 linewidth = 0.9, inherit.aes = FALSE) +
    # number right-aligned on the marker, cluster letter left-aligned just
    # after it and in a smaller size
    geom_text(data = filter(dd, !is.na(value)),
              aes(label = val_label, colour = fill_col,
                  vjust = if_else(off_scale, 1.8,
                          if_else(level >= 1L, -0.7, 1.8))),
              hjust = 1, size = 5.4 * fs, fontface = "bold") +
    geom_text(data = filter(dd, !is.na(value), !is.na(tag_label)),
              aes(label = tag_label, colour = fill_col,
                  vjust = if_else(off_scale, 2.3,
                          if_else(level >= 1L, -0.9, 2.3))),
              hjust = 0, size = 4.0 * fs, fontface = "bold") +
    scale_colour_identity() +
    scale_shape_manual(values = shape_vec, guide = "none") +
    # GEM-E3 sits at the panel edge and its value label is right-aligned on the
    # marker, so the x scale needs extra room on the left.
    scale_x_discrete(labels = mod_labels,
                     expand = expansion(add = c(1.0, 0.75))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = "Value (worst decade)") +
    theme_bw(base_size = 18 * fs, base_family = "Arial") +
    theme(
      axis.text.x        = element_text(size = 15 * fs, angle = 35, hjust = 1),
      axis.text.y        = element_text(size = 15 * fs),
      strip.text.x       = element_text(size = 15 * fs, face = "bold", lineheight = 1.15),
      strip.text.y       = element_text(size = 17 * fs, face = "bold", angle = 0,
                                        hjust = 0, lineheight = 1.1),
      strip.background   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing.x    = unit(SPACING_X, "lines"),
      legend.position    = "none"
    )

  p <- if (one) p + facet_wrap(~ strip_label, nrow = n_row, scales = "free_y")
       else     p + facet_grid2(scenario_label ~ strip_label,
                                scales = "free", independent = "y")

  g <- ggplotGrob(p)

  # colour the column strips by dimension
  sd <- ind_meta_dot$dim[match(levels(dd$strip_label),
                               as.character(ind_meta_dot$strip_label))]
  sf <- dim_palette[sd]
  idx <- grep("strip-t", g$layout$name)
  for (i in seq_along(idx)) {
    fc <- sf[i]; if (is.na(fc)) next
    pg <- g$grobs[[idx[i]]]
    ri <- grep("rect", names(pg$grobs[[1]]$children))
    if (length(ri) > 0) pg$grobs[[1]]$children[[ri]]$gp$fill <- fc
    g$grobs[[idx[i]]] <- pg
  }

  # Divider before the panel that is not a Brutschin indicator. It only works
  # on a single row of panels; with n_row > 1 the grey strip identifies it.
  pan <- g$layout[grepl("^panel", g$layout$name), ]
  if (nrow(pan) >= 2 && n_row == 1) {
    cols    <- sort(unique(pan$l))
    gap_col <- cols[length(cols)] - 1
    # the panel outside the Brutschin set keeps a wide gap; the divider sits
    # in the middle of it
    g$widths[gap_col] <- unit(SPACING_LAST, "lines")
    st      <- g$layout$t[grepl("^strip-t", g$layout$name)]
    g <- gtable::gtable_add_grob(
      g,
      grid::linesGrob(x = unit(c(0.3, 0.3), "npc"), y = unit(c(0, 1), "npc"),
                      gp = grid::gpar(col = "grey35", lwd = 1.8)),
      t = if (length(st)) min(st) else min(pan$t), b = max(pan$b),
      l = gap_col, r = gap_col, z = Inf, clip = "off", name = "pace-divider")
  }

  # widen the first and last panel columns: their value labels were running off
  if (nrow(pan) >= 2) {
    cols_all <- sort(unique(pan$l))
    g$widths[cols_all[1]]   <- g$widths[cols_all[1]]   * CP_WIDEN
    g$widths[max(cols_all)] <- g$widths[max(cols_all)] * P1_WIDEN
  }

  out <- file.path(graphs_dir, out_png)
  n_col <- ceiling(nrow(ind_meta_dot) / n_row)
  ggsave(out, g,
         width  = PANEL_W * fs * n_col + 1.2 +
                  PANEL_W * ((P1_WIDEN - 1) + (CP_WIDEN - 1)),
         height = height, dpi = 300)
  message("Saved: ", out)
}

build_fig(FIG4_SCEN, "fig4_clusters.png", height = 6.0)

build_fig(SM9_SCENS, "fig_sm9_clusters.png",
          height = 4.2 * length(SM9_SCENS) + 1.4)
