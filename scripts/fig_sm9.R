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
CP_DISCOUNT   <- 0.02   # matches the 2% used for the GDP NPV elsewhere
                        # (Brutschin SM 3.2 specify 5%; deviation to keep one
                        # discount rate across the paper)

plot_scens <- c(
  "SSP2 - Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back",
  "SSP2 - Staying Alive"
)
scen_labels <- c(
  "SSP2 - Meet Aspirations"     = "(a) Meet Aspirations",
  "SSP2 - Asymmetric Roll-Back" = "(b) Shifted Ambition",
  "SSP2 - Staying Alive"        = "(c) Tech Momentum"
)

# TEMPORARY COMPARISON BUILD.
#   2.1  carbon price growth, undiscounted level condition
#   2.1b carbon price growth, level condition on NPV at 5% to 2020
#   5.1  per-capita CO2 decline, fixed screen (EU / Pac OECD / N. America out)
#   5.1b per-capita CO2 decline, time-varying governance screen (Andrijevic)
# Drop 2.1b and 5.1b once the comparison is settled.
SELECTED_INDICATORS <- c("2.1", "3.1", "3.2", "4.1", "5.1b", "P1")

# Largest single-year fall in global CO2 on record: 2020 pandemic shutdown.
COVID_DECLINE <- 2.23

# Brutschin et al. Table 1, indicator 2.1: a fivefold rise per decade.
# Check SM section 3 for the currency year and whether the level is discounted.
CP_RATIO     <- 5
# Denominator floor: growth is measured against at least this price, so that a
# near-zero starting price does not produce an arbitrarily large ratio.
CP_FLOOR     <- 10
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
  v <- gov_cl$governance[gov_cl$cluster == cl &
                         gov_cl$year == min(max(y, 2020), 2100)]
  if (length(v) == 0) NA_real_ else v[1]
}

dec_end <- seq(2030, 2100, by = 10)
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

dec_end <- seq(2030, 2100, by = 10)
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
# 3. INDICATORS  (same as fig3_dot.R)
# ══════════════════════════════════════════════════════════════
indicators <- list(

  # Brutschin et al. Table 1, indicator 2.1. The plotted value is the decadal
  # growth ratio; high concern additionally requires the price level to reach
  # CP_MIN_LEVEL, which is what screens out large ratios from a near-zero base.
  list(id = "2.1", dim = "Economic", name = "Carbon price growth",
       unit = "\u00d7 per decade", med = CP_RATIO, high = CP_RATIO,
       fn = function(m, s) {
         if (is.null(cp_raw)) return(na_res())
         p <- get_series(cp_raw, m, s, "Price|Carbon", region = "World")
         if (all(is.na(p))) {
           ps <- lapply(r10_regions, function(r) get_series(cp_raw, m, s, "Price|Carbon", region = r))
           if (all(sapply(ps, function(x) all(is.na(x))))) return(na_res())
           p <- do.call(pmax, c(ps, na.rm = TRUE)); names(p) <- YEARS
         }
         # Growth only. Brutschin pair the 5x decadal rise with a price level
         # of 120$ in NPV; that level condition is not applied here, so the
         # panel reports the rate of increase and nothing about the level.
         pd    <- at(p, dec_end)
         pprev <- at(p, dec_end - 10)
         ratio <- ifelse(is.na(pprev) | is.na(pd), NA_real_,
                         pd / pmax(pprev, CP_FLOOR))
         level <- ifelse(is.na(ratio), NA_integer_,
                         ifelse(ratio >= CP_RATIO, 2L, 0L))
         list(value = ratio, level = level)
       }),

  simple_ind("3.1", "Technological", "Wind scale-up", "%/decade", 10, 20,
    function(m, s) dec_delta(share_series(elec_raw, m, s,
      "Secondary Energy|Electricity|Wind", "Secondary Energy|Electricity"))),

  simple_ind("3.2", "Technological", "Solar scale-up", "%/decade", 10, 20,
    function(m, s) dec_delta(share_series(elec_raw, m, s,
      "Secondary Energy|Electricity|Solar", "Secondary Energy|Electricity"))),

  simple_ind("4.1", "Socio-cultural", "Energy demand decline", "%/decade", 10, 20,
    function(m, s) -dec_pct_chg(get_series(fe_raw, m, s, "Final Energy"))),

  # TEMPORARY: 5.1 with the governance condition applied as published, per
  # region and per decade, from the Andrijevic SSP2 projections. In each decade
  # only regions with governance < PC_GOV_THR are eligible, and the value is the
  # worst decline among them.
  list(id = "5.1b", dim = "Institutional",
       name = "Per-cap reg. CO₂ decl.",
       unit = "%/decade", med = PC_RATE, high = PC_RATE,
       fn = function(m, s) {
         if (is.null(reg_raw)) return(na_res())
         nd <- length(dec_end)
         vs <- sapply(PC_CLUSTERS, function(cl) {
           regs <- names(PC_CLUSTER_MAP)[PC_CLUSTER_MAP == cl]
           co <- rowSums(sapply(regs, function(r)
                   get_series(reg_raw, m, s, "Emissions|CO2", region = r)), na.rm = TRUE)
           po <- rowSums(sapply(regs, function(r)
                   get_series(reg_raw, m, s, "Population", region = r)), na.rm = TRUE)
           co[co == 0] <- NA_real_; po[po == 0] <- NA_real_
           pc     <- setNames(co / po, YEARS)
           pc_srt <- at(pc, dec_end - 10)
           x      <- 100 * (pc_srt - at(pc, dec_end)) / pc_srt
           x[is.na(pc_srt) | pc_srt < 0.3] <- NA_real_
           # eligible only in the decades where this cluster is still projected
           # below the governance threshold
           elig <- sapply(dec_end, function(y) {
             g <- gov_at(cl, y); !is.na(g) && g < PC_GOV_THR
           })
           x[!elig] <- NA_real_
           x
         })
         vs <- matrix(vs, nrow = nd)
         v  <- rep(NA_real_, nd)
         for (i in seq_len(nd)) {
           ok <- which(!is.na(vs[i, ]))
           if (length(ok) == 0) next
           v[i] <- max(vs[i, ok])
         }
         list(value = v, level = lvl(v, PC_RATE, PC_RATE))
       }),

  # ── Not a Brutschin et al. indicator ────────────────────────
  # Steepest annualised fall in global CO2 within each decade.
  # World CO2 is the sum over the R10 regions, as in 5.1 above.
  # Reference level: the 2020 pandemic fall, the largest on record.
  list(id = "P1", dim = "Transition pace", name = "Max annual CO₂ decline",
       unit = "Gt CO₂/yr", med = COVID_DECLINE, high = COVID_DECLINE,
       fn = function(m, s) {
         if (is.null(reg_raw)) return(na_res())
         co_mat <- sapply(r10_regions, function(r) get_series(reg_raw, m, s, "Emissions|CO2", region = r))
         co_w   <- rowSums(co_mat, na.rm = TRUE)
         co_w[co_w == 0] <- NA_real_
         co_gt  <- setNames(co_w / 1000, YEARS)          # Mt -> Gt
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
      id == "P1"  ~ "",
      id == "2.1"  ~ paste0("H \u2265 ", CP_RATIO, "\u00d7"),
      id == "5.1b" ~ paste0("H \u2265 ", PC_RATE, " \u00b7 gov < ", PC_GOV_THR),
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
    val_label = if_else(is.na(val_num), NA_character_,
                        if_else(is.na(tag), val_num, paste0(val_num, "\n", tag)))
  )

thresh_high_df <- ind_meta_dot %>% select(strip_label, thr_val = high, thr_lab)
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
ind_y_lims <- bind_rows(
  dot_df         %>% filter(!is.na(value)) %>% select(strip_label, value),
  thresh_high_df %>% select(strip_label, value = thr_val),
  thresh_med_df  %>% select(strip_label, value = thr_val)
) %>%
  group_by(strip_label) %>%
  summarise(ylo = min(value, na.rm = TRUE),
            yhi = max(value, na.rm = TRUE),
            .groups = "drop")

blank_df <- crossing(ind_y_lims,
                     scenario_label = levels(dot_df$scenario_label)) %>%
  pivot_longer(c(ylo, yhi), names_to = ".drop", values_to = "value") %>%
  select(strip_label, scenario_label, value) %>%
  mutate(model_short = factor(levels(dot_df$model_short)[1],
                              levels = levels(dot_df$model_short)))

p_dot <- ggplot(dot_df, aes(x = model_short, y = value)) +

  geom_blank(data = blank_df,
             aes(x = model_short, y = value),
             inherit.aes = FALSE) +

  geom_hline(data = thresh_med_df,
             aes(yintercept = thr_val),
             linetype = "dashed", linewidth = 0.65, colour = "#C8A400",
             inherit.aes = FALSE) +
  geom_text(data = thresh_med_df,
            aes(x = -Inf, y = thr_val, label = paste0("M: ", thr_val)),
            hjust = -0.12, vjust = 1.5, size = 5.0,
            colour = "#8B6914", fontface = "italic",
            inherit.aes = FALSE) +

  geom_hline(data = thresh_high_df,
             aes(yintercept = thr_val),
             linetype = "dotted", linewidth = 0.85, colour = "#C94040",
             inherit.aes = FALSE) +
  geom_text(data = thresh_high_df,
            aes(x = -Inf, y = thr_val, label = thr_lab),
            hjust = -0.12, vjust = -0.45, size = 5.0,
            colour = "#C94040", fontface = "italic",
            inherit.aes = FALSE) +

  geom_point(aes(colour = fill_col, shape = model_short),
             size = 4.5, stroke = 0.65, na.rm = TRUE) +

  geom_text(data = filter(dot_df, !is.na(value)),
            aes(label  = val_label,
                colour = fill_col,
                vjust  = if_else(level >= 1L, -0.7, 1.8)),
            size = 5.5, fontface = "bold") +

  facet_grid2(scenario_label ~ strip_label, scales = "free", independent = "y") +
  scale_colour_identity() +
  scale_shape_manual(values = shape_vec, guide = "none") +
  scale_x_discrete(labels = mod_labels) +
  scale_y_continuous(expand = expansion(mult = c(0.28, 0.24))) +
  labs(
    title = paste0("Feasibility indicators for three PRISMA scenarios: Brutschin et al. ",
                   "(2021, Table 1) on the left, maximum annual CO₂ decline on the right"),
    x     = NULL,
    y     = "Value (worst decade)"
  ) +
  theme_bw(base_size = 16, base_family = "Arial") +
  theme(
    plot.title         = element_text(size = 17, face = "bold"),
    axis.text.x        = element_text(size = 13, angle = 35, hjust = 1),
    axis.text.y        = element_text(size = 13),
    strip.text.x       = element_text(size = 13, face = "bold", lineheight = 1.15),
    strip.text.y       = element_text(size = 15, face = "bold", angle = 0, hjust = 0),
    strip.background   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x    = unit(1.6, "lines"),   # room for the divider
    legend.position    = "none"
  )

# ── Colour column strip backgrounds by dimension ───────────────
library(grid)
library(gtable)

strip_dims  <- ind_meta_dot$dim[match(levels(dot_df$strip_label),
                                       as.character(ind_meta_dot$strip_label))]
strip_fills <- dim_palette[strip_dims]

grob      <- ggplotGrob(p_dot)
strip_idx <- grep("strip-t-", grob$layout$name)

for (i in seq_along(strip_idx)) {
  fill_col   <- strip_fills[i]
  if (is.na(fill_col)) next
  panel_grob <- grob$grobs[[strip_idx[i]]]
  rect_idx   <- grep("rect", names(panel_grob$grobs[[1]]$children))
  if (length(rect_idx) > 0)
    panel_grob$grobs[[1]]$children[[rect_idx]]$gp$fill <- fill_col
  grob$grobs[[strip_idx[i]]] <- panel_grob
}

# ── Divider before the column that is not a Brutschin indicator ─
local({
  pan <- grob$layout[grepl("^panel", grob$layout$name), ]
  if (nrow(pan) < 2) return(invisible(NULL))
  # The column immediately left of the last panel holds its axis. Drawing
  # at x = 0 of that column puts the line on the boundary with the panel
  # spacer, so the widened spacing shows as clear air to its left.
  cols    <- sort(unique(pan$l))
  gap_col <- cols[length(cols)] - 1
  strip_t <- grob$layout$t[grepl("^strip-t", grob$layout$name)]
  top_row <- if (length(strip_t)) min(strip_t) else min(pan$t)
  grob <<- gtable::gtable_add_grob(
    grob,
    grid::linesGrob(x = unit(c(-0.3, -0.3), "npc"), y = unit(c(0, 1), "npc"),
                    gp = grid::gpar(col = "grey35", lwd = 1.8)),
    t = top_row, b = max(pan$b), l = gap_col, r = gap_col,
    z = Inf, clip = "off", name = "pace-divider"
  )
})

n_inds   <- nrow(ind_meta_dot)
n_scens  <- length(plot_scens)
out_file <- file.path(graphs_dir, "fig_sm9.png")
ggsave(out_file, grob,
       width  = 2.9 * n_inds + 1.2,
       height = 4.2 * n_scens + 2.0,
       dpi    = 300)
message("Saved: ", out_file)

# ── Update captions ────────────────────────────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig_sm9", graphs_dir)
  }
})
