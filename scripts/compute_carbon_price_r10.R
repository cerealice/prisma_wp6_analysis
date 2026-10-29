library(tidyverse)
library(arrow)
library(jsonlite)
library(writexl)

# ============================================================
# PURPOSE
# Download Price|Carbon and Emissions|CO2 at native model
# resolution (no region filter), then aggregate to R10 using
# CO2-weighted mean. Saves data/carbon_price_weighted_r10.xlsx
#
# Run this script ONCE or let carbon_price_loader.R call it
# automatically. It re-aggregates only when the underlying
# parquet files are newer than the cached Excel — so running
# it on every figure render is cheap once the Excel is fresh.
#
# To update the native→R10 region mapping after a first run,
# look for "UNMAPPED REGIONS" in the console output and add
# entries to the `native_to_r10` tibble below.
# ============================================================

# ── Paths ─────────────────────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
outdir      <- file.path(project_dir, "data", "direct")
data_dir    <- file.path(project_dir, "data")
xl_path     <- file.path(data_dir, "carbon_price_weighted_r10.xlsx")

dir.create(outdir,   recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# ── Scenarios ─────────────────────────────────────────────────────────────────
scenarios <- c(
  "SSP2 - Late Reawakening",
  "SSP2 - Meet Aspirations",
  "SSP2 - Staying Alive",
  "SSP2 - Asymmetric Roll-Back"
)

# ── Python executable ─────────────────────────────────────────────────────────
miniconda_envs        <- file.path(Sys.getenv("USERPROFILE"), "miniconda3", "envs")
env_dirs              <- list.dirs(miniconda_envs, recursive = FALSE)
python_exe_candidates <- Filter(file.exists, file.path(env_dirs, "python.exe"))
if (length(python_exe_candidates) == 0) stop("No python.exe found under ", miniconda_envs)
python_exe <- python_exe_candidates[1]
message("Using Python: ", python_exe)

# ── Download helper: NO region filter → all native model regions ───────────────
run_download_native <- function(variable, prefix) {
  existing <- list.files(outdir, pattern = paste0("^", prefix, "_.*\\.parquet$"),
                         full.names = TRUE)
  existing <- existing[!grepl("_meta\\.parquet$", existing)]
  if (length(existing) > 0) {
    latest <- existing[which.max(file.info(existing)$mtime)]
    if (as.Date(file.info(latest)$mtime) == Sys.Date()) {
      message("Skipping '", prefix, "' \u2014 today's parquet exists: ", basename(latest))
      return(invisible(NULL))
    }
  }
  message("Downloading '", prefix, "' at native model resolution (all regions) ...")
  py_code <- paste0(
    "import sys\n",
    "sys.path.insert(0, r'''", scripts_dir, "''')\n",
    "from download_prisma import download_prisma\n",
    "import json\n",
    "download_prisma(\n",
    "    variables=json.loads(r'''", toJSON(variable, auto_unbox = TRUE), "'''),\n",
    "    scenarios=json.loads(r'''", toJSON(scenarios, auto_unbox = TRUE), "'''),\n",
    "    prefix='", prefix, "',\n",
    "    outdir=r'''", normalizePath(outdir, winslash = "/", mustWork = FALSE), "'''\n",
    ")\n"
  )
  tmp <- tempfile(fileext = ".py")
  writeLines(py_code, tmp)
  status <- system2(python_exe, tmp)
  if (status != 0) { message("Retrying ..."); status <- system2(python_exe, tmp) }
  if (status != 0) warning("Download of '", prefix, "' failed (status ", status, ")")
  else             message("Download of '", prefix, "' completed.")
}

load_latest <- function(prefix, folder = outdir) {
  files <- list.files(folder, pattern = paste0("^", prefix, "_.*\\.parquet$"),
                      full.names = TRUE)
  files <- files[!grepl("_meta\\.parquet$", files)]
  if (length(files) == 0) stop("No parquet found for prefix: ", prefix)
  latest <- files[which.max(file.info(files)$mtime)]
  message("Loading: ", basename(latest))
  read_parquet(latest)
}

# ── Model filter: exclude EU28 variant + keep only latest REMIND ──────────────
filter_models <- function(m) {
  m <- m[!m %in% c("WITCH 6.0-EU28")]
  remind_models <- m[grepl("^REMIND", m)]
  if (length(remind_models) > 1) {
    versions <- regmatches(remind_models, regexpr("[0-9]+\\.[0-9]+", remind_models))
    latest   <- remind_models[order(numeric_version(versions), decreasing = TRUE)[1]]
    m        <- m[!grepl("^REMIND", m) | m == latest]
    message("Multiple REMIND versions found \u2014 keeping: ", latest)
  }
  m
}

# ── Download ───────────────────────────────────────────────────────────────────
run_download_native("Price|Carbon",  "cp_native")
run_download_native("Emissions|CO2", "co2_native")

# ── Freshness guard: skip aggregation if Excel is newer than both parquets ────
.cp_files  <- list.files(outdir, "^cp_native_.*\\.parquet$",  full.names = TRUE)
.co2_files <- list.files(outdir, "^co2_native_.*\\.parquet$", full.names = TRUE)
.cp_files  <- .cp_files[!grepl("_meta\\.parquet$",  .cp_files)]
.co2_files <- .co2_files[!grepl("_meta\\.parquet$", .co2_files)]

.needs_recompute <- TRUE
if (file.exists(xl_path) && length(.cp_files) > 0 && length(.co2_files) > 0) {
  .xl_mtime  <- file.info(xl_path)$mtime
  .cp_new    <- max(file.info(.cp_files)$mtime)  > .xl_mtime
  .co2_new   <- max(file.info(.co2_files)$mtime) > .xl_mtime
  if (!.cp_new && !.co2_new) {
    message("carbon_price_weighted_r10.xlsx is up to date \u2014 skipping aggregation.")
    .needs_recompute <- FALSE
  } else {
    message("Parquet data is newer than Excel \u2014 recomputing carbon price aggregation ...")
  }
}
rm(.cp_files, .co2_files)

if (.needs_recompute) {

# ── Native region → R10 mapping ───────────────────────────────────────────────
# Add rows here whenever the console prints "UNMAPPED REGIONS".
# World is excluded (NA) — it is recomputed from R10 values.
native_to_r10 <- tribble(
  ~native,                                                    ~r10,

  # ── R10 pass-through (models that already report at R10) ──────────────────
  "Africa (R10)",              "Africa (R10)",
  "China+ (R10)",              "China+ (R10)",
  "Europe (R10)",              "Europe (R10)",
  "India+ (R10)",              "India+ (R10)",
  "Latin America (R10)",       "Latin America (R10)",
  "Middle East (R10)",         "Middle East (R10)",
  "North America (R10)",       "North America (R10)",
  "Other (R10)",               "Other (R10)",
  "Pacific OECD (R10)",        "Pacific OECD (R10)",
  "Reforming Economies (R10)", "Reforming Economies (R10)",
  "Rest of Asia (R10)",        "Rest of Asia (R10)",

  # ── World / global aggregates → excluded ────────────────────────────────────
  "World",     NA_character_,
  "WORLD",     NA_character_,
  "Global",    NA_character_,

  # ── REMIND-MAgPIE (3-letter codes — older versions) ─────────────────────────
  "AFR",  "Africa (R10)",
  "CAZ",  "Pacific OECD (R10)",
  "CHA",  "China+ (R10)",
  "EUR",  "Europe (R10)",
  "IND",  "India+ (R10)",
  "JPN",  "Pacific OECD (R10)",
  "LAM",  "Latin America (R10)",
  "MEA",  "Middle East (R10)",
  "NEU",  "Europe (R10)",
  "OAS",  "Rest of Asia (R10)",
  "REF",  "Reforming Economies (R10)",
  "USA",  "North America (R10)",

  # ── REMIND-MAgPIE 3.7-4.14 (pipe-format native regions) ─────────────────────
  "REMIND-MAgPIE 3.7-4.14|Canada, Australia, New Zealand", "Pacific OECD (R10)",
  "REMIND-MAgPIE 3.7-4.14|China and Taiwan",               "China+ (R10)",
  "REMIND-MAgPIE 3.7-4.14|EU 28",                          "Europe (R10)",
  "REMIND-MAgPIE 3.7-4.14|India",                          "India+ (R10)",
  "REMIND-MAgPIE 3.7-4.14|Japan",                          "Pacific OECD (R10)",
  "REMIND-MAgPIE 3.7-4.14|Latin America and the Caribbean","Latin America (R10)",
  "REMIND-MAgPIE 3.7-4.14|Middle East and North Africa",   "Middle East (R10)",
  "REMIND-MAgPIE 3.7-4.14|Non-EU28 Europe",                "Europe (R10)",
  "REMIND-MAgPIE 3.7-4.14|Other Asia",                     "Rest of Asia (R10)",
  "REMIND-MAgPIE 3.7-4.14|Russia and Reforming Economies", "Reforming Economies (R10)",
  "REMIND-MAgPIE 3.7-4.14|Sub-Saharan Africa",             "Africa (R10)",
  "REMIND-MAgPIE 3.7-4.14|United States of America",       "North America (R10)",

  # ── WITCH 6.0 (pipe-format native regions) ───────────────────────────────────
  "WITCH 6.0|Australia, New Zealand, and Oceania islands", "Pacific OECD (R10)",
  "WITCH 6.0|Brazil",                                      "Latin America (R10)",
  "WITCH 6.0|Canada",                                      "North America (R10)",
  "WITCH 6.0|China",                                       "China+ (R10)",
  "WITCH 6.0|Europe",                                      "Europe (R10)",
  "WITCH 6.0|India",                                       "India+ (R10)",
  "WITCH 6.0|Indonesia",                                   "Rest of Asia (R10)",
  "WITCH 6.0|Japan and Korea",                             "Pacific OECD (R10)",
  "WITCH 6.0|Latin America and the Caribbean",             "Latin America (R10)",
  "WITCH 6.0|Mexico",                                      "Latin America (R10)",
  "WITCH 6.0|Middle East and North Africa",                "Middle East (R10)",
  "WITCH 6.0|Non-EU Eastern European and Transition Countries", "Reforming Economies (R10)",
  "WITCH 6.0|South Africa",                                "Africa (R10)",
  "WITCH 6.0|South Asia",                                  "India+ (R10)",
  "WITCH 6.0|South East Asia",                             "Rest of Asia (R10)",
  "WITCH 6.0|Sub-Saharan Africa",                          "Africa (R10)",
  "WITCH 6.0|United States",                               "North America (R10)",

  # ── MESSAGEix-GLOBIOM-GAINS (R12 short codes — older versions) ──────────────
  "R12_AFR",     "Africa (R10)",
  "R12_CHN",     "China+ (R10)",
  "R12_CPA",     "China+ (R10)",
  "R12_EEU",     "Reforming Economies (R10)",
  "R12_EEU_CPA", "Reforming Economies (R10)",
  "R12_FSU",     "Reforming Economies (R10)",
  "R12_LAM",     "Latin America (R10)",
  "R12_MEA",     "Middle East (R10)",
  "R12_NAM",     "North America (R10)",
  "R12_PAS",     "Rest of Asia (R10)",
  "R12_PAO",     "Pacific OECD (R10)",
  "R12_SAS",     "India+ (R10)",
  "R12_WEU",     "Europe (R10)",
  "R12_WAM",     "Latin America (R10)",

  # ── MESSAGEix-GLOBIOM-GAINS 2.1-M-R12 (pipe-format native regions) ───────────
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|China",                          "China+ (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Eastern Europe",                 "Reforming Economies (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Former Soviet Union",            "Reforming Economies (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Latin America and the Caribbean","Latin America (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Middle East and North Africa",   "Middle East (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|North America",                  "North America (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Other Pacific Asia",             "Rest of Asia (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Pacific OECD",                   "Pacific OECD (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Rest of Centrally Planned Asia", "China+ (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|South Asia",                     "India+ (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Sub-Saharan Africa",             "Africa (R10)",
  "MESSAGEix-GLOBIOM-GAINS 2.1-R12|Western Europe",                 "Europe (R10)",

  # ── IMAGE 3.4 (short codes — older versions) ─────────────────────────────────
  "BRA",   "Latin America (R10)",
  "CAN",   "North America (R10)",
  "CHN",   "China+ (R10)",
  "EAF",   "Africa (R10)",
  "EFTA",  "Europe (R10)",
  "EU12",  "Europe (R10)",
  "EU15",  "Europe (R10)",
  "INDIA", "India+ (R10)",
  "INDO",  "Rest of Asia (R10)",
  "JAP",   "Pacific OECD (R10)",
  "KOR",   "Pacific OECD (R10)",
  "ME",    "Middle East (R10)",
  "MEX",   "Latin America (R10)",
  "NOAS",  "Middle East (R10)",
  "OCE",   "Pacific OECD (R10)",
  "OEUR",  "Reforming Economies (R10)",
  "RCAM",  "Latin America (R10)",
  "RSAM",  "Latin America (R10)",
  "RSAS",  "India+ (R10)",
  "RUS",   "Reforming Economies (R10)",
  "SAF",   "Africa (R10)",
  "SEAS",  "Rest of Asia (R10)",
  "STAN",  "Reforming Economies (R10)",
  "TURK",  "Middle East (R10)",
  "UKR",   "Reforming Economies (R10)",
  "WAF",   "Africa (R10)",

  # ── IMAGE 3.4 (pipe-format native regions) ────────────────────────────────────
  "IMAGE 3.4|Brazil",                "Latin America (R10)",
  "IMAGE 3.4|Canada",                "North America (R10)",
  "IMAGE 3.4|Central America",       "Latin America (R10)",
  "IMAGE 3.4|Central Asia",          "Reforming Economies (R10)",
  "IMAGE 3.4|Central Europe",        "Europe (R10)",
  "IMAGE 3.4|China Region",          "China+ (R10)",
  "IMAGE 3.4|Eastern Africa",        "Africa (R10)",
  "IMAGE 3.4|India",                 "India+ (R10)",
  "IMAGE 3.4|Indonesia Region",      "Rest of Asia (R10)",
  "IMAGE 3.4|Japan",                 "Pacific OECD (R10)",
  "IMAGE 3.4|Korea Region",          "Pacific OECD (R10)",
  "IMAGE 3.4|Mexico",                "Latin America (R10)",
  "IMAGE 3.4|Middle East",           "Middle East (R10)",
  "IMAGE 3.4|Northern Africa",       "Middle East (R10)",
  "IMAGE 3.4|Oceania",               "Pacific OECD (R10)",
  "IMAGE 3.4|Rest of South America", "Latin America (R10)",
  "IMAGE 3.4|Rest of South Asia",    "India+ (R10)",
  "IMAGE 3.4|Rest of Southern Africa","Africa (R10)",
  "IMAGE 3.4|Russia Region",         "Reforming Economies (R10)",
  "IMAGE 3.4|South Africa",          "Africa (R10)",
  "IMAGE 3.4|Southeastern Asia",     "Rest of Asia (R10)",
  "IMAGE 3.4|Turkey",                "Middle East (R10)",
  "IMAGE 3.4|Ukraine Region",        "Reforming Economies (R10)",
  "IMAGE 3.4|United States",         "North America (R10)",
  "IMAGE 3.4|Western Africa",        "Africa (R10)",
  "IMAGE 3.4|Western Europe",        "Europe (R10)",

  # ── IMACLIM 2.0 (pipe-format native regions) ─────────────────────────────────
  "IMACLIM 2.0|Africa",                                               "Africa (R10)",
  "IMACLIM 2.0|Brazil",                                               "Latin America (R10)",
  "IMACLIM 2.0|Canada",                                               "North America (R10)",
  "IMACLIM 2.0|China",                                                "China+ (R10)",
  "IMACLIM 2.0|Europe excluding FSU",                                 "Europe (R10)",
  "IMACLIM 2.0|Former Soviet Union excluding EU Baltic countries",    "Reforming Economies (R10)",
  "IMACLIM 2.0|India",                                                "India+ (R10)",
  "IMACLIM 2.0|Japan, Australia, New Zealand and Korea",              "Pacific OECD (R10)",
  "IMACLIM 2.0|Middle East",                                          "Middle East (R10)",
  "IMACLIM 2.0|Rest of Asia",                                         "Rest of Asia (R10)",
  "IMACLIM 2.0|Rest of Latin America",                                "Latin America (R10)",
  "IMACLIM 2.0|USA",                                                  "North America (R10)",
  "Europe excluding FSU",                             "Europe (R10)",
  "Former Soviet Union excluding EU Baltic countries","Reforming Economies (R10)",
  "India",                                            "India+ (R10)",

  # ── GEM-E3 V2026 (pipe-format country/region scheme) ─────────────────────────
  "GEM-E3 V2026|Austria",          "Europe (R10)",
  "GEM-E3 V2026|Belgium",          "Europe (R10)",
  "GEM-E3 V2026|Bulgaria",         "Europe (R10)",
  "GEM-E3 V2026|Croatia",          "Europe (R10)",
  "GEM-E3 V2026|Cyprus",           "Europe (R10)",
  "GEM-E3 V2026|Czech Republic",   "Europe (R10)",
  "GEM-E3 V2026|Denmark",          "Europe (R10)",
  "GEM-E3 V2026|Estonia",          "Europe (R10)",
  "GEM-E3 V2026|Finland",          "Europe (R10)",
  "GEM-E3 V2026|France",           "Europe (R10)",
  "GEM-E3 V2026|Germany",          "Europe (R10)",
  "GEM-E3 V2026|Greece",           "Europe (R10)",
  "GEM-E3 V2026|Hungary",          "Europe (R10)",
  "GEM-E3 V2026|Ireland",          "Europe (R10)",
  "GEM-E3 V2026|Italy",            "Europe (R10)",
  "GEM-E3 V2026|Latvia",           "Europe (R10)",
  "GEM-E3 V2026|Lithuania",        "Europe (R10)",
  "GEM-E3 V2026|Luxembourg",       "Europe (R10)",
  "GEM-E3 V2026|Malta",            "Europe (R10)",
  "GEM-E3 V2026|Netherlands",      "Europe (R10)",
  "GEM-E3 V2026|Poland",           "Europe (R10)",
  "GEM-E3 V2026|Portugal",         "Europe (R10)",
  "GEM-E3 V2026|Romania",          "Europe (R10)",
  "GEM-E3 V2026|Slovakia",         "Europe (R10)",
  "GEM-E3 V2026|Slovenia",         "Europe (R10)",
  "GEM-E3 V2026|Spain",            "Europe (R10)",
  "GEM-E3 V2026|Sweden",           "Europe (R10)",
  "GEM-E3 V2026|United Kingdom",   "Europe (R10)",
  "GEM-E3 V2026|Rest of Europe",   "Europe (R10)",
  "GEM-E3 V2026|Turkey",           "Middle East (R10)",
  "GEM-E3 V2026|USA",              "North America (R10)",
  "GEM-E3 V2026|Canada",           "North America (R10)",
  "GEM-E3 V2026|Japan",            "Pacific OECD (R10)",
  "GEM-E3 V2026|South Korea",      "Pacific OECD (R10)",
  "GEM-E3 V2026|Oceania",          "Pacific OECD (R10)",
  "GEM-E3 V2026|China",            "China+ (R10)",
  "GEM-E3 V2026|India",            "India+ (R10)",
  "GEM-E3 V2026|Russian Federation","Reforming Economies (R10)",
  "GEM-E3 V2026|Brazil",           "Latin America (R10)",
  "GEM-E3 V2026|Argentina",        "Latin America (R10)",
  "GEM-E3 V2026|Mexico",           "Latin America (R10)",
  "GEM-E3 V2026|Indonesia",        "Rest of Asia (R10)",
  "GEM-E3 V2026|South Africa",     "Africa (R10)",
  "GEM-E3 V2026|Saudi Arabia",     "Middle East (R10)",
  "GEM-E3 V2026|United Arab Emirates", "Middle East (R10)",
  "GEM-E3 V2026|Other Major Fossil-Exporting Countries", "Middle East (R10)",
  "GEM-E3 V2026|Rest of the World",                      "Other (R10)",

  # ── OPEN-PROM 2.2 (pipe-format native regions) ───────────────────────────────
  "OPEN-PROM 2.2|Austria",          "Europe (R10)",
  "OPEN-PROM 2.2|Belgium",          "Europe (R10)",
  "OPEN-PROM 2.2|Bulgaria",         "Europe (R10)",
  "OPEN-PROM 2.2|Croatia",          "Europe (R10)",
  "OPEN-PROM 2.2|Cyprus",           "Europe (R10)",
  "OPEN-PROM 2.2|Czechia",          "Europe (R10)",
  "OPEN-PROM 2.2|Denmark",          "Europe (R10)",
  "OPEN-PROM 2.2|Estonia",          "Europe (R10)",
  "OPEN-PROM 2.2|Finland",          "Europe (R10)",
  "OPEN-PROM 2.2|France",           "Europe (R10)",
  "OPEN-PROM 2.2|Germany",          "Europe (R10)",
  "OPEN-PROM 2.2|Greece",           "Europe (R10)",
  "OPEN-PROM 2.2|Hungary",          "Europe (R10)",
  "OPEN-PROM 2.2|Ireland",          "Europe (R10)",
  "OPEN-PROM 2.2|Italy",            "Europe (R10)",
  "OPEN-PROM 2.2|Latvia",           "Europe (R10)",
  "OPEN-PROM 2.2|Lithuania",        "Europe (R10)",
  "OPEN-PROM 2.2|Luxembourg",       "Europe (R10)",
  "OPEN-PROM 2.2|Malta",            "Europe (R10)",
  "OPEN-PROM 2.2|Netherlands",      "Europe (R10)",
  "OPEN-PROM 2.2|Poland",           "Europe (R10)",
  "OPEN-PROM 2.2|Portugal",         "Europe (R10)",
  "OPEN-PROM 2.2|Romania",          "Europe (R10)",
  "OPEN-PROM 2.2|Slovakia",         "Europe (R10)",
  "OPEN-PROM 2.2|Slovenia",         "Europe (R10)",
  "OPEN-PROM 2.2|Spain",            "Europe (R10)",
  "OPEN-PROM 2.2|Sweden",           "Europe (R10)",
  "OPEN-PROM 2.2|United Kingdom",   "Europe (R10)",
  "OPEN-PROM 2.2|Non-EU Europe",    "Europe (R10)",
  "OPEN-PROM 2.2|United States",                               "North America (R10)",
  "OPEN-PROM 2.2|Canada, Australia, New Zealand",              "Pacific OECD (R10)",
  "OPEN-PROM 2.2|Japan",                                       "Pacific OECD (R10)",
  "OPEN-PROM 2.2|China and Taiwan",                            "China+ (R10)",
  "OPEN-PROM 2.2|India",                                       "India+ (R10)",
  "OPEN-PROM 2.2|Reforming Economies of Former Soviet Union",  "Reforming Economies (R10)",
  "OPEN-PROM 2.2|Latin America and the Caribbean",             "Latin America (R10)",
  "OPEN-PROM 2.2|Middle East, North Africa, Central Asia",     "Middle East (R10)",
  "OPEN-PROM 2.2|Sub-Saharan Africa",                          "Africa (R10)",
  "OPEN-PROM 2.2|Other Asia",                                  "Rest of Asia (R10)",
)

# ── Load ───────────────────────────────────────────────────────────────────────
cp_raw  <- load_latest("cp_native")
co2_raw <- load_latest("co2_native")

# ── Model filtering ────────────────────────────────────────────────────────────
models <- filter_models(sort(unique(cp_raw$model)))
message("Models included: ", paste(models, collapse = ", "))

# ── Prepare: filter models, scenarios, exclude World, normalize variable name ──
cp_data <- cp_raw %>%
  filter(model    %in% models,
         scenario %in% scenarios,
         grepl("^Price\\|Carbon", variable),
         region   != "World",
         region   != "WORLD",
         year     >= 2005,
         year     <= 2100) %>%
  mutate(cp_unit = unit)

co2_data <- co2_raw %>%
  filter(model    %in% models,
         scenario %in% scenarios,
         variable == "Emissions|CO2",
         region   != "World",
         region   != "WORLD",
         year     >= 2005,
         year     <= 2100)

# ── Map native regions to R10 ──────────────────────────────────────────────────
cp_mapped  <- cp_data  %>% left_join(native_to_r10, by = c("region" = "native"))
co2_mapped <- co2_data %>% left_join(native_to_r10, by = c("region" = "native"))

# ── Step 1: suppress non-R10 Rx aggregates for ALL models ─────────────────────
suppress_rx <- function(df) {
  df %>% mutate(r10 = if_else(
    grepl("\\(R[0-9]+\\)", region) & !grepl("\\(R10\\)", region),
    NA_character_, r10
  ))
}
cp_mapped  <- suppress_rx(cp_mapped)
co2_mapped <- suppress_rx(co2_mapped)

# ── Step 2: for models with pipe-format regions, use ONLY those ───────────────
models_pipe_cp  <- cp_mapped  %>% filter(grepl("\\|", region), !is.na(r10)) %>% pull(model) %>% unique()
models_pipe_co2 <- co2_mapped %>% filter(grepl("\\|", region), !is.na(r10)) %>% pull(model) %>% unique()

cp_mapped <- cp_mapped %>%
  mutate(r10 = if_else(model %in% models_pipe_cp  & !grepl("\\|", region), NA_character_, r10))
co2_mapped <- co2_mapped %>%
  mutate(r10 = if_else(model %in% models_pipe_co2 & !grepl("\\|", region), NA_character_, r10))

message("Models using pipe-format native regions: ",        paste(sort(models_pipe_cp),  collapse = ", "))
message("Models using R10 pass-through / short codes: ",    paste(sort(setdiff(models, models_pipe_cp)), collapse = ", "))

# ── Report unmapped regions ────────────────────────────────────────────────────
unmapped_cp <- cp_mapped %>% filter(is.na(r10)) %>% distinct(model, region)
if (nrow(unmapped_cp) > 0) {
  message("\n", strrep("!", 70))
  message("UNMAPPED REGIONS \u2014 excluded from R10 aggregation.")
  message("Add these entries to the native_to_r10 table in this script:")
  message(strrep("!", 70))
  unmapped_cp %>%
    arrange(model, region) %>%
    group_by(model) %>%
    group_walk(~ {
      message("  Model: ", .y$model)
      for (r in .x$region) message('    "', r, '",  "<TARGET R10 REGION>",')
    })
  message(strrep("!", 70), "\n")
} else {
  message("All regions successfully mapped to R10.")
}

# ── Keep only mapped regions ───────────────────────────────────────────────────
cp_mapped  <- cp_mapped  %>% filter(!is.na(r10))
co2_mapped <- co2_mapped %>% filter(!is.na(r10))

# ── CO2-weighted aggregation: native regions → R10 ────────────────────────────
joined <- cp_mapped %>%
  select(model, scenario, native_region = region, r10_region = r10,
         year, cp = value, cp_unit) %>%
  inner_join(
    co2_mapped %>% select(model, scenario, native_region = region, year, co2 = value),
    by = c("model", "scenario", "native_region", "year")
  ) %>%
  mutate(weight = abs(co2))

cp_r10 <- joined %>%
  group_by(model, scenario, region = r10_region, year, cp_unit) %>%
  summarise(
    value = {
      w  <- weight[!is.na(cp) & !is.na(weight)]
      cp <- cp[!is.na(cp) & !is.na(weight)]
      if (length(cp) == 0)     NA_real_
      else if (sum(w) > 0)     weighted.mean(cp, w)
      else                     mean(cp, na.rm = TRUE)
    },
    n_native = n(),
    .groups = "drop"
  )

message("R10-aggregated rows: ", nrow(cp_r10))

# ── World: CO2-weighted mean of R10 values ─────────────────────────────────────
co2_by_r10 <- co2_mapped %>%
  filter(!is.na(r10)) %>%
  group_by(model, scenario, r10_region = r10, year) %>%
  summarise(co2_r10 = sum(value, na.rm = TRUE), .groups = "drop")

cp_world <- cp_r10 %>%
  left_join(co2_by_r10, by = c("model", "scenario", "region" = "r10_region", "year")) %>%
  group_by(model, scenario, year, cp_unit) %>%
  summarise(
    value = {
      w <- abs(co2_r10[!is.na(value) & !is.na(co2_r10)])
      v <- value[!is.na(value) & !is.na(co2_r10)]
      if (length(v) == 0)  NA_real_
      else if (sum(w) > 0) weighted.mean(v, w)
      else                 mean(v, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(region = "World")

message("World rows: ", nrow(cp_world))

# ── Combine R10 + World ────────────────────────────────────────────────────────
cp_final <- bind_rows(
  cp_r10   %>% select(model, scenario, region, year, value, unit = cp_unit),
  cp_world %>% select(model, scenario, region, year, value, unit = cp_unit)
) %>%
  arrange(model, scenario, region, year)

# ── Coverage summary ───────────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("Carbon price R10 coverage summary")
message(strrep("=", 60))
cp_final %>%
  filter(region != "World", !is.na(value)) %>%
  distinct(model, scenario, region) %>%
  count(model, scenario, name = "n_regions") %>%
  arrange(model, scenario) %>%
  { message(paste0("  ", .$model, " / ", .$scenario, ": ", .$n_regions,
                   " R10 regions", collapse = "\n")) }
message(strrep("=", 60), "\n")

# ── Save to Excel ──────────────────────────────────────────────────────────────
write_xlsx(cp_final, xl_path)
message("Saved: ", xl_path)
message("Rows:  ", nrow(cp_final))
message("Cols:  model, scenario, region, year, value, unit")

} # end if (.needs_recompute)
rm(.needs_recompute)
