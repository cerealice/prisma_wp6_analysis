# ============================================================
# carbon_price_loader.R
# Helper: load CO2-weighted carbon price from pre-computed Excel.
#
# Usage (in any script):
#   source(file.path(scripts_dir, "carbon_price_loader.R"))
#   cp <- load_cp_weighted(regions = c("World", r10_regions))
#
# If data/carbon_price_weighted_r10.xlsx does not exist, this
# helper automatically runs compute_carbon_price_r10.R once to
# generate it (downloads from PRISMA — requires network access).
# On subsequent calls the Excel is read directly (no re-download).
# ============================================================

load_cp_weighted <- function(
  scenarios = NULL,
  regions   = NULL,
  years     = NULL,
  models    = NULL,
  data_dir  = NULL
) {
  # ── Resolve paths ─────────────────────────────────────────────────────────────
  if (is.null(data_dir)) {
    wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
    project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
    data_dir    <- file.path(project_dir, "data")
  } else {
    project_dir <- dirname(data_dir)
  }

  xl_path      <- file.path(data_dir, "carbon_price_weighted_r10.xlsx")
  compute_path <- file.path(project_dir, "scripts", "compute_carbon_price_r10.R")

  # ── Auto-generate if Excel is missing ─────────────────────────────────────────
  if (!file.exists(xl_path)) {
    if (!file.exists(compute_path)) {
      stop(
        "carbon_price_weighted_r10.xlsx not found and compute script missing:\n  ",
        compute_path
      )
    }
    message(strrep("=", 60))
    message("carbon_price_weighted_r10.xlsx not found.")
    message("Running compute_carbon_price_r10.R to generate it ...")
    message("(This downloads from PRISMA — happens only once.)")
    message(strrep("=", 60))
    source(compute_path, local = FALSE)
    if (!file.exists(xl_path)) {
      stop("compute_carbon_price_r10.R ran but did not produce ", xl_path)
    }
    message(strrep("=", 60))
    message("Excel generated successfully. Future calls will use the cached file.")
    message(strrep("=", 60))
  }

  # ── Load and filter ───────────────────────────────────────────────────────────
  df <- readxl::read_xlsx(xl_path)

  if (!is.null(scenarios)) df <- df %>% filter(scenario %in% scenarios)
  if (!is.null(regions))   df <- df %>% filter(region   %in% regions)
  if (!is.null(years))     df <- df %>% filter(year     %in% years)
  if (!is.null(models))    df <- df %>% filter(model    %in% models)

  df
}
