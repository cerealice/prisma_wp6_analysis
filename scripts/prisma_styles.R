# ============================================================
# PRISMA shared plot styles
# Source this file in any script that shows individual model
# markers to guarantee the same shape → model mapping everywhere.
# ============================================================

# ── Arial font registration ───────────────────────────────────
# Register Arial for both the Windows GDI device (ggsave default) and
# the ragg device (RStudio graphics pane), silencing "font family not
# found in Windows font database" warnings on both.
if (.Platform$OS.type == "windows")
  windowsFonts(Arial = windowsFont("Arial"))
local({
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(invisible(NULL))
  already <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character(0))
  if ("Arial" %in% already) return(invisible(NULL))
  ttf <- file.path(Sys.getenv("WINDIR"), "Fonts", "arial.ttf")
  if (file.exists(ttf))
    systemfonts::register_font("Arial", plain = ttf,
                               bold        = file.path(dirname(ttf), "arialbd.ttf"),
                               italic      = file.path(dirname(ttf), "ariali.ttf"),
                               bolditalic  = file.path(dirname(ttf), "arialbi.ttf"))
})

model_shapes <- c(
  # Full names
  "GEM-E3 V2026"                          = 16,   # filled circle
  "IMACLIM 2.0"                           = 17,   # filled triangle up
  "IMAGE 3.4"                             = 15,   # filled square
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12"    = 18,   # filled diamond
  "OPEN-PROM 2.2"                         =  8,   # asterisk / star
  "REMIND-MAgPIE 3.7-4.14"               =  1,   # open circle
  "WITCH 6.0"                             =  2,   # open triangle up
  "WITCH 6.0-EU28"                        =  0,   # open square
  # Short names (same shapes — used when legends show abbreviations)
  "GEM-E3"                                = 16,
  "IMACLIM"                               = 17,
  "IMAGE"                                 = 15,
  "MESSAGEix"                             = 18,
  "OPEN-PROM"                             =  8,
  "REMIND"                                =  1,
  "WITCH"                                 =  2
)

# Call this instead of using model_shapes directly.
# Returns a named shape vector that covers every model in `models_in_data`:
#   - known models get their fixed shape from the dictionary above
#   - unknown models (name mismatch / new models) get a fallback shape
#     and a message so you know to update the dictionary
resolve_model_shapes <- function(models_in_data) {
  known   <- model_shapes[names(model_shapes) %in% models_in_data]
  unknown <- setdiff(models_in_data, names(known))
  if (length(unknown) > 0) {
    message("Models not in prisma_styles.R model_shapes (update dictionary): ",
            paste(unknown, collapse = ", "))
    all_pch   <- c(16, 17, 15, 18, 8, 1, 2, 0, 5, 6, 3, 4, 7, 9, 10, 11)
    spare_pch <- setdiff(all_pch, unname(known))
    extra     <- setNames(spare_pch[seq_along(unknown)], unknown)
    known     <- c(known, extra)
  }
  known
}

# When multiple versions of REMIND-MAgPIE are present in the data,
# keep only the one with the highest primary version number
# (e.g. 3.7 beats 3.6).  Pass the model vector; get the filtered vector back.
filter_latest_remind <- function(m) {
  remind_models <- m[grepl("^REMIND", m)]
  if (length(remind_models) <= 1) return(m)
  versions <- regmatches(remind_models, regexpr("[0-9]+\\.[0-9]+", remind_models))
  latest   <- remind_models[order(numeric_version(versions), decreasing = TRUE)[1]]
  message("Multiple REMIND versions found — keeping: ", latest)
  m[!grepl("^REMIND", m) | m == latest]
}

model_colors <- c(
  "GEM-E3 V2026"                       = "#E41A1C",   # red
  "IMACLIM 2.0"                        = "#377EB8",   # blue
  "IMAGE 3.4"                          = "#4DAF4A",   # green
  "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" = "#984EA3",   # purple
  "OPEN-PROM 2.2"                      = "#FF7F00",   # orange
  "REMIND-MAgPIE 3.7-4.14"            = "#A65628",   # brown
  "WITCH 6.0"                          = "#F781BF"    # pink
)

resolve_model_colors <- function(models_in_data) {
  known   <- model_colors[names(model_colors) %in% models_in_data]
  unknown <- setdiff(models_in_data, names(known))
  if (length(unknown) > 0) {
    message("Models not in prisma_styles.R model_colors (update dictionary): ",
            paste(unknown, collapse = ", "))
    fallback <- c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494")
    spare    <- setdiff(fallback, unname(known))
    extra    <- setNames(spare[seq_along(unknown)], unknown)
    known    <- c(known, extra)
  }
  known
}

scenario_colors <- c(
  "SSP2 - Asymmetric Roll-Back" = "#666666",
  "SSP2 - Late Reawakening"     = "#D62728",
  "SSP2 - Meet Aspirations"     = "#5BB85C",
  "SSP2 - Staying Alive"        = "#87CEEB"
)

scenario_labels <- c(
  "SSP2 - Asymmetric Roll-Back" = "Uneven Retreat",
  "SSP2 - Late Reawakening"     = "Late Reawakening",
  "SSP2 - Meet Aspirations"     = "Meet Aspirations",
  "SSP2 - Staying Alive"        = "Tech Momentum"
)

# PRISMA cluster colors — matched to the country-cluster map figure.
# Distinct from scenario_colors by design.
cluster_colors <- c(
  "Transition leaders"          = "#7B9CC8",   # periwinkle blue
  "Diversifying economies"      = "#F4874B",   # warm orange
  "Fossil-dependent economies"  = "#2CBD9E"    # teal
)
