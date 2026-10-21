# ══════════════════════════════════════════════════════════════
# captions.R  —  post_oct15
#
# Central store for all figure captions.
# Each figure script calls update_caption() at the end, which
# logs a timestamp for that figure and regenerates:
#   graphs/figure_captions.txt
#
# To edit a caption: change the text here, then re-run the
# corresponding figure script (or call write_captions_txt()
# directly after sourcing this file).
# ══════════════════════════════════════════════════════════════

CAPTIONS <- list(

  fig_sm1 = list(
    label = "Supplementary Figure SM1",
    text  = paste0(
      "Supplementary Figure SM1 | PRISMA WP6 country cluster assignments. ",
      "World map showing the assignment of countries to the three PRISMA clusters: ",
      "Transition Leaders (blue), Diversifying Economies (orange), and ",
      "Fossil-Dependent Economies (green). ",
      "Solid fill = data-confirmed assignment; hatching (////) = cluster assumed ",
      "from partial data (at least one of fossil wealth, renewables growth, or GHG ",
      "intensity missing). ",
      "Classification based on the PRISMA WP6 Scenario Protocol."
    )
  ),

  fig1 = list(
    label = "Figure 1",
    text  = paste0(
      "Figure 1 | Scenario design. ",
      "The four PRISMA scenarios positioned in the space of climate policy ambition ",
      "(x-axis) and clean-technology investment (y-axis). ",
      "Meet Aspirations (top-right): NDCs and long-term strategies implemented. ",
      "Tech Momentum (top-left): ambition withdrawn, investment held. ",
      "Shifted Ambition (bottom-left): milestones deferred by 10, 20 or 30 years. ",
      "Late Reawakening (bottom-right): follows Shifted Ambition until 2035, ",
      "then resumes Meet Aspirations long-term targets from 2040. ",
      "The curved arrow traces the Late Reawakening trajectory. ",
      "RQ1 compares Shifted Ambition with Meet Aspirations; ",
      "RQ2 compares Late Reawakening with Meet Aspirations; ",
      "RQ3 compares Tech Momentum with Shifted Ambition."
    )
  ),

  fig2 = list(
    label = "Figure 2",
    text  = paste0(
      "Figure 2 | CO2 emissions and surface temperature across PRISMA scenarios. ",
      "(a) World CO2 emissions, 2020-2100. ",
      "Lines show the cross-model median; ribbons span the model minimum to maximum. ",
      "(b) Distribution of global surface temperature across models and MAGICC v7.6.0a3 ",
      "percentiles (T33, T50, T67). For each scenario, two side-by-side boxplots show ",
      "peak temperature (yellow) and temperature at 2100 (pink). ",
      "Box spans Q33-Q67; whiskers extend to Q10-Q90, computed across all ",
      "model x MAGICC-percentile combinations. ",
      "Dotted black line marks 2.0 degrees C above pre-industrial levels. ",
      "All climate diagnostics computed with MAGICC v7.6.0a3."
    )
  ),

  fig3 = list(
    label = "Figure 3",
    text  = paste0(
      "Figure 3 | Economic impacts across PRISMA scenarios. ",
      "(a) Annual GDP relative to the Meet Aspirations scenario, 2020-2100. ",
      "Solid lines show undamaged GDP; dashed lines show damage-adjusted GDP using the ",
      "Howard & Sterner damage function applied to the global mean surface temperature (T50). ",
      "Bold black line denotes the cross-model mean. Only models with endogenous GDP are shown. ",
      "(b) Net Present Value (NPV) of GDP relative to Meet Aspirations, aggregated over ",
      "2020-2100, global totals. ",
      "(c) NPV of GDP relative to Meet Aspirations by PRISMA cluster. ",
      "Damage function: D(T) = alpha * T^1.5 + beta * T (Howard & Sterner)."
    )
  ),

  fig4 = list(
    label = "Figure 4",
    text  = paste0(
      "Figure 4 | Technology and socio-political feasibility ",
      "indicators for SSP2 - Late Reawakening. ",
      "Each dot shows the worst-decade value for one model across 2030-2100. ",
      "Dashed line = medium concern threshold; dotted line = high concern threshold. ",
      "Colours: green = low concern, yellow = medium, red = high, grey = not reported. ",
      "Feasibility framework following Brutschin et al. (2021, ",
      "Environ. Res. Lett. 16, 064069). ",
      "Indicators: 2.1 carbon price decade growth (\u00d7); ",
      "3.1 wind electricity share scale-up (pp/decade); ",
      "3.2 solar electricity share scale-up (pp/decade); ",
      "4.1 total final energy demand decline (%/decade); ",
      "5.1 global per-capita CO\u2082 emissions decline (%/decade)."
    )
  ),

  fig5 = list(
    label = "Figure 5",
    text  = paste0(
      "Figure 5 | Energy transition indicators across PRISMA scenarios and clusters ",
      "(Late Reawakening excluded). ",
      "(a) Renewable share (%) in primary energy supply. ",
      "(b) Electricity share (%) in final energy demand. ",
      "Bars show the cross-model median at 2030, 2040, and 2050; ",
      "points show individual model values. ",
      "Labels on Shifted Ambition and Tech Momentum bars show the % change ",
      "in median relative to Meet Aspirations. ",
      "R10 regions summed within PRISMA cluster before computing shares. ",
      "Clusters: Transition Leaders, Diversifying Economies, Fossil-Dependent Economies."
    )
  ),

  fig_sm2 = list(
    label = "Supplementary Figure SM2",
    text  = paste0(
      "Supplementary Figure SM2 | Global surface temperature, cumulative carbon budget, ",
      "effective radiative forcing, and CO2 emissions by PRISMA cluster. ",
      "(a) Global mean surface temperature (GSAT, T50 percentile) timeseries, 2020-2100. ",
      "Bold line = most-central model; ribbon = model min-max range. ",
      "(b) As (a) for the 67th-percentile temperature (T67). ",
      "(c) Cumulative CO2 emissions from 2020, anchored at zero in 2020 for all models. ",
      "Lines show the cross-model median; ribbons span model minimum to maximum. ",
      "Dashed horizontal lines mark IPCC AR6 WG1 remaining carbon budgets from 2020 ",
      "(CO2-only): 400, 500, 1150, 1350 GtCO2. ",
      "(d) Effective Radiative Forcing, 2020-2100. ",
      "Lines show the cross-model median; ribbons span model minimum to maximum. ",
      "(e) World CO2 emissions by PRISMA cluster, 2020-2100. ",
      "Lines show the cross-model median within each cluster; ",
      "ribbons span the model minimum to maximum. ",
      "All temperature and climate diagnostics computed with MAGICC v7.6.0a3."
    )
  ),

  fig_sm3 = list(
    label = "Supplementary Figure SM3",
    text  = paste0(
      "Supplementary Figure SM3 | Sensitivity of GDP Net Present Value to discount rate. ",
      "NPV of GDP relative to the Meet Aspirations scenario (% of damage-adjusted Meet NPV) ",
      "computed at discount rates of 1%, 2% (default, matching Figure 3), 3%, and 4%. ",
      "Each panel shows one non-Meet scenario; x-axis is the discount rate. ",
      "Lines connect per-model median NPV (T50 surface temperature); ",
      "error bars span the Q10-Q90 range of the Howard & Sterner damage uncertainty ",
      "(5000 Monte Carlo draws). Only models with endogenous GDP are included. ",
      "Damage function: D(T) = alpha * T^1.5 + beta * T."
    )
  ),

  fig_sm6 = list(
    label = "Supplementary Figure SM6",
    text  = paste0(
      "Supplementary Figure SM6 | GDP trajectories relative to Meet Aspirations, ",
      "disaggregated by PRISMA cluster and scenario. ",
      "Each panel shows annual GDP (% relative to cluster-level Meet Aspirations, 2020-2100) ",
      "for one cluster (columns: Transition Leaders, Diversifying Economies, Fossil-Dependent) ",
      "and one scenario (rows). ",
      "Solid lines show undamaged GDP; dashed lines show damage-adjusted GDP ",
      "using the Howard & Sterner damage function applied to the global T50 surface temperature. ",
      "Only models with endogenous GDP are included. ",
      "Damage function: D(T) = alpha * T^1.5 + beta * T."
    )
  ),

  fig_sm7 = list(
    label = "Supplementary Figure SM7",
    text  = paste0(
      "Supplementary Figure SM7 | Technology and socio-political feasibility ",
      "indicators for three PRISMA scenarios. ",
      "(a) SSP2 - Meet Aspirations. ",
      "(b) SSP2 - Asymmetric Roll-Back (Shifted Ambition). ",
      "(c) SSP2 - Staying Alive (Tech Momentum). ",
      "Each dot shows the worst-decade value for one model across 2030-2100. ",
      "Dashed line = medium concern threshold; dotted line = high concern threshold. ",
      "Colours: green = low concern, yellow = medium, red = high, grey = not reported. ",
      "Feasibility framework following Brutschin et al. (2021, ",
      "Environ. Res. Lett. 16, 064069). ",
      "Indicators: 2.1 carbon price decade growth (\u00d7); ",
      "3.1 wind electricity share scale-up (pp/decade); ",
      "3.2 solar electricity share scale-up (pp/decade); ",
      "4.1 total final energy demand decline (%/decade); ",
      "5.1 global per-capita CO\u2082 emissions decline (%/decade)."
    )
  ),

  fig_sm8 = list(
    label = "Supplementary Figure SM8",
    text  = paste0(
      "Supplementary Figure SM8 | Sensitivity of GDP NPV to the choice of damage function. ",
      "Replicates Figure 3 panel (b) for all 9 Howard & Sterner (2025) damage functions. ",
      "Rows: three non-Meet scenarios (Shifted Ambition, Late Reawakening, Tech Momentum). ",
      "Columns: 9 H&S (2025) damage function specifications. ",
      "x-axis: GSAT median (T50) and 67th percentile (T67). ",
      "y-axis: NPV vs Meet Aspirations (% of damage-adjusted Meet NPV). ",
      "Error bars show the q10-q90 Monte Carlo range (N=5000 draws) for functions 2-7; ",
      "functions 1, 8, 9 use fixed coefficients and have no uncertainty band. ",
      "Star (*) marks the preferred specification (fn6). ",
      "Discount rate 2%, base year 2020. ",
      "Damage function: Howard & Sterner (2025), Environ Resour Econ 88:3289-3327."
    )
  ),

  fig_gem_e3_ca_diag = list(
    label = "Supplementary Diagnostic - GEM-E3 (Staying Alive)",
    text  = paste0(
      "Supplementary diagnostic | All Climate Assessment variables for ",
      "SSP2 - Staying Alive. ",
      "Each panel shows one Climate Assessment output variable reported by PRISMA models ",
      "(MAGICC v7.6.0a3). Lines are coloured by model; ",
      "GEM-E3 V2026 is highlighted with a bold red line. ",
      "y-axes are free across panels (units differ per variable). ",
      "Produced to diagnose anomalous GEM-E3 temperature behaviour."
    )
  ),

  gem_e3_ca_meet_aspirations = list(
    label = "Supplementary Diagnostic - GEM-E3 (Meet Aspirations)",
    text  = paste0(
      "Supplementary diagnostic | All Climate Assessment variables for ",
      "SSP2 - Meet Aspirations. ",
      "Layout and colour coding as in the Staying Alive diagnostic. ",
      "GEM-E3 V2026 is highlighted with a bold red line."
    )
  ),

  gem_e3_ca_late_reawakening = list(
    label = "Supplementary Diagnostic - GEM-E3 (Late Reawakening)",
    text  = paste0(
      "Supplementary diagnostic | All Climate Assessment variables for ",
      "SSP2 - Late Reawakening. ",
      "Layout and colour coding as in the Staying Alive diagnostic. ",
      "GEM-E3 V2026 is highlighted with a bold red line."
    )
  ),

  gem_e3_ca_staying_alive = list(
    label = "Supplementary Diagnostic - GEM-E3 (Staying Alive, all-scenarios run)",
    text  = paste0(
      "Supplementary diagnostic | All Climate Assessment variables for ",
      "SSP2 - Staying Alive (from run_gem_e3_ca.R). ",
      "Layout and colour coding as in the standalone Staying Alive diagnostic."
    )
  ),

  gem_e3_ca_asymmetric_rollback = list(
    label = "Supplementary Diagnostic - GEM-E3 (Asymmetric Roll-Back)",
    text  = paste0(
      "Supplementary diagnostic | All Climate Assessment variables for ",
      "SSP2 - Asymmetric Roll-Back. ",
      "Layout and colour coding as in the Staying Alive diagnostic. ",
      "GEM-E3 V2026 is highlighted with a bold red line."
    )
  )

)

# ══════════════════════════════════════════════════════════════
# update_caption()
#   Called at the end of each figure script.
#   Records a timestamp for fig_id and regenerates the Word file.
# ══════════════════════════════════════════════════════════════

update_caption <- function(fig_id, graphs_dir) {
  store_path <- file.path(graphs_dir, ".caption_timestamps.rds")
  store <- if (file.exists(store_path)) readRDS(store_path) else list()
  store[[fig_id]] <- format(Sys.time(), "%Y-%m-%d %H:%M")
  saveRDS(store, store_path)
  write_captions_docx(graphs_dir, store)
  invisible(NULL)
}

# ══════════════════════════════════════════════════════════════
# write_captions_docx()
#   Writes graphs/figure_captions.docx
#   Open in Microsoft Word.
# ══════════════════════════════════════════════════════════════

write_captions_docx <- function(graphs_dir, store = list()) {

  if (!requireNamespace("officer", quietly = TRUE))
    stop("Package 'officer' is required. Run: install.packages('officer')")

  out_path <- file.path(graphs_dir, "figure_captions.docx")

  doc <- officer::read_docx()

  # ── Title block ──────────────────────────────────────────────
  doc <- officer::body_add_par(
    doc, "PRISMA Post-Oct-15 \u2014 Figure Captions", style = "heading 1")
  doc <- officer::body_add_par(
    doc, paste0("Last regenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    style = "Normal")

  # ── Script run log ───────────────────────────────────────────
  doc <- officer::body_add_par(doc, "Script Run Log", style = "heading 2")
  if (length(store) > 0) {
    for (nm in names(store))
      doc <- officer::body_add_par(
        doc, sprintf("%-38s  %s", nm, store[[nm]]), style = "Normal")
  } else {
    doc <- officer::body_add_par(doc, "(no scripts logged yet)", style = "Normal")
  }

  # ── Captions ─────────────────────────────────────────────────
  doc <- officer::body_add_par(doc, "Captions", style = "heading 2")

  for (fig_id in names(CAPTIONS)) {
    cap <- CAPTIONS[[fig_id]]
    ts  <- if (!is.null(store[[fig_id]])) store[[fig_id]] else "not yet run"

    doc <- officer::body_add_par(
      doc,
      paste0(cap$label, "  [Last run: ", ts, "]"),
      style = "heading 3")
    doc <- officer::body_add_par(doc, cap$text, style = "Normal")
  }

  print(doc, target = out_path)
  message("Captions updated: ", out_path)
}
