# ══════════════════════════════════════════════════════════════
# fig5_temp_nocluster.R  —  TEMPORARY
# Figure 5 without the cluster breakdown: all R10 regions pooled
# to give a single world-level bar chart per panel.
# Sources fig5.R (with DOWNLOAD_DATA = FALSE) to reuse loaded data.
#
# Output: graphs/fig5_temp_nocluster.png
# ══════════════════════════════════════════════════════════════

wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
graphs_dir  <- file.path(project_dir, "graphs")

DOWNLOAD_DATA <- FALSE
source(file.path(scripts_dir, "fig5.R"))

# After sourcing fig5.R the following are in scope:
#   se_raw, fe_raw, all_models, se_renew_final, se_elec_total_var,
#   fe_elec_var, fe_total_var, all_model_lbls, model_shapes,
#   scenario_levels, scenario_colors, scenario_labels,
#   shift_scen, plot_years, short_name(), dodge

# ══════════════════════════════════════════════════════════════
# 1. REAGGREGATE — all R10 regions pooled (no cluster split)
# ══════════════════════════════════════════════════════════════
re_renew_w <- se_raw %>%
  filter(model %in% all_models, variable %in% se_renew_final) %>%
  group_by(model, scenario, year) %>%
  summarise(renew = sum(value, na.rm = TRUE), .groups = "drop")

re_total_w <- se_raw %>%
  filter(model %in% all_models, variable == se_elec_total_var) %>%
  group_by(model, scenario, year) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

re_share_w <- re_renew_w %>%
  inner_join(re_total_w, by = c("model", "scenario", "year")) %>%
  mutate(value = renew / total * 100) %>%
  filter(is.finite(value), total > 0) %>%
  select(model, scenario, year, value)

fe_elec_w <- fe_raw %>%
  filter(model %in% all_models, variable == fe_elec_var) %>%
  group_by(model, scenario, year) %>%
  summarise(elec = sum(value, na.rm = TRUE), .groups = "drop")

fe_total_w <- fe_raw %>%
  filter(model %in% all_models, variable == fe_total_var) %>%
  group_by(model, scenario, year) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

fe_share_w <- fe_elec_w %>%
  inner_join(fe_total_w, by = c("model", "scenario", "year")) %>%
  mutate(value = elec / total * 100) %>%
  filter(is.finite(value), total > 0) %>%
  select(model, scenario, year, value)

# ══════════════════════════════════════════════════════════════
# 2. PREP DOTS / SUMMARY / LABELS (no cluster column)
# ══════════════════════════════════════════════════════════════
prep_dots_w <- function(share_df) {
  share_df %>%
    filter(year %in% plot_years) %>%
    mutate(
      year_f    = factor(year, levels = as.character(plot_years)),
      scenario  = factor(scenario, levels = scenario_levels),
      model_lbl = factor(short_name(as.character(model)), levels = all_model_lbls)
    )
}

prep_summary_w <- function(dots_df) {
  dots_df %>%
    group_by(year_f, scenario) %>%
    summarise(median_val = median(value, na.rm = TRUE), .groups = "drop")
}

prep_labels_w <- function(summary_df) {
  shift_ref <- summary_df %>%
    filter(as.character(scenario) == shift_scen) %>%
    select(year_f, ref_val = median_val)
  summary_df %>%
    left_join(shift_ref, by = "year_f") %>%
    mutate(
      pct_chg    = (median_val - ref_val) / ref_val * 100,
      label_text = ifelse(as.character(scenario) == shift_scen, "",
                          sprintf("%+.0f%%", pct_chg))
    )
}

dots_a_w    <- prep_dots_w(re_share_w)
summary_a_w <- prep_summary_w(dots_a_w)
labels_a_w  <- prep_labels_w(summary_a_w)

dots_b_w    <- prep_dots_w(fe_share_w)
summary_b_w <- prep_summary_w(dots_b_w)
labels_b_w  <- prep_labels_w(summary_b_w)

# ══════════════════════════════════════════════════════════════
# 3. PLOT (single panel per variable, no cluster facet)
# ══════════════════════════════════════════════════════════════
make_panel_w <- function(summary_df, dots_df, labels_df, title_str, y_label) {
  shift_seg <- summary_df %>%
    filter(as.character(scenario) == shift_scen) %>%
    mutate(x_ctr = as.numeric(year_f))

  ggplot(summary_df,
         aes(x = year_f, y = median_val, fill = scenario, group = scenario)) +
    geom_col(position = dodge, width = 0.65, alpha = 0.85) +
    geom_segment(data = shift_seg,
                 aes(x = x_ctr - 0.35, xend = x_ctr + 0.35,
                     y = median_val, yend = median_val),
                 colour = "black", linetype = "dashed",
                 linewidth = 0.5, inherit.aes = FALSE) +
    geom_point(data = dots_df,
               aes(y = value, shape = model_lbl, group = scenario),
               colour = "black", size = 2.8, alpha = 0.45, position = dodge) +
    geom_text(data = labels_df,
              aes(x = year_f, y = median_val, group = scenario, label = label_text),
              position  = position_dodge(width = 0.7),
              vjust = -0.4, hjust = -0.1, size = 4.5, fontface = "bold",
              colour = "grey20", inherit.aes = FALSE) +
    scale_fill_manual(values = scenario_colors, labels = scenario_labels, name = NULL) +
    scale_shape_manual(values = model_shapes, name = NULL) +
    labs(title = title_str, x = NULL, y = y_label) +
    guides(
      fill  = guide_legend(order = 1, ncol = 1),
      shape = guide_legend(order = 2, ncol = ceiling(length(model_shapes) / 4), byrow = TRUE)
    )
}

p_a_w <- make_panel_w(summary_a_w, dots_a_w, labels_a_w,
                      "(a) Renewable electricity share",
                      "% of total electricity generation")

p_b_w <- make_panel_w(summary_b_w, dots_b_w, labels_b_w,
                      "(b) Electricity share in final energy",
                      "% of final energy demand")

fig_w <- (p_a_w + theme(legend.position = "bottom", legend.box = "horizontal")) +
         (p_b_w + theme(legend.position = "bottom", legend.box = "horizontal")) +
         plot_layout(guides = "collect")

out_file_w <- file.path(graphs_dir, "fig5_temp_nocluster.png")
ggsave(out_file_w, fig_w, width = 14, height = 6, dpi = 300)
message("Saved: ", out_file_w)