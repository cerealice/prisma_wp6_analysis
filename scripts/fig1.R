library(tidyverse)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 figure 1
#   a) Scenario design diagram: four PRISMA scenarios positioned
#      in the (climate policy ambition × clean-technology investment)
#      space, Late Reawakening in the centre as the intermediate case.
#   b) Differentiated shift by cluster: every milestone moved
#      later by 10 / 20 / 30 years, cluster colours as in SM1.
#
# Output: graphs/fig1.png
# ══════════════════════════════════════════════════════════════

# ── Paths ──────────────────────────────────────────────────────
wd          <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- if (basename(dirname(wd)) == "scripts") dirname(dirname(wd)) else wd
scripts_dir <- file.path(project_dir, "scripts")
graphs_dir  <- file.path(project_dir, "graphs")
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)

# ── PRISMA colours ─────────────────────────────────────────────
col_meet    <- "#5BB85C"
col_tech    <- "#87CEEB"
col_shifted <- "#999999"
col_late    <- "#D62728"

fill_meet    <- "#E8F8E8"
fill_tech    <- "#EAF4FC"
fill_shifted <- "#F0F0F0"

# ── Cluster colours — must match fig_sm1 (country cluster map) ──
col_leaders <- "#6B7FD7"   # transition leaders
col_divers  <- "#F0885C"   # diversifying economies
col_fossil  <- "#45BE9B"   # fossil-dependent economies

# ── Panel b geometry ───────────────────────────────────────────
# Panel b lives on the same canvas as panel a, to the right of
# x = b_x0. Arrow length is proportional to the shift, so the
# 10 / 20 / 30 gradient reads before the labels do.
b_x0   <- 11.9          # left edge of panel b
b_dot  <- b_x0 + 0.25   # stated milestone marker
b_rows <- c(8.5, 6.4, 4.3)                       # block tops (cluster names)
b_shift_scale <- 0.115                           # canvas units per year

clusters_b <- tibble(
  name   = c("Transition leaders", "Diversifying economies",
             "Fossil-dependent economies"),
  colour = c(col_leaders, col_divers, col_fossil),
  shift  = c(10, 20, 30),
  y_name = b_rows,
  y_arrow = b_rows - 0.78
) |>
  mutate(
    x_end    = b_dot + shift * b_shift_scale,
    lab_x    = x_end + 0.22,
    lab_text = paste0("+", shift, " years")
  )

# ── Rounded box helper ─────────────────────────────────────────
# annotate("rect") draws square corners only, so the scenario boxes
# are grid roundrect grobs placed in data coordinates. The radius is
# an absolute length, so every box gets the same physical rounding.
rbox <- function(xmin, xmax, ymin, ymax, fill, colour, linewidth,
                 radius = unit(4, "mm")) {
  annotation_custom(
    grob = grid::roundrectGrob(
      r  = radius,
      gp = grid::gpar(fill = fill, col = colour, lwd = linewidth * .pt)
    ),
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
  )
}

# ── Canvas ─────────────────────────────────────────────────────
# Panel a: x 0–10.2,  Panel b: x 11.9–17.6
# RQ legend below panel a: y –3.0 to 0
p <- ggplot(data.frame(x = c(-1.15, 17.6), y = c(-3.05, 10.6)), aes(x, y)) +
  geom_blank() +
  coord_cartesian(xlim = c(-1.15, 17.6), ylim = c(-3.05, 10.6), clip = "off") +

  # ════════════════════ PANEL A ═══════════════════════════════

  # ── Boxes ───────────────────────────────────────────────────
  # The three pure scenarios sit in their quadrants. Late Reawakening
  # is intermediate in both dimensions, so it occupies the centre.

  # Tech Momentum — top-left (blue)
  rbox(0.2, 4.8, 6.3, 9.9, fill_tech, col_tech, 1.9) +

  # Meet Aspirations — top-right (green)
  rbox(5.2, 9.8, 6.3, 9.9, fill_meet, col_meet, 2.3) +

  # Shifted Ambition — bottom-left (grey)
  rbox(0.2, 4.8, 0.6, 3.7, fill_shifted, col_shifted, 1.9) +

  # Late Reawakening — centre, straddling both dimensions (red)
  rbox(3.05, 6.95, 3.85, 6.15, "white", col_late, 2.1) +

  # ── Box titles ─────────────────────────────────────────────

  annotate("text", x = 2.5, y = 9.15,
           label = "Tech Momentum",
           size = 7.8, fontface = "bold", colour = "#1565C0") +

  annotate("text", x = 7.5, y = 9.15,
           label = "Meet Aspirations",
           size = 7.8, fontface = "bold", colour = "#2E7D32") +

  annotate("text", x = 2.5, y = 2.95,
           label = "Shifted Ambition",
           size = 7.8, fontface = "bold", colour = "#555555") +

  annotate("text", x = 5.0, y = 5.50,
           label = "Late Reawakening",
           size = 7.0, fontface = "bold", colour = col_late) +

  # ── Box descriptions ────────────────────────────────────────

  annotate("text", x = 2.5, y = 7.60,
           label = "Ambition withdrawn\nInvestment held",
           size = 6.3, colour = "grey30", lineheight = 1.3) +

  annotate("text", x = 7.5, y = 7.60,
           label = "NDCs and LTS\nimplemented",
           size = 6.3, colour = "grey30", lineheight = 1.3) +

  annotate("text", x = 2.5, y = 1.50,
           label = "Milestones shifted\n10 / 20 / 30 yr",
           size = 6.3, colour = "grey30", lineheight = 1.3) +

  # first line split so that the pivot year carries the scenario colour
  annotate("text", x = 5.88, y = 4.73,
           label = "Shifted Ambition to", hjust = 1,
           size = 4.6, colour = "grey20") +

  annotate("text", x = 5.95, y = 4.73,
           label = "2035,", hjust = 0,
           size = 4.6, fontface = "bold", colour = col_late) +

  annotate("text", x = 5.0, y = 4.27,
           label = "then Meet Aspirations",
           size = 4.6, colour = "grey20") +

  # ── Where Late Reawakening comes from and ends up ───────────

  annotate("segment",
           x = 4.90, y = 3.22, xend = 5.95, yend = 3.77,
           arrow  = arrow(length = unit(0.26, "cm"), type = "closed"),
           colour = col_late, linewidth = 1.3) +

  annotate("segment",
           x = 7.05, y = 5.65, xend = 8.10, yend = 6.20,
           arrow  = arrow(length = unit(0.26, "cm"), type = "closed"),
           colour = col_late, linewidth = 1.3) +

  # ── Axes (manual arrows on white background) ─────────────────

  annotate("segment",
           x = 0, xend = 10.15, y = 0, yend = 0,
           arrow    = arrow(length = unit(0.28, "cm"), type = "closed"),
           colour   = "grey25", linewidth = 0.95) +

  annotate("segment",
           x = 0, xend = 0, y = 0, yend = 10.45,
           arrow    = arrow(length = unit(0.28, "cm"), type = "closed"),
           colour   = "grey25", linewidth = 0.95) +

  annotate("text", x = 5.05, y = -0.62,
           label = "Climate policy ambition",
           size = 6.6, colour = "grey25") +

  annotate("text", x = 0.35, y = -0.62,
           label = "Low", hjust = 0,
           size = 5.2, colour = "grey45") +
  annotate("text", x = 9.95, y = -0.62,
           label = "High", hjust = 1,
           size = 5.2, colour = "grey45") +

  annotate("text", x = -0.6, y = 5.25,
           label = "Clean-technology investment",
           size = 6.6, colour = "grey25", angle = 90) +

  annotate("text", x = -0.6, y = 0.35,
           label = "Low", hjust = 0,
           size = 5.2, colour = "grey45", angle = 90) +
  annotate("text", x = -0.6, y = 10.2,
           label = "High", hjust = 1,
           size = 5.2, colour = "grey45", angle = 90) +

  # ── RQ legend ───────────────────────────────────────────────

  annotate("text", x = 0.2, y = -1.35,
           label = "RQ1", hjust = 0,
           size = 6.3, fontface = "bold", colour = "grey20") +
  annotate("text", x = 1.6, y = -1.35,
           label = "Shifted Ambition vs Meet Aspirations",
           hjust = 0, size = 6.3, colour = "grey20") +

  annotate("text", x = 0.2, y = -2.05,
           label = "RQ2", hjust = 0,
           size = 6.3, fontface = "bold", colour = col_late) +
  annotate("text", x = 1.6, y = -2.05,
           label = "Late Reawakening vs Meet Aspirations",
           hjust = 0, size = 6.3, colour = "grey20") +

  annotate("text", x = 0.2, y = -2.75,
           label = "RQ3", hjust = 0,
           size = 6.3, fontface = "bold", colour = "#1565C0") +
  annotate("text", x = 1.6, y = -2.75,
           label = "Tech Momentum vs Shifted Ambition",
           hjust = 0, size = 6.3, colour = "grey20") +

  # ════════════════════ PANEL B ═══════════════════════════════

  # Divider between the two panels
  annotate("segment",
           x = b_x0 - 0.75, xend = b_x0 - 0.75, y = -2.55, yend = 10.2,
           colour = "grey88", linewidth = 0.7) +

  # Header
  annotate("text", x = b_x0, y = 9.95,
           label = "Shifted Ambition",
           hjust = 0, size = 6.0, fontface = "bold", colour = "#555555") +

  annotate("text", x = b_x0, y = 9.35,
           label = "Each cluster shifts every milestone by",
           hjust = 0, size = 5.2, colour = "grey25") +

  # Cluster rows — name, stated-milestone marker, arrow, shift label
  geom_text(data = clusters_b,
            aes(x = b_x0, y = y_name, label = name, colour = colour),
            hjust = 0, size = 6.5, fontface = "bold",
            inherit.aes = FALSE, show.legend = FALSE) +

  geom_point(data = clusters_b,
             aes(x = b_dot, y = y_arrow),
             shape = 21, fill = "white", colour = "grey35",
             size = 3.4, stroke = 1.0, inherit.aes = FALSE) +

  geom_segment(data = clusters_b,
               aes(x = b_dot + 0.12, xend = x_end,
                   y = y_arrow, yend = y_arrow, colour = colour),
               arrow = arrow(length = unit(0.26, "cm"), type = "closed"),
               linewidth = 1.5, inherit.aes = FALSE, show.legend = FALSE) +

  geom_text(data = clusters_b,
            aes(x = lab_x, y = y_arrow, label = lab_text, colour = colour),
            hjust = 0, size = 6.1, fontface = "bold",
            inherit.aes = FALSE, show.legend = FALSE) +

  scale_colour_identity() +

  # ── Tech Momentum: same shifts, investment held ─────────────

  annotate("segment",
           x = b_x0, xend = b_x0 + 5.3, y = 3.00, yend = 3.00,
           colour = "grey85", linewidth = 0.6) +

  annotate("text", x = b_x0, y = 2.55,
           label = "Tech Momentum",
           hjust = 0, size = 6.0, fontface = "bold", colour = "#1565C0") +

  annotate("text", x = b_x0, y = 1.95,
           label = "Same shifts by cluster, but clean-technology",
           hjust = 0, size = 4.8, colour = "grey25") +

  annotate("text", x = b_x0, y = 1.45,
           label = "investment held at Meet Aspirations levels",
           hjust = 0, size = 4.8, colour = "grey25") +

  # ── Late Reawakening: the same shift for every cluster ──────

  annotate("segment",
           x = b_x0, xend = b_x0 + 5.3, y = 0.95, yend = 0.95,
           colour = "grey85", linewidth = 0.6) +

  annotate("text", x = b_x0, y = 0.50,
           label = "Late Reawakening",
           hjust = 0, size = 6.0, fontface = "bold", colour = col_late) +

  annotate("point", x = b_dot, y = -0.28,
           shape = 21, fill = "white", colour = "grey35",
           size = 3.4, stroke = 1.0) +

  annotate("segment",
           x = b_dot + 0.12, xend = b_dot + 10 * b_shift_scale,
           y = -0.28, yend = -0.28,
           arrow = arrow(length = unit(0.26, "cm"), type = "closed"),
           colour = col_late, linewidth = 1.5) +

  annotate("text", x = b_dot + 10 * b_shift_scale + 0.22, y = -0.28,
           label = "+10 years, every cluster",
           hjust = 0, size = 5.8, fontface = "bold", colour = col_late) +

  # Footnotes
  annotate("segment",
           x = b_x0, xend = b_x0 + 5.3, y = -1.00, yend = -1.00,
           colour = "grey85", linewidth = 0.6) +

  annotate("text", x = b_x0, y = -1.55,
           label = "Milestones shifted: NDC 2030, NDC 2035, LTS",
           hjust = 0, size = 4.6, colour = "grey25") +

  annotate("text", x = b_x0, y = -2.15,
           label = "LTS also attenuated to 80% of the stated level",
           hjust = 0, size = 4.6, colour = "grey25") +

  # ── Theme ──────────────────────────────────────────────────
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(4, 6, 4, 8)
  )

out_file <- file.path(graphs_dir, "fig1.png")
ggsave(out_file, p, width = 14.0, height = 7.2, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig1", graphs_dir)
  }
})
