library(tidyverse)

# ══════════════════════════════════════════════════════════════
# PURPOSE — post-Oct-15 figure 1
#   Scenario design diagram: four PRISMA scenarios positioned
#   in the (climate policy ambition × clean-technology investment)
#   space, with a curved arrow tracing Late Reawakening's path.
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

# ── Canvas ─────────────────────────────────────────────────────
# Main area: x 0–10, y 0–10.2
# RQ legend below: y –3 to 0
p <- ggplot(data.frame(x = c(-2.0, 10.5), y = c(-3.3, 10.9)), aes(x, y)) +
  geom_blank() +
  coord_cartesian(xlim = c(-2.0, 10.5), ylim = c(-3.3, 10.9), clip = "off") +

  # ── Boxes ───────────────────────────────────────────────────

  # Tech Momentum — top-left (blue)
  annotate("rect", xmin = 0.2, xmax = 4.8, ymin = 5.4, ymax = 9.9,
           fill = fill_tech, colour = col_tech, linewidth = 1.9) +

  # Meet Aspirations — top-right (green)
  annotate("rect", xmin = 5.2, xmax = 9.8, ymin = 5.4, ymax = 9.9,
           fill = fill_meet, colour = col_meet, linewidth = 2.3) +

  # Shifted Ambition — bottom-left (grey)
  annotate("rect", xmin = 0.2, xmax = 4.8, ymin = 0.6, ymax = 5.0,
           fill = fill_shifted, colour = col_shifted, linewidth = 1.9) +

  # Late Reawakening — bottom-right: text only, no box

  # ── Box titles ─────────────────────────────────────────────

  annotate("text", x = 2.5, y = 9.3,
           label = "Tech Momentum",
           size = 7.8, fontface = "bold", colour = "#1565C0") +

  annotate("text", x = 7.5, y = 9.3,
           label = "Meet Aspirations",
           size = 7.8, fontface = "bold", colour = "#2E7D32") +

  annotate("text", x = 2.5, y = 4.42,
           label = "Shifted Ambition",
           size = 7.8, fontface = "bold", colour = "#555555") +

  # ── Box descriptions ────────────────────────────────────────

  annotate("text", x = 2.5, y = 7.85,
           label = "Ambition withdrawn\nInvestment held",
           size = 6.3, colour = "grey30", lineheight = 1.3) +

  annotate("text", x = 7.5, y = 7.85,
           label = "NDCs and LTS\nImplemented",
           size = 6.3, colour = "grey30", lineheight = 1.3) +

  annotate("text", x = 2.5, y = 3.05,
           label = "Milestones deferred\n10 / 20 / 30 yr",
           size = 6.3, colour = "grey30", lineheight = 1.3) +

  # ── Late Reawakening ────────────────────────────────────────

  annotate("text", x = 8.1, y = 4.42,
           label = "Late Reawakening",
           size = 7.8, fontface = "bold", colour = col_late) +

  annotate("text", x = 8.1, y = 3.3,
           label = "Shifted Ambition until 2035,\nthen resumes Meet Aspirations",
           size = 4.6, colour = "grey20", lineheight = 1.32) +

  # ── Curved arrow — 2035 → from 2040 ─────────────────────────

  annotate("text", x = 5.4, y = 2.4,
           label = "2035",
           size = 6.6, fontface = "bold", colour = col_late) +

  annotate("text", x = 5.45, y = 6.65,
           label = "from 2040",
           size = 6.1, fontface = "bold", colour = col_late, hjust = 0) +

  annotate("curve",
           x = 4.8, y = 2.8, xend = 5.35, yend = 6.55,
           curvature = 0.35,
           arrow     = arrow(length = unit(0.30, "cm"), type = "closed"),
           colour    = col_late, linewidth = 1.45) +

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

  annotate("text", x = -0.6, y = 5.25,
           label = "Clean-technology investment",
           size = 6.6, colour = "grey25", angle = 90) +

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

  # ── Theme ──────────────────────────────────────────────────
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(10, 20, 10, 55)
  )

out_file <- file.path(graphs_dir, "fig1.png")
ggsave(out_file, p, width = 9.5, height = 7.5, dpi = 300)
message("Saved: ", out_file)

# ── Update figure captions Word document ──────────────────────
local({
  cap_script <- file.path(scripts_dir, "captions.R")
  if (file.exists(cap_script)) {
    source(cap_script, local = TRUE)
    update_caption("fig1", graphs_dir)
  }
})
