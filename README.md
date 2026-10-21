# PRISMA WP6 Analysis — Replication Package

Standalone replication package for the PRISMA WP6 scenario analysis paper figures.
Contains all scripts, committed data snapshots, and a one-command runner.

---

## Requirements

### R (>= 4.3)

Install the following packages before running:

```r
install.packages(c(
  "tidyverse",   # data manipulation and ggplot2
  "arrow",       # read Parquet files
  "patchwork",   # compose multi-panel figures
  "ggh4x",       # extended ggplot2 axes/facets
  "officer",     # write figure captions to Word (.docx)
  "jsonlite"     # JSON encoding for Python download calls
))
```

### Python (>= 3.10) — for data download and fig_sm1.ipynb

A miniconda installation with a `pypsa-eur` environment is expected at:

```
C:\Users\<username>\miniconda3\envs\pypsa-eur\
```

Required Python packages (install inside the environment):

```bash
conda activate pypsa-eur
pip install requests pandas pyarrow pyreadr
```

The Jupyter executable used by `run_all.R` is:

```
C:\Users\<username>\miniconda3\envs\pypsa-eur\Scripts\jupyter.exe
```

If your installation is at a different path, edit the `jupyter_exe` variable in
`scripts/run_all.R`.

---

## Data

### Committed snapshots (`data/backup/`)

Parquet snapshots downloaded from the PRISMA database are committed to this
repository in `data/backup/`. These are used automatically when no fresh download
is present in `data/direct/`.

Files keep their original timestamped filenames (e.g.,
`post_co2_total_20261021-081800.parquet`) so that `load_pq()` can identify the
most-recent version by modification time.

### Fresh downloads (`data/direct/`)

Each figure script calls `run_python_download()` at startup to check whether
today's data is already present. If not, it downloads from the PRISMA API and
saves to `data/direct/`. This folder is **gitignored** — only backup snapshots
are committed.

Scripts fall back silently to `data/backup/` if `data/direct/` is empty or the
download fails.

### Protocol Excel file

`scripts/fig_sm1.ipynb` reads `PRISMA_WP6_Scenario_Protocol.xlsx` from the
**repository root**. Place this file there before running the notebook:

```
prisma_wp6_analysis/
└── PRISMA_WP6_Scenario_Protocol.xlsx   ← place here
```

---

## How to run

```r
setwd("C:/path/to/prisma_wp6_analysis")
source("scripts/run_all.R")
```

`run_all.R` will:

1. Execute `scripts/fig_sm1.ipynb` via `jupyter nbconvert` (requires pypsa-eur env).
2. Run each figure script (`fig1.R` through `fig_sm8.R`) in alphabetical order.
3. Write `graphs/summary.html` — an HTML gallery of all produced PNG files.
4. Regenerate `graphs/figure_captions.docx` with up-to-date captions.

Individual scripts can also be run directly:

```r
setwd("C:/path/to/prisma_wp6_analysis")
source("scripts/fig2.R")
```

---

## Figure list

| Script | Output | Description |
|---|---|---|
| `fig_sm1.ipynb` | `fig_sm1.png` | SM1 — PRISMA WP6 country cluster map |
| `fig1.R` | `fig1.png` | Figure 1 — Scenario design diagram (4 scenarios in policy × investment space) |
| `fig2.R` | `fig2.png` | Figure 2 — World CO2 emissions band (a) and temperature boxplots (b) |
| `fig3.R` | `fig3.png`, `fig_sm4.png`, `fig_sm5.png` | Figure 3 — Economic impacts: GDP timeseries, NPV world, NPV by cluster |
| `fig4.R` | `fig4.png` | Figure 4 — Brutschin feasibility indicator dots: SSP2 – Late Reawakening |
| `fig5.R` | `fig5.png` | Figure 5 — Renewable and electricity shares by PRISMA cluster (2030/2040/2050) |
| `fig_sm2.R` | `fig_sm2.png` | SM2 — Temperature, cumulative CO2, ERF, and CO2 by PRISMA cluster |
| `fig_sm3.R` | `fig_sm3.png` | SM3 — GDP NPV discount rate sensitivity (T50, 1%–4%) |
| `fig_sm6.R` | `fig_sm6.png` | SM6 — GDP trajectories vs Meet Aspirations by cluster and scenario |
| `fig_sm7.R` | `fig_sm7.png` | SM7 — Brutschin feasibility indicator dots: three scenarios |
| `fig_sm8.R` | `fig_sm8.png` | SM8 — H&S (2025) damage function sensitivity: GDP NPV vs Meet Aspirations |

All outputs are written to `graphs/`.

---

## Support scripts

| File | Purpose |
|---|---|
| `scripts/prisma_styles.R` | PRISMA colour palette, scenario labels, ggplot2 theme |
| `scripts/model_fixes.R` | Model name harmonisation and year-fix utilities |
| `scripts/carbon_price_loader.R` | Loads and harmonises carbon price data |
| `scripts/download_prisma.py` | Downloads variables from the PRISMA IIASA database |
| `scripts/captions.R` | Central figure caption store; writes `figure_captions.docx` |

---

## Citation / data source

Data are downloaded from the **PRISMA project** database hosted on IIASA.
Scenarios: SSP2 – Meet Aspirations, SSP2 – Shifted Ambition,
SSP2 – Late Reawakening, SSP2 – Tech Momentum.

Climate diagnostics computed with **MAGICC v7.6.0a3**.

---

## Notes

- Temperature and climate data require the MAGICC output parquets
  (`climate_assess_full_*.parquet`, `f15_ca_all_*.parquet`, etc.).
  If those are not present in `data/backup/`, they will be downloaded on first run.
- `fig3.R` produces three outputs: `fig3.png`, `fig_sm4.png`, and `fig_sm5.png`.
- IMACLIM 2.0 has not submitted SSP2 – Staying Alive; that model/scenario
  combination is excluded from all completeness checks.
