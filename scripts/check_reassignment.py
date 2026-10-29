import warnings; warnings.filterwarnings("ignore")
import numpy as np, pandas as pd

PATH_PROTOCOL = r"C:\Users\Dibella\Desktop\CMCC\prisma_diagnostics\PRISMA_WP6_Scenario_Protocol.xlsx"
PATH_DATA     = "data/PRISMA_data_protocol.xlsx"

REASSIGN = ["BEL", "MLT", "CYP", "LVA"]

# ── Load WP6 protocol (already has GHG_intensity + the indicator columns) ────
df_wp6 = pd.read_excel(PATH_PROTOCOL, sheet_name="WP6_classification")
df_wp6 = df_wp6.rename(columns={"iso3_code": "Code", "country_name": "Country",
                                  "Diversifying economies": "Cluster"})
df_wp6["Code"] = df_wp6["Code"].astype(str).str.strip().str.upper()
df_wp6 = df_wp6[df_wp6["Code"].str.len() == 3].dropna(subset=["Cluster"]).copy()
for c in ["GHG_intensity"]:
    if c in df_wp6.columns:
        df_wp6[c] = pd.to_numeric(df_wp6[c], errors="coerce")

# ── Get GDP_PPP from the data protocol ────────────────────────────────────────
df_data = pd.read_excel(PATH_DATA, sheet_name=0, skiprows=8)
df_data = df_data.replace({"..": np.nan, "#VALUE!": np.nan, "#N/A": np.nan, "": np.nan})
df_data = df_data.rename(columns={
    "Country code":                             "Code",
    "GDP, PPP (constant 2021 international $)": "GDP_PPP",
})
df_data["Code"] = df_data["Code"].astype(str).str.strip().str.upper()
df_data = df_data[df_data["Code"].str.len() == 3].drop_duplicates(subset=["Code"]).copy()
df_data["GDP_PPP"] = pd.to_numeric(df_data["GDP_PPP"], errors="coerce")

# Merge GDP only
merged = df_wp6.merge(df_data[["Code", "GDP_PPP"]], on="Code", how="left")

# GHG proxy: intensity (tCO2e / $GDP) × GDP_PPP → total GHG (proportional)
merged["GHG_proxy"] = merged["GHG_intensity"] * merged["GDP_PPP"]

# ── Print current assignments for the four countries ─────────────────────────
print("Current WP6 assignments — four candidate countries:")
print(f"{'Code':<5} {'Country':<28} {'Cluster':<28} {'GDP_PPP (bn$)':>14} {'GHG_intensity':>14} {'GHG_proxy':>12}")
print("-" * 105)
for code in REASSIGN:
    row = merged[merged["Code"] == code]
    if row.empty:
        print(f"  {code} — not found"); continue
    r = row.iloc[0]
    gdp = r["GDP_PPP"] / 1e9 if pd.notna(r["GDP_PPP"]) else float("nan")
    print(f"  {r['Code']:<4} {str(r['Country'])[:27]:<28} {str(r['Cluster']):<28} "
          f"{gdp:>14.1f} {r['GHG_intensity']:>14.4f} {r['GHG_proxy']:>12.2f}")

# ── Emissions shares ──────────────────────────────────────────────────────────
total_global     = merged["GHG_proxy"].sum()
total_transition = merged[merged["Cluster"] == "Transition leaders"]["GHG_proxy"].sum()
total_4          = merged[merged["Code"].isin(REASSIGN)]["GHG_proxy"].sum()

print()
print("Emissions proxy — share of WP6-assigned countries total:")
print(f"  BEL + MLT + CYP + LVA combined:   {total_4:>12.2f}  = {100*total_4/total_global:.3f}% of WP6-country total")
print(f"  Current Transition Leaders total:  {total_transition:>12.2f}  = {100*total_transition/total_global:.2f}%")
print(f"  All WP6 countries total:           {total_global:>12.2f}")
new_transition = total_transition + total_4
print(f"  Transition Leaders AFTER reassign: {new_transition:>12.2f}  "
      f"(+{100*(new_transition-total_transition)/total_transition:.2f}% change)")

# ── Current Transition Leaders for context ────────────────────────────────────
print()
print("Current Transition Leaders (for context):")
tl = (merged[merged["Cluster"] == "Transition leaders"]
      [["Code","Country","GHG_proxy"]]
      .dropna(subset=["GHG_proxy"])
      .sort_values("GHG_proxy", ascending=False))
for _, r in tl.iterrows():
    pct = 100 * r["GHG_proxy"] / total_global
    print(f"  {r['Code']:<4} {str(r['Country'])[:35]:<36}  GHG_proxy={r['GHG_proxy']:>10.2f}  ({pct:.3f}%)")
print(f"\n  ─── Adding: BEL+MLT+CYP+LVA ──── GHG_proxy={total_4:>10.2f}  ({100*total_4/total_global:.3f}%)")
