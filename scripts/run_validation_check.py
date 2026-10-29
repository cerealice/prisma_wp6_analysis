import warnings; warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from sklearn.mixture import GaussianMixture
from sklearn.preprocessing import StandardScaler

PATH = "data/PRISMA_data_protocol.xlsx"
PATH_PROTOCOL = r"C:\Users\Dibella\Desktop\CMCC\prisma_diagnostics\PRISMA_WP6_Scenario_Protocol.xlsx"
WINSOR_LO, WINSOR_HI = 0.02, 0.98

# ── Refit GMM (same settings as notebook cell 3) ──
df_all = pd.read_excel(PATH, sheet_name=0, skiprows=8)
df_all = df_all.replace({"..": np.nan, "#VALUE!": np.nan, "#N/A": np.nan, "": np.nan})
df_all = df_all.rename(columns={"Country code": "Code", "FF/GDP": "FF_GDP", "RES grwth": "RES_growth"})
df_all["Code"] = df_all["Code"].astype(str).str.strip().str.upper()
df_all = df_all.dropna(subset=["Code"])
df_all = df_all[df_all["Code"].str.len() == 3].copy()
df_all = df_all.drop_duplicates(subset=["Code"], keep="first").copy()
for c in ["FF_GDP", "RES_growth", "GHG_intensity"]:
    df_all[c] = pd.to_numeric(df_all[c], errors="coerce")
possible_fossil_cols = ["Oil", "Natural gas", "Coal", "Total fossil fuels"]
fossil_cols = [c for c in possible_fossil_cols if c in df_all.columns]
if fossil_cols:
    for c in fossil_cols: df_all[c] = pd.to_numeric(df_all[c], errors="coerce")
    fossil_sum = df_all[fossil_cols].fillna(0).sum(axis=1)
    df_all.loc[(df_all["FF_GDP"] == 0) & (fossil_sum == 0), "FF_GDP"] = np.nan

raw_features = ["FF_GDP", "RES_growth", "GHG_intensity"]
fit_mask = df_all[raw_features].notna().all(axis=1)
df_fit = df_all.loc[fit_mask, ["Code", "Country"] + raw_features].copy()
df_fit["FF_GDP_t"] = np.log1p(df_fit["FF_GDP"])
df_fit["GHG_intensity_t"] = np.log1p(df_fit["GHG_intensity"])
df_fit["RES_growth_t"] = df_fit["RES_growth"]
features_t = ["FF_GDP_t", "RES_growth_t", "GHG_intensity_t"]
for c in features_t:
    lo, hi = df_fit[c].quantile([WINSOR_LO, WINSOR_HI])
    df_fit[c] = df_fit[c].clip(lo, hi)
scaler = StandardScaler()
X_scaled = scaler.fit_transform(df_fit[features_t].to_numpy())
gmm = GaussianMixture(n_components=3, covariance_type="diag", random_state=42,
                      n_init=50, init_params="kmeans", reg_covar=1e-6)
df_fit["_cluster"] = gmm.fit_predict(X_scaled)

centroids_t = pd.DataFrame(scaler.inverse_transform(gmm.means_), columns=features_t)
centroids_t["cluster"] = np.arange(3)
centroids_t["FF_GDP_center"] = np.expm1(centroids_t["FF_GDP_t"])
centroids_t["GHG_intensity_center"] = np.expm1(centroids_t["GHG_intensity_t"])
centroids_t["RES_growth_center"] = centroids_t["RES_growth_t"]
fossil_cluster = int(centroids_t.sort_values("FF_GDP_center", ascending=False).iloc[0]["cluster"])
remaining = centroids_t[centroids_t["cluster"] != fossil_cluster].copy()
remaining["transition_score"] = (
    remaining["RES_growth_center"].rank(ascending=False)
    + remaining["GHG_intensity_center"].rank(ascending=True)
    + remaining["FF_GDP_center"].rank(ascending=True)
)
transition_cluster = int(remaining.sort_values("transition_score").iloc[0]["cluster"])
diversifying_cluster = int([c for c in [0,1,2] if c not in [fossil_cluster, transition_cluster]][0])
label_map = {
    fossil_cluster:      "Fossil-dependent economies",
    diversifying_cluster:"Diversifying economies",
    transition_cluster:  "Transition leaders",
}

# ── Load WP6 protocol ──
df_wp6v = pd.read_excel(PATH_PROTOCOL, sheet_name="WP6_classification")
df_wp6v = df_wp6v.rename(columns={"iso3_code": "Code", "country_name": "Country",
                                   "Diversifying economies": "Cluster"})
df_wp6v["Code"] = df_wp6v["Code"].astype(str).str.strip().str.upper()
df_wp6v = df_wp6v[df_wp6v["Code"].str.len() == 3].copy()
df_wp6v = df_wp6v.dropna(subset=["Cluster"])
df_wp6v = df_wp6v.drop_duplicates(subset=["Code"], keep="first").copy()
for c in ["FF_GDP", "RES_growth", "GHG_intensity"]:
    df_wp6v[c] = pd.to_numeric(df_wp6v[c], errors="coerce")
df_wp6v["n_missing"] = df_wp6v[["FF_GDP","RES_growth","GHG_intensity"]].isna().sum(axis=1)
df_partial = df_wp6v[df_wp6v["n_missing"] > 0].copy()

# ── Marginal posterior ──
def gmm_marginal_posterior(row, gmm, scaler, features_t):
    obs = {}
    if pd.notna(row.get("FF_GDP")) and row["FF_GDP"] > 0:
        obs["FF_GDP_t"] = np.log1p(row["FF_GDP"])
    if pd.notna(row.get("RES_growth")):
        obs["RES_growth_t"] = float(row["RES_growth"])
    if pd.notna(row.get("GHG_intensity")) and row["GHG_intensity"] > 0:
        obs["GHG_intensity_t"] = np.log1p(row["GHG_intensity"])
    avail = [(f, features_t.index(f)) for f in obs if f in features_t]
    if not avail:
        return None, 0
    feat_names, feat_idx = zip(*avail); feat_idx = list(feat_idx)
    x_raw = np.array([obs[f] for f in feat_names], dtype=float)
    x_sc = (x_raw - scaler.mean_[feat_idx]) / scaler.scale_[feat_idx]
    log_p = np.zeros(gmm.n_components)
    for k in range(gmm.n_components):
        mu_k = gmm.means_[k][feat_idx]; var_k = gmm.covariances_[k][feat_idx]
        diff = x_sc - mu_k
        log_p[k] = (np.log(gmm.weights_[k])
                    - 0.5*np.sum(diff**2/var_k)
                    - 0.5*np.sum(np.log(2.0*np.pi*var_k)))
    log_p -= log_p.max(); probs = np.exp(log_p)
    return probs / probs.sum(), len(avail)

cluster_order = [label_map[k] for k in range(gmm.n_components)]
SHORT = {"Fossil-dependent economies": "Fossil",
         "Diversifying economies":     "Diversifying",
         "Transition leaders":         "Transition"}

records = []
for _, row in df_partial.iterrows():
    probs, n_feat = gmm_marginal_posterior(row, gmm, scaler, features_t)
    base = dict(Code=str(row["Code"]), Country=str(row["Country"]),
                WP6=str(row["Cluster"]), n_feat=int(n_feat))
    if probs is None:
        records.append({**base, "Model": "—", "max_prob": float("nan"), "agrees": 0,
                        **{c: float("nan") for c in cluster_order}})
    else:
        model_cl = cluster_order[int(np.argmax(probs))]
        records.append({**base, "Model": model_cl,
                        "max_prob": float(probs.max()),
                        "agrees": int(str(row["Cluster"]) == str(model_cl)),
                        **{c: float(v) for c, v in zip(cluster_order, probs)}})

df_val = pd.DataFrame(records)
def tier(r):
    if r["n_feat"] == 0: return "untestable"
    if r["agrees"] != 1: return "mismatch"
    if r["max_prob"] < 0.60: return "border"
    return "confirmed"
df_val["tier"] = df_val.apply(tier, axis=1)
df_testable = df_val[df_val["n_feat"] > 0].copy()

print("=== VALIDATION SUMMARY ===")
print(f"Total expert-assigned (hatched):   {len(df_partial)}")
print(f"Testable (>=1 feature available):  {len(df_testable)}")
print(f"Untestable (no features at all):   {(df_val['n_feat']==0).sum()}")
print()
print("Tier counts:")
for t, n in df_val["tier"].value_counts().items():
    print(f"  {t}: {n}")
print()
print("=== MISMATCHES ===")
mm = df_testable[df_testable["agrees"]==0].sort_values("max_prob")
print(f"  ({len(mm)} total)")
for _, r in mm.iterrows():
    probs_s = "  ".join(f"{SHORT.get(c,c)[:5]}:{r[c]:.2f}" for c in cluster_order if c in r and pd.notna(r[c]))
    print(f"  {r['Code']:<4} {r['Country'][:28]:<28} WP6={SHORT.get(r['WP6'],'?'):<14}"
          f" GMM={SHORT.get(r['Model'],'?'):<14} [{probs_s}]  n_feat={r['n_feat']}")
print()
print("=== BORDER ZONE (p < 0.60) ===")
bz = df_testable[df_testable["max_prob"] < 0.60].sort_values("max_prob")
print(f"  ({len(bz)} total, includes mismatches that are also low-confidence)")
for _, r in bz.iterrows():
    flag = "mismatch" if r["agrees"] != 1 else "border"
    probs_s = "  ".join(f"{SHORT.get(c,c)[:5]}:{r[c]:.2f}" for c in cluster_order if c in r and pd.notna(r[c]))
    print(f"  [{flag}] {r['Code']:<4} {r['Country'][:26]:<26} WP6={SHORT.get(r['WP6'],'?'):<14}"
          f" p_max={r['max_prob']:.2f}  [{probs_s}]")
