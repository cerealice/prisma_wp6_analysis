#!/usr/bin/env python3
"""
Compute population-weighted annual mean temperature (2015) for IAMC R10 regions
from Gortan et al. (2024) Weighted Climate Dataset.

Method:
  1. Partially download the Figshare ZIP to read its central directory (no full download).
  2. Download only the population-weighted annual temperature CSV file.
  3. Map ISO3 countries to R10 regions.
  4. Compute population-weighted R10 averages for 2015.

Output: R vector ready to paste into fig3.R as r10_baseline_temp.

Reference:
  Gortan M., Testa L., Fagiolo G., Lamperti F. (2024)
  A unified repository for pre-processed climate data weighted by gridded
  economic activity. Scientific Data 11, 533.
  https://weightedclimatedata.streamlit.app/
"""

import struct
import zlib
import sys
import requests
import io

try:
    import pandas as pd
    import numpy as np
except ImportError:
    sys.exit("pandas and numpy are required: pip install pandas numpy")

# ── Figshare direct-download URL ──────────────────────────────────────────────
URL = "https://ndownloader.figshare.com/files/45222226"

# ── IAMC R10 country mapping (ISO3 → R10 region) ─────────────────────────────
# Based on IAMC/SSP standard regional definitions
R10_MAP = {}

_AFRICA = [
    "AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD","COM","COD","COG",
    "CIV","DJI","EGY","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB",
    "KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NAM",
    "NER","NGA","RWA","STP","SEN","SLE","SOM","ZAF","SSD","SDN","TZA","TGO",
    "TUN","UGA","ZMB","ZWE","DZA","LBN","REU","MYT","SHN","ESH","SOM"
]
_CHINA = [
    "CHN","HKG","MAC","TWN","MNG","PRK"
]
_EUROPE = [
    "ALB","AND","AUT","BLR","BEL","BIH","BGR","HRV","CYP","CZE","DNK","EST",
    "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA","XKX","LVA","LIE","LTU",
    "LUX","MLT","MDA","MCO","MNE","NLD","MKD","NOR","POL","PRT","ROU","SMR",
    "SRB","SVK","SVN","ESP","SWE","CHE","TUR","UKR","GBR","VAT","GEO","ARM",
    "AZE"
]
_INDIA = [
    "IND","BGD","BTN","MDV","NPL","PAK","LKA","AFG"
]
_LATIN = [
    "ARG","BLZ","BOL","BRA","CHL","COL","CRI","CUB","DOM","ECU","SLV","GTM",
    "GUY","HTI","HND","JAM","MEX","NIC","PAN","PRY","PER","PRI","SUR","TTO",
    "URY","VEN","ABW","ATG","BHS","BRB","DMA","GRD","GLP","GUF","KNA","LCA",
    "MTQ","VCT","CUW","BES","SXM","VIR","PRI","TCA","CYM","HTI"
]
_MIDDLE_EAST = [
    "BHR","IRQ","IRN","ISR","JOR","KWT","OMN","QAT","SAU","SYR","ARE","YEM",
    "PSE","MLT"
]
_NORTH_AMERICA = [
    "CAN","USA","BMU","GRL","SPM"
]
_PACIFIC_OECD = [
    "AUS","JPN","NZL","KOR"
]
_REFORMING = [
    "ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LVA","LTU","MDA","RUS","TJK",
    "TKM","UKR","UZB"
]
_REST_ASIA = [
    "BRN","KHM","IDN","LAO","MYS","MMR","PHL","SGP","THA","TLS","VNM","FJI",
    "PNG","VUT","SLB","WSM","TON","KIR","FSM","MHL","PLW","NRU","TUV","COK",
    "NIU","NCL","PYF","GUM","MNP","ASM"
]

# Build the map — note: Reforming Economies overlaps with some Europe entries;
# process Europe first then overwrite with Reforming so former-Soviet states
# land in the right bucket.
for iso in _AFRICA:        R10_MAP[iso] = "Africa (R10)"
for iso in _CHINA:         R10_MAP[iso] = "China+ (R10)"
for iso in _INDIA:         R10_MAP[iso] = "India+ (R10)"
for iso in _LATIN:         R10_MAP[iso] = "Latin America (R10)"
for iso in _MIDDLE_EAST:   R10_MAP[iso] = "Middle East (R10)"
for iso in _NORTH_AMERICA: R10_MAP[iso] = "North America (R10)"
for iso in _PACIFIC_OECD:  R10_MAP[iso] = "Pacific OECD (R10)"
for iso in _EUROPE:        R10_MAP[iso] = "Europe (R10)"
for iso in _REFORMING:     R10_MAP[iso] = "Reforming Economies (R10)"
for iso in _REST_ASIA:     R10_MAP[iso] = "Rest of Asia (R10)"

R10_REGIONS = [
    "Africa (R10)", "China+ (R10)", "Europe (R10)", "India+ (R10)",
    "Latin America (R10)", "Middle East (R10)", "North America (R10)",
    "Other (R10)", "Pacific OECD (R10)", "Reforming Economies (R10)",
    "Rest of Asia (R10)"
]

# ── ZIP partial-download helpers ──────────────────────────────────────────────

def range_get(url, start, end, timeout=120):
    """Range request via ORIGINAL URL — Figshare issues a fresh signed S3 URL
    each time, so always pass the original ndownloader URL, never a cached one."""
    r = requests.get(url, headers={"Range": f"bytes={start}-{end}"},
                     allow_redirects=True, timeout=timeout)
    r.raise_for_status()
    return r.content

def get_size(url):
    """Get file size using a tiny range request (avoids caching an expiring URL)."""
    r = requests.get(url, headers={"Range": "bytes=0-0"},
                     allow_redirects=True, timeout=30)
    r.raise_for_status()
    cr = r.headers.get("Content-Range", "")
    if "/" in cr:
        return int(cr.split("/")[-1])
    return int(r.headers["Content-Length"])

def read_eocd(url, total):
    tail = range_get(url, max(0, total - 65536), total - 1)
    sig = b"\x50\x4b\x05\x06"
    i = tail.rfind(sig)
    if i < 0:
        raise RuntimeError("EOCD signature not found — ZIP may use ZIP64")
    eocd = tail[i:]
    cd_size   = struct.unpack_from("<I", eocd, 12)[0]
    cd_offset = struct.unpack_from("<I", eocd, 16)[0]
    return cd_offset, cd_size

def list_zip_files(url):
    """Return dict of filename → (compressed_size, uncompressed_size, local_offset, method)."""
    print("Getting file size …")
    total = get_size(url)
    print(f"  Size: {total/1e9:.2f} GB")

    print("Reading ZIP central directory …")
    cd_offset, cd_size = read_eocd(url, total)
    cd = range_get(url, cd_offset, cd_offset + cd_size - 1)
    print(f"  Central directory: {cd_size/1e3:.0f} KB")

    files = {}
    i = 0
    sig = b"\x50\x4b\x01\x02"
    while i + 46 <= len(cd):
        if cd[i:i+4] != sig:
            i += 1
            continue
        method  = struct.unpack_from("<H", cd, i+10)[0]
        csize   = struct.unpack_from("<I", cd, i+20)[0]
        usize   = struct.unpack_from("<I", cd, i+24)[0]
        fnlen   = struct.unpack_from("<H", cd, i+28)[0]
        exlen   = struct.unpack_from("<H", cd, i+30)[0]
        cmlen   = struct.unpack_from("<H", cd, i+32)[0]
        loffset = struct.unpack_from("<I", cd, i+42)[0]
        fname   = cd[i+46:i+46+fnlen].decode("utf-8", errors="replace")
        files[fname] = (csize, usize, loffset, method)
        i += 46 + fnlen + exlen + cmlen
    return files, total

def extract_entry(url, fname, csize, usize, loffset, method):
    """Download and decompress one ZIP entry (always via original URL)."""
    hdr = range_get(url, loffset, loffset + 29)
    fnlen = struct.unpack_from("<H", hdr, 26)[0]
    exlen = struct.unpack_from("<H", hdr, 28)[0]
    data_start = loffset + 30 + fnlen + exlen
    data_end   = data_start + csize - 1

    print(f"  Downloading {fname} "
          f"({csize/1e6:.1f} MB compressed, {usize/1e6:.1f} MB uncompressed)")
    raw = range_get(url, data_start, data_end, timeout=300)

    if method == 0:   # STORE
        return raw
    elif method == 8: # DEFLATE
        return zlib.decompress(raw, -15)
    else:
        raise RuntimeError(f"Unsupported ZIP compression method: {method}")

# ── Main ──────────────────────────────────────────────────────────────────────

print("=" * 60)
print("Step 1 — List files in ZIP")
print("=" * 60)
files, total = list_zip_files(URL)
print(f"\nAll files ({len(files)}):")
for f in sorted(files.keys()):
    csize, usize = files[f][0], files[f][1]
    print(f"  {f:60s}  {usize/1e6:6.1f} MB")

# ── Find the right file ───────────────────────────────────────────────────────
# Look for: annual, population-weighted, temperature, country (GADM0) level
# CRU TS is the preferred source; ERA5 as fallback.
import re

def score_fname(f):
    f = f.lower()
    score = 0
    if "monthly" in f:   score += 10  # monthly files (aggregate to annual)
    if "pop_2015" in f:  score += 15  # population weighted, 2015 base year
    if "tmp" in f:       score += 10  # temperature
    if "gadm0" in f:     score += 8   # country level
    if "cru" in f:       score += 3   # prefer CRU TS
    if "era" in f:       score += 2
    return score

ranked = sorted(files.keys(), key=score_fname, reverse=True)
print("\nTop candidates:")
for f in ranked[:10]:
    print(f"  [{score_fname(f):2d}] {f}")

# Target: gadm0_cru_tmp_pop_2015_monthly.csv
TARGET = "WCD_CSV/gadm0/temperature/gadm0_cru_tmp_pop_2015_monthly.csv"
best = TARGET if TARGET in files else ranked[0]
print(f"\nSelected: {best}")

# ── Download and parse ────────────────────────────────────────────────────────
print("\n" + "=" * 60)
print("Step 2 — Download selected file")
print("=" * 60)

csize, usize, loffset, method = files[best]
raw_bytes = extract_entry(URL, best, csize, usize, loffset, method)

df = pd.read_csv(io.BytesIO(raw_bytes))
print(f"\nShape: {df.shape}")
print(df.iloc[:3, :5])

# ── Parse wide format: rows = dates (X{YYYYMM}), columns = ISO3 ──────────────
# Date column is the first column, named 'Date' or similar
date_col = df.columns[0]
# Parse dates: "X190101" -> year=1901, month=01
df["year"]  = df[date_col].astype(str).str[1:5].astype(int)
df["month"] = df[date_col].astype(str).str[5:7].astype(int)

# ── Filter 2015 ───────────────────────────────────────────────────────────────
df15 = df[df["year"] == 2015].copy()
print(f"Rows for 2015: {len(df15)} (expect 12)")
if len(df15) == 0:
    sys.exit("No 2015 data found.")

# Drop non-country columns; country columns are standard ISO3 (3 uppercase letters)
# Some special codes like Z01-Z09 exist — skip them
import re
country_cols = [c for c in df.columns
                if re.match(r'^[A-Z]{3}$', c) and c not in ("NAN",)]
print(f"Country columns: {len(country_cols)}")

# Annual mean per country (mean of 12 monthly values)
annual_vals = df15[country_cols].mean(axis=0)
annual = pd.DataFrame({"iso3": annual_vals.index, "temp": annual_vals.values})
annual["iso3"] = annual["iso3"].str.upper().str.strip()
print(f"Countries with 2015 annual mean: {len(annual)}")

# ── Fetch 2015 country populations from World Bank ───────────────────────────
print("\nFetching country populations 2015 from World Bank API ...")
pop_url = ("https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL"
           "?format=json&per_page=500&date=2015")
try:
    rp = requests.get(pop_url, timeout=30)
    rp.raise_for_status()
    pop_rows = rp.json()[1]
    pop_df = pd.DataFrame([
        {"iso3": r["countryiso3code"].upper(), "pop": r["value"]}
        for r in pop_rows
        if r.get("value") and r.get("countryiso3code")
    ])
    print(f"  Got population for {len(pop_df)} countries")
except Exception as e:
    print(f"  Failed ({e}) -- using equal weights")
    pop_df = pd.DataFrame(columns=["iso3","pop"])

# ── Merge and map to R10 ─────────────────────────────────────────────────────
merged = annual.merge(pop_df, on="iso3", how="left")
merged["pop"] = merged["pop"].fillna(1.0)
merged["r10"] = merged["iso3"].map(R10_MAP)

unmapped = merged[merged["r10"].isna()]["iso3"].tolist()
if unmapped:
    print(f"Unmapped ISO3 ({len(unmapped)}): {unmapped[:30]}")
merged = merged.dropna(subset=["r10","temp"])

# ── Population-weighted R10 mean ──────────────────────────────────────────────
def wavg(g):
    return np.average(g["temp"], weights=g["pop"])

r10_temp = merged.groupby("r10").apply(wavg).round(2)

print("\n" + "=" * 60)
print("R10 population-weighted annual mean temperatures, 2015")
print("Source: Gortan et al. (2024), CRU TS, pop weights 2015")
print("=" * 60)
for reg in R10_REGIONS:
    val = r10_temp.get(reg, float("nan"))
    n   = int(merged[merged["r10"] == reg].shape[0])
    print(f"  {reg:35s}: {val:6.2f} C  ({n} countries)")

print("\n# -- R code for fig3.R " + "-" * 40)
print("# Source: Gortan et al. (2024), population-weighted annual mean temperature,")
print("# CRU TS v4, pop weights 2015, aggregated to R10 with World Bank 2015 population.")
print("r10_baseline_temp <- c(")
for i, reg in enumerate(R10_REGIONS):
    val   = r10_temp.get(reg, float("nan"))
    comma = "," if i < len(R10_REGIONS) - 1 else ""
    print(f'  "{reg}" = {val:.2f}{comma}')
print(")")
