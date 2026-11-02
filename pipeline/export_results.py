"""Collect the R model outputs into docs/ for the dashboard and keep a history of forecast vintages.

Run from the repository root after the R scripts:
    python pipeline/export_results.py
"""
from __future__ import annotations

import json
import shutil
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "output"
DATA = ROOT / "data"
DOCS = ROOT / "docs"
FIGS = ["sfd_forecast.png", "sfd_growth_forecast.png", "sfd_by_sector.png", "sfd_sectors_panel.png",
        "sfd_holdout.png", "household_forecast.png", "private_forecast.png", "public_forecast.png"]


def clean(obj):
    if isinstance(obj, dict):
        return {str(k): clean(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [clean(v) for v in obj]
    if isinstance(obj, (float, np.floating)):
        return None if np.isnan(obj) else round(float(obj), 4)
    if isinstance(obj, (np.integer,)):
        return int(obj)
    if isinstance(obj, (pd.Timestamp, datetime)):
        return obj.strftime("%Y-%m-%d")
    return obj


def read_public_sfd() -> pd.Series:
    """Official ABS State Final Demand (chain volume, $m) from the input file in ABS layout."""
    raw = pd.read_excel(DATA / "goverment.xlsx", header=None, dtype=str)
    titles = raw.iloc[0].tolist()
    col = next(i for i, t in enumerate(titles) if isinstance(t, str) and t.startswith("STATE FINAL DEMAND"))
    body = raw.iloc[1:]
    serial = pd.to_numeric(body[0], errors="coerce")   # Excel serial numbers (built files) ...
    dates = pd.to_datetime(serial, unit="D", origin="1899-12-30").fillna(
        pd.to_datetime(body[0], errors="coerce"))       # ... or real dates (hand-made files)
    return pd.Series(pd.to_numeric(body[col]).values, index=dates).dropna()


def main() -> None:
    wb = pd.read_excel(OUT / "forecast_results_sfd.xlsx", sheet_name=None)
    growth = wb["SFD growth forecast"]
    by_sector = wb["SFD forecast by sector"]
    holdout = wb["Hold-out accuracy"]
    sfd_hold = wb["SFD hold-out"]
    manifest = json.loads((DATA / "inputs_manifest.json").read_text())

    sfd = read_public_sfd()
    g = 100 * np.log(sfd).diff().dropna()
    actual = [{"quarter": d.strftime("%Y-%m"), "level": float(sfd[d]), "growth": float(g[d])} for d in g.index[-24:]]

    sfd_acc = holdout[holdout["sector"] == "SFD (sum)"].copy()
    sector_acc = holdout[holdout["sector"] != "SFD (sum)"].copy()
    keep = ["sector", "model", "RMSE_dlog", "RMSE_logerr", "MAPE", "year_pct_error"]

    out = {
        "generated_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "data": manifest,
        "headline_model": "Combination A+B",
        "forecast_growth": growth.to_dict("records"),
        "forecast_by_sector": by_sector.to_dict("records"),
        "actual_recent": actual,
        "holdout_sfd": sfd_acc[[c for c in keep + ["ratio_vs_plain_average"] if c in sfd_acc]].to_dict("records"),
        "holdout_sectors": sector_acc[[c for c in keep + ["ratio_vs_avg_growth"] if c in sector_acc]].to_dict("records"),
        "holdout_path": sfd_hold[sfd_hold["model"].isin(["Combination A+B", "MinT (A)", "VECM-MinT (B)",
                                                         "Benchmark: average growth"])]
            [["model", "quarter", "forecast_dlog", "actual_dlog"]].to_dict("records"),
    }
    (DOCS / "data").mkdir(parents=True, exist_ok=True)
    (DOCS / "data" / "latest.json").write_text(json.dumps(clean(out), indent=1))

    # Forecast vintages: one row per (data vintage, forecast quarter) so forecasts can be scored later
    hist_path = DATA / "forecast_history.csv"
    rows = growth.assign(data_last_quarter=manifest["last_quarter"], run_utc=out["generated_utc"])
    if hist_path.exists():
        hist = pd.read_csv(hist_path, dtype={"quarter": str, "data_last_quarter": str})
        hist = hist[hist["data_last_quarter"] != manifest["last_quarter"]]   # re-runs replace the same vintage
        rows = pd.concat([hist, rows], ignore_index=True)
    rows.to_csv(hist_path, index=False)

    (DOCS / "figures").mkdir(parents=True, exist_ok=True)
    for f in FIGS:
        if (OUT / f).exists():
            shutil.copy(OUT / f, DOCS / "figures" / f)
    print(f"Exported results for data up to {manifest['last_quarter']}")


if __name__ == "__main__":
    main()
