#!/usr/bin/env python3
"""Batch portfolio risk / correlation / divergence → CSV + JSON.

Reads the shared portfolio config (Stock_corr/config.json via symlink).
"""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

# Allow running as `python apps/risk_corr/run.py` from suite root
SUITE_ROOT = Path(__file__).resolve().parents[2]
if str(SUITE_ROOT) not in sys.path:
    sys.path.insert(0, str(SUITE_ROOT))

from shared.data.portfolio import config_path_used, load_portfolio_tickers
from shared.risk import batch_divergence, batch_risk_analysis, risk_summary_rows


def main() -> int:
    parser = argparse.ArgumentParser(description="Portfolio risk / corr / divergence batch")
    parser.add_argument(
        "--config",
        default=None,
        help="Path to portfolio JSON (default: shared/data/portfolio_config.json → Desktop Stock_corr/config.json)",
    )
    parser.add_argument("--years", type=float, default=3.0, help="History years for corr/β")
    parser.add_argument("--lookback-days", type=int, default=20, help="Divergence lookback")
    parser.add_argument("--profile-years", type=float, default=1.0, help="β profile years")
    parser.add_argument(
        "--outdir",
        default=str(SUITE_ROOT / "outputs"),
        help="Output directory for CSV/JSON",
    )
    args = parser.parse_args()

    tickers = load_portfolio_tickers(args.config)
    cfg = config_path_used(args.config)
    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")

    print(f"Config: {cfg}")
    print(f"Tickers: {', '.join(tickers)}")
    print(f"Running risk analysis ({args.years}y)…")

    risk = batch_risk_analysis(tickers, years=args.years)
    risk_rows = risk_summary_rows(risk)
    risk_df = pd.DataFrame(risk_rows)
    risk_csv = outdir / f"risk_summary_{stamp}.csv"
    risk_json = outdir / f"risk_summary_{stamp}.json"
    risk_df.to_csv(risk_csv, index=False)
    with open(risk_json, "w", encoding="utf-8") as f:
        json.dump(
            {
                "config": cfg,
                "tickers": tickers,
                "years": args.years,
                "generated_utc": stamp,
                "rows": risk_rows,
            },
            f,
            indent=2,
            default=str,
        )
    print(f"Wrote {risk_csv}")
    print(f"Wrote {risk_json}")
    print(risk_df.to_string(index=False))

    print(f"\nRunning divergence (lookback={args.lookback_days}d, formula=r_b*β)…")
    div_rows = batch_divergence(
        tickers,
        lookback_days=args.lookback_days,
        profile_years=args.profile_years,
    )
    div_df = pd.DataFrame(div_rows)
    div_csv = outdir / f"divergence_{stamp}.csv"
    div_json = outdir / f"divergence_{stamp}.json"
    div_df.to_csv(div_csv, index=False)
    with open(div_json, "w", encoding="utf-8") as f:
        json.dump(
            {
                "config": cfg,
                "tickers": tickers,
                "expected_formula": "r_b * beta",
                "generated_utc": stamp,
                "rows": div_rows,
            },
            f,
            indent=2,
            default=str,
        )
    print(f"Wrote {div_csv}")
    print(f"Wrote {div_json}")
    print(div_df.to_string(index=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
