#!/usr/bin/env python3
"""Screen portfolio tickers with canonical fundamentals → CSV + XLSX.

Tickers come from the shared portfolio config (not a hardcoded list).
"""

from __future__ import annotations

import argparse
import sys
from datetime import datetime, timezone
from pathlib import Path

# Allow running as `python apps/fundamentals_screen/run.py` from suite root
SUITE_ROOT = Path(__file__).resolve().parents[2]
if str(SUITE_ROOT) not in sys.path:
    sys.path.insert(0, str(SUITE_ROOT))

from shared.data.portfolio import config_path_used, load_portfolio_tickers
from shared.fundamentals import screen_tickers


def main() -> int:
    parser = argparse.ArgumentParser(description="Fundamentals screen for portfolio tickers")
    parser.add_argument(
        "--config",
        default=None,
        help="Path to portfolio JSON (default: shared/data/portfolio_config.json)",
    )
    parser.add_argument(
        "--outdir",
        default=str(SUITE_ROOT / "outputs"),
        help="Output directory",
    )
    parser.add_argument("--no-xlsx", action="store_true", help="Skip Excel export")
    args = parser.parse_args()

    tickers = load_portfolio_tickers(args.config)
    cfg = config_path_used(args.config)
    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")

    print(f"Config: {cfg}")
    print(f"Tickers: {', '.join(tickers)}")
    print("Screening…")

    clean = screen_tickers(tickers)
    csv_path = outdir / f"fundamentals_{stamp}.csv"
    clean.to_csv(csv_path, index=False)
    print(f"Wrote {csv_path}")
    print(clean.to_string(index=False))

    if not args.no_xlsx:
        try:
            xlsx_path = outdir / f"fundamentals_{stamp}.xlsx"
            clean.to_excel(xlsx_path, index=False, engine="openpyxl")
            print(f"Wrote {xlsx_path}")
        except Exception as exc:  # noqa: BLE001
            print(f"XLSX skipped: {exc}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
