# Stock Analysis Suite (Market_Analysis_Major consolidation)

Python suite for **portfolio dependence / risk** and **point-in-time fundamentals**.

This repository is the repaired home of [Market_Analysis_Major](https://github.com/zehaog2/Market_Analysis_Major). Desktop originals under `~/Desktop/Market_Analysis_Major` and `~/Desktop/Fundamental-Analysis-Major` stay intact; this working tree is a separate clone.

## What this does

| App | Role |
| --- | --- |
| `apps/risk_corr` | Conditional corr, β, vol/up-down-day effects, risk level, divergence vs SPY & IWM → CSV/JSON |
| `apps/fundamentals_screen` | Quality + valuation screen (canonical ROIC) → CSV/XLSX |

## Shared watchlist (one file)

Both apps read the **same portfolio config** the user already edits:

```text
/Users/henryguo/Desktop/Market_Analysis_Major/Stock_corr/config.json
```

Schema:

```json
{ "portfolio": { "stocks": ["SMCI", "ANET", "PLAB"] } }
```

In this clone, `shared/data/portfolio_config.json` is a **symlink** to that Desktop file. Override with `PORTFOLIO_CONFIG=/path/to/config.json` or `--config`.

## Layout

```text
shared/
  data/           # portfolio loader + Yahoo prices
  risk/           # Python port of R corr/β/risk/divergence
  fundamentals/   # compute_metrics from Fundamentals.ipynb
apps/
  risk_corr/
  fundamentals_screen/
Stock_corr/       # legacy R sources (reference; not required to run the suite)
outputs/          # CSV/JSON/XLSX from batch runs
```

## Setup

```bash
cd ~/Desktop/Stock_Analysis_Suite
python3 -m pip install -r requirements.txt
```

No R runtime is required for the new suite.

## Run

```bash
# Risk / correlation / divergence batch
python3 apps/risk_corr/run.py --years 3 --lookback-days 20

# Fundamentals screen (annual + TTM side-by-side)
python3 apps/fundamentals_screen/run.py
```

Outputs land in `outputs/`.

## Methodology notes (honest)

### Divergence (P0 fix)
- **Expected move = \(r_b \times \beta\)** (CAPM-style).
- Legacy R used \(r_b \times \rho \times \beta\) (double-counted association). That formula is **not** used here.
- Score = mean(last 5 day residual pp) + cumulative/10; labels ±0.5 / ±1.5 are heuristics, not backtested.

### “Bull / bear” in risk matrix
- Still **daily benchmark-return quantiles** (top/bottom ~40%), not calendar bull/bear markets. Columns are labeled up-day / down-day in CSV exports.

### Fundamentals vintage
- Quality metrics shown **annual and TTM side-by-side** with statement as-of dates.
- Canonical **ROIC** = NOPAT / (Debt + Equity − Cash + Operating Lease ROU); NOPAT = EBIT × (1 − effective tax).
- CapEx from Yahoo is typically negative → FCF = OCF + CapEx.
- Absolute thresholds (>60% gross, >11% ROIC, etc.) are aspirational screens, not calibrated models.
- PEG comes from Yahoo as-is.

### Not included
- **Fear & Greed** — dropped (README historically advertised scrapers that were never in-tree).
- PDF risk-matrix / divergence reports — optional later; CSV/JSON first.
- ML, portfolio optimizer, backtests — none.

## Legacy R

`Stock_corr/*.R` remains for reference. Prefer the Python apps above. If you still run R, point batch scripts at the same `config.json`.

## License

See `LICENSE`.
