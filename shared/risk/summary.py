"""Flatten batch risk results into table rows for CSV/JSON export."""

from __future__ import annotations

from typing import Any, Dict, List


def risk_summary_rows(all_results: Dict[str, Dict[str, Any]]) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    for ticker, by_bm in all_results.items():
        if "error" in by_bm and len(by_bm) == 1:
            rows.append({"ticker": ticker, "error": by_bm["error"]})
            continue
        for bm, result in by_bm.items():
            if result is None:
                rows.append({"ticker": ticker, "benchmark": bm, "error": "null result"})
                continue
            if isinstance(result, dict) and "error" in result and "overall" not in result:
                rows.append({"ticker": ticker, "benchmark": bm, "error": result["error"]})
                continue
            overall = result.get("overall") or {}
            rows.append(
                {
                    "ticker": ticker,
                    "benchmark": bm,
                    "correlation": overall.get("correlation"),
                    "beta": overall.get("beta"),
                    "p_value": overall.get("p_value"),
                    "n_obs": overall.get("n_obs"),
                    "vol_effect": result.get("vol_effect"),
                    "regime_effect": result.get("regime_effect"),
                    "up_day_corr": (result.get("bull") or {}).get("correlation"),
                    "down_day_corr": (result.get("bear") or {}).get("correlation"),
                    "high_vol_corr": (result.get("high_vol") or {}).get("correlation"),
                    "low_vol_corr": (result.get("low_vol") or {}).get("correlation"),
                    "lower_tail_corr": (result.get("lower_tail") or {}).get("correlation"),
                    "upper_tail_corr": (result.get("upper_tail") or {}).get("correlation"),
                    "risk_level": result.get("risk_level"),
                    "risk_count": result.get("risk_count"),
                    "note": (
                        "regime_effect = down_day_corr - up_day_corr "
                        "(daily benchmark quantiles, not calendar regimes)"
                    ),
                }
            )
    return rows
