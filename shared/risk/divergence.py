"""Divergence detector — P0 fix: expected move = r_b * β (not ρ × β)."""

from __future__ import annotations

from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd

from shared.data.yahoo import align_returns, download_prices, log_returns
from shared.risk.analysis import _corr_block


def _profile_beta(
    ticker: str, benchmark: str, profile_years: float = 1.0
) -> Dict[str, float]:
    prices_s = download_prices(ticker, years=profile_years)
    prices_b = download_prices(benchmark, years=profile_years)
    s, b = align_returns(log_returns(prices_s), log_returns(prices_b))
    block = _corr_block(s.values, b.values)
    if block is None:
        raise RuntimeError(f"Insufficient overlap for {ticker} vs {benchmark}")
    return {"correlation": block["correlation"], "beta": block["beta"]}


def _classify(score: float) -> tuple[str, str]:
    if score > 1.5:
        return "Strong Positive", "BULLISH"
    if score > 0.5:
        return "Weak Positive", "Slightly Bullish"
    if score < -1.5:
        return "Strong Negative", "BEARISH"
    if score < -0.5:
        return "Weak Negative", "Slightly Bearish"
    return "Neutral", "Neutral"


def detect_divergence(
    ticker: str,
    benchmark: str = "SPY",
    lookback_days: int = 20,
    profile_years: float = 1.0,
) -> Optional[Dict[str, Any]]:
    """
    Compare actual stock log-returns to β-scaled benchmark moves.

    P0 correction vs legacy R:
      expected = r_benchmark * beta
    (legacy incorrectly used r_benchmark * correlation * beta)
    """
    profile = _profile_beta(ticker, benchmark, profile_years)

    end = datetime.utcnow()
    start = end - timedelta(days=lookback_days + 40)
    s_px = download_prices(ticker, start=start, end=end)
    b_px = download_prices(benchmark, start=start, end=end)
    s_ret, b_ret = align_returns(log_returns(s_px), log_returns(b_px))

    if len(s_ret) < max(5, lookback_days // 2):
        return None

    recent_s = s_ret.tail(lookback_days)
    recent_b = b_ret.loc[recent_s.index]

    # P0: expected move = rb * β  (NOT ρ × β)
    expected = recent_b.values * profile["beta"]
    actual = recent_s.values
    divergences = (actual - expected) * 100.0  # percentage points of log-return

    valid = np.isfinite(divergences)
    div_clean = divergences[valid]
    if len(div_clean) < 5:
        return None

    cumulative = np.cumsum(div_clean)
    recent_5 = float(np.mean(div_clean[-5:]))
    current_cumulative = float(cumulative[-1])
    score = recent_5 + (current_cumulative / 10.0)
    if not np.isfinite(score):
        return None

    dtype, signal = _classify(score)
    return {
        "ticker": ticker,
        "benchmark": benchmark,
        "correlation": profile["correlation"],
        "beta": profile["beta"],
        "expected_formula": "r_b * beta",
        "divergence_score": score,
        "divergence_type": dtype,
        "signal": signal,
        "avg_divergence_pp": float(np.mean(div_clean)),
        "recent_5day_div_pp": recent_5,
        "cumulative_div_pp": current_cumulative,
        "lookback_days": lookback_days,
        "n_obs": int(len(div_clean)),
    }


def batch_divergence(
    tickers: List[str],
    benchmarks: Optional[List[str]] = None,
    lookback_days: int = 20,
    profile_years: float = 1.0,
) -> List[Dict[str, Any]]:
    benchmarks = benchmarks or ["SPY", "IWM"]
    rows: List[Dict[str, Any]] = []
    for ticker in tickers:
        for bm in benchmarks:
            try:
                row = detect_divergence(
                    ticker, bm, lookback_days=lookback_days, profile_years=profile_years
                )
                if row:
                    rows.append(row)
                else:
                    rows.append(
                        {
                            "ticker": ticker,
                            "benchmark": bm,
                            "error": "insufficient data",
                        }
                    )
            except Exception as exc:  # noqa: BLE001
                rows.append({"ticker": ticker, "benchmark": bm, "error": str(exc)})
    return rows
