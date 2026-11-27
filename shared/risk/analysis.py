"""Python port of advanced_correlation_functions.R (conditional corr + β)."""

from __future__ import annotations

from datetime import datetime
from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd
from scipy import stats

from shared.data.yahoo import align_returns, download_prices, log_returns


def _corr_block(x: np.ndarray, y: np.ndarray) -> Dict[str, Any]:
    if len(x) < 10:
        return None  # type: ignore[return-value]
    test = stats.pearsonr(x, y)
    # pearsonr returns (stat, p) in older scipy; PearsonRResult in newer
    r = float(test.statistic if hasattr(test, "statistic") else test[0])
    p = float(test.pvalue if hasattr(test, "pvalue") else test[1])
    var_y = float(np.var(y, ddof=1))
    beta = float(np.cov(x, y, ddof=1)[0, 1] / var_y) if var_y != 0 else float("nan")
    return {
        "correlation": r,
        "beta": beta,
        "p_value": p,
        "n_obs": int(len(x)),
        "significant": p < 0.05,
    }


def _rolling_std(series: pd.Series, window: int = 20) -> pd.Series:
    return series.rolling(window=window).std()


def advanced_correlation_analysis(
    stock_returns: pd.Series,
    benchmark_returns: pd.Series,
    stock_name: str,
    benchmark_name: str,
) -> Optional[Dict[str, Any]]:
    """Conditional correlation / beta vs a benchmark (SPY or IWM)."""
    s, b = align_returns(stock_returns, benchmark_returns)
    if len(s) < 30:
        return None

    results: Dict[str, Any] = {
        "stock": stock_name,
        "benchmark": benchmark_name,
        "n_obs": int(len(s)),
    }

    overall = _corr_block(s.values, b.values)
    if overall is None:
        return None
    results["overall"] = overall

    # Method E: volatility regimes (20d rolling SD of stock returns)
    if len(s) >= 20:
        stock_vol = _rolling_std(s, 20)
        valid = stock_vol.notna()
        sv = stock_vol[valid]
        s_v = s[valid]
        b_v = b[valid]
        if len(sv) >= 30:
            vol_75 = float(sv.quantile(0.75))
            vol_25 = float(sv.quantile(0.25))
            high_mask = sv > vol_75
            low_mask = sv < vol_25
            if high_mask.sum() >= 10:
                results["high_vol"] = _corr_block(
                    s_v[high_mask].values, b_v[high_mask].values
                )
            if low_mask.sum() >= 10:
                results["low_vol"] = _corr_block(
                    s_v[low_mask].values, b_v[low_mask].values
                )
            if results.get("high_vol") and results.get("low_vol"):
                results["vol_effect"] = (
                    results["high_vol"]["correlation"] - results["low_vol"]["correlation"]
                )

    # Method C: up/down-day quantiles of *benchmark* returns
    # (historical label "bull/bear" — daily return quantiles, not calendar regimes)
    q_hi = float(b.quantile(0.60))  # top 40% ≈ bull days
    q_lo = float(b.quantile(0.40))  # bottom 40% ≈ bear days
    bull_mask = b >= q_hi
    bear_mask = b <= q_lo
    neutral_mask = ~(bull_mask | bear_mask)

    if bull_mask.sum() >= 10:
        results["bull"] = _corr_block(s[bull_mask].values, b[bull_mask].values)
    if bear_mask.sum() >= 10:
        results["bear"] = _corr_block(s[bear_mask].values, b[bear_mask].values)
    if neutral_mask.sum() >= 10:
        results["neutral"] = _corr_block(
            s[neutral_mask].values, b[neutral_mask].values
        )
    if results.get("bull") and results.get("bear"):
        results["regime_effect"] = (
            results["bear"]["correlation"] - results["bull"]["correlation"]
        )

    # Method F: tail dependence (±2σ on stock OR benchmark extremes)
    s_mu, s_sd = float(s.mean()), float(s.std(ddof=1))
    b_mu, b_sd = float(b.mean()), float(b.std(ddof=1))
    if s_sd > 0 and b_sd > 0:
        upper = (s > s_mu + 2 * s_sd) | (b > b_mu + 2 * b_sd)
        lower = (s < s_mu - 2 * s_sd) | (b < b_mu - 2 * b_sd)
        # Also percentile OR-masks (5th/95th)
        s_lo, s_hi = float(s.quantile(0.05)), float(s.quantile(0.95))
        b_lo, b_hi = float(b.quantile(0.05)), float(b.quantile(0.95))
        upper = upper | (s >= s_hi) | (b >= b_hi)
        lower = lower | (s <= s_lo) | (b <= b_lo)
        if upper.sum() >= 5:
            results["upper_tail"] = _corr_block(s[upper].values, b[upper].values)
        if lower.sum() >= 5:
            results["lower_tail"] = _corr_block(s[lower].values, b[lower].values)

    # Risk level heuristic (same thresholds as R)
    risk_count = 0
    if results.get("vol_effect") is not None and results["vol_effect"] > 0.15:
        risk_count += 1
    if results.get("regime_effect") is not None and results["regime_effect"] > 0.15:
        risk_count += 1
    if results.get("lower_tail") and results["lower_tail"]["correlation"] > 0.6:
        risk_count += 1
    results["risk_level"] = (
        "HIGH" if risk_count >= 2 else ("MEDIUM" if risk_count == 1 else "LOW")
    )
    results["risk_count"] = risk_count
    return results


def batch_risk_analysis(
    tickers: List[str],
    years: float = 3.0,
    benchmarks: Optional[List[str]] = None,
) -> Dict[str, Dict[str, Any]]:
    """Fetch prices and run advanced analysis for each ticker × benchmark."""
    benchmarks = benchmarks or ["SPY", "IWM"]
    end = datetime.utcnow()
    all_results: Dict[str, Dict[str, Any]] = {}

    bench_rets: Dict[str, pd.Series] = {}
    for bm in benchmarks:
        prices = download_prices(bm, years=years, end=end)
        bench_rets[bm] = log_returns(prices)

    for ticker in tickers:
        try:
            prices = download_prices(ticker, years=years, end=end)
            s_ret = log_returns(prices)
        except Exception as exc:  # noqa: BLE001 — surface per-ticker failure
            all_results[ticker] = {"error": str(exc)}
            continue

        ticker_results: Dict[str, Any] = {}
        for bm, b_ret in bench_rets.items():
            try:
                ticker_results[bm] = advanced_correlation_analysis(
                    s_ret, b_ret, ticker, bm
                )
            except Exception as exc:  # noqa: BLE001
                ticker_results[bm] = {"error": str(exc)}
        all_results[ticker] = ticker_results

    return all_results
