"""Thin Yahoo Finance price client used by risk (and optionally fundamentals)."""

from __future__ import annotations

from datetime import datetime, timedelta
from typing import Optional

import numpy as np
import pandas as pd
import yfinance as yf


def download_prices(
    ticker: str,
    start: Optional[datetime] = None,
    end: Optional[datetime] = None,
    years: Optional[float] = None,
) -> pd.Series:
    """Adjusted close series indexed by date. Raises on empty download."""
    end = end or datetime.utcnow()
    if start is None:
        years = years if years is not None else 3.0
        start = end - timedelta(days=int(years * 365))

    data = yf.download(
        ticker,
        start=start.strftime("%Y-%m-%d"),
        end=end.strftime("%Y-%m-%d"),
        progress=False,
        auto_adjust=True,
        threads=False,
    )
    if data is None or data.empty:
        raise RuntimeError(f"No Yahoo price data for {ticker}")

    # yfinance may return MultiIndex columns for single tickers in newer versions
    if isinstance(data.columns, pd.MultiIndex):
        close = data["Close"]
        if isinstance(close, pd.DataFrame):
            close = close.iloc[:, 0]
    elif "Close" in data.columns:
        close = data["Close"]
    elif "Adj Close" in data.columns:
        close = data["Adj Close"]
    else:
        raise RuntimeError(f"Unexpected columns for {ticker}: {list(data.columns)}")

    close = close.dropna().astype(float)
    close.name = ticker.upper()
    if len(close) < 30:
        raise RuntimeError(f"Insufficient price history for {ticker} ({len(close)} rows)")
    return close


def log_returns(prices: pd.Series) -> pd.Series:
    """Daily log returns (matches R diff(log(prices)))."""
    rets = np.log(prices.astype(float)).diff().dropna()
    rets.name = prices.name
    return rets


def align_returns(a: pd.Series, b: pd.Series) -> tuple[pd.Series, pd.Series]:
    joined = pd.concat([a, b], axis=1, join="inner").dropna()
    if joined.empty:
        raise RuntimeError("No overlapping return dates")
    return joined.iloc[:, 0], joined.iloc[:, 1]
