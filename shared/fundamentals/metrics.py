"""Canonical point-in-time fundamentals (from Fundamentals.ipynb).

ROIC = NOPAT / Invested Capital
  NOPAT = EBIT × (1 − effective tax)
  IC = Debt + Equity − Cash + Operating Lease ROU

Quality metrics are reported BOTH annual and TTM side-by-side (P0 vintage clarity).
"""

from __future__ import annotations

import math
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd
import yfinance as yf


def safe_first(series: pd.Series, prefer_positive: bool = False) -> float:
    if series is None or len(series) == 0:
        return np.nan
    vals = series.dropna().astype(float).values
    if vals.size == 0:
        return np.nan
    if prefer_positive:
        for v in vals:
            if v > 0:
                return float(v)
    return float(vals[0])


def normalize(df: Optional[pd.DataFrame]) -> pd.DataFrame:
    """Ensure the latest reporting period is the first column."""
    if df is None or df.empty:
        return pd.DataFrame()
    df = df.copy()
    if not isinstance(df.index, pd.Index):
        df.index = pd.Index(df.index)
    try:
        cols = pd.to_datetime(df.columns)
        df = df.loc[:, cols.sort_values(ascending=False).index]
    except Exception:
        pass
    return df


def ttm_sum(qdf: Optional[pd.DataFrame], labels) -> float:
    if qdf is None or qdf.empty:
        return np.nan
    if isinstance(labels, str):
        labels = [labels]
    for label in labels:
        if label in qdf.index:
            vals = qdf.loc[label].dropna().astype(float)
            if vals.size == 0:
                continue
            return float(vals.iloc[:4].sum())
    return np.nan


def effective_tax_rate(income_stmt: pd.DataFrame) -> float:
    try:
        if "Tax Provision" in income_stmt.index:
            tax = safe_first(income_stmt.loc["Tax Provision"], prefer_positive=False)
        elif "Income Tax Expense" in income_stmt.index:
            tax = safe_first(income_stmt.loc["Income Tax Expense"], prefer_positive=False)
        else:
            return 0.21
        pretax = safe_first(income_stmt.loc["Pretax Income"], prefer_positive=True)
        if pd.isna(tax) or pd.isna(pretax) or pretax == 0:
            return 0.21
        return max(0.0, min(0.40, float(tax) / float(pretax)))
    except Exception:
        return 0.21


def _latest_period_label(df: pd.DataFrame) -> Optional[str]:
    if df is None or df.empty:
        return None
    col = df.columns[0]
    try:
        return pd.Timestamp(col).strftime("%Y-%m-%d")
    except Exception:
        return str(col)


def compute_metrics(tick: str) -> Dict[str, Any]:
    t = yf.Ticker(tick)

    price = np.nan
    try:
        if hasattr(t, "fast_info") and t.fast_info is not None:
            price = float(t.fast_info.get("last_price", np.nan))
    except Exception:
        pass

    info: Dict[str, Any] = {}
    try:
        info = t.get_info() if hasattr(t, "get_info") else (t.info if hasattr(t, "info") else {})
    except Exception:
        info = {}

    if math.isnan(price):
        price = float(
            info.get("currentPrice", np.nan)
            or info.get("regularMarketPrice", np.nan)
            or np.nan
        )

    trailing_eps = info.get("trailingEps", np.nan)
    trailing_pe = info.get("trailingPE", np.nan)
    if (
        (pd.isna(trailing_pe) or trailing_pe in (None,))
        and not pd.isna(price)
        and not pd.isna(trailing_eps)
        and trailing_eps not in (0, None)
    ):
        trailing_pe = price / trailing_eps

    income_stmt = t.income_stmt
    if income_stmt is None or income_stmt.empty:
        income_stmt = t.financials
    balance_sheet = t.balance_sheet
    cashflow = t.cashflow

    income_stmt = normalize(income_stmt)
    balance_sheet = normalize(balance_sheet)
    cashflow = normalize(cashflow)

    q_income = normalize(getattr(t, "quarterly_income_stmt", None))
    if q_income is None or q_income.empty:
        q_income = normalize(getattr(t, "quarterly_financials", None))
    q_cashflow = normalize(getattr(t, "quarterly_cashflow", None))

    annual_as_of = _latest_period_label(income_stmt)
    balance_as_of = _latest_period_label(balance_sheet)

    revenue = np.nan
    gross_profit = np.nan
    try:
        for label in ["Total Revenue", "Revenues", "Revenue"]:
            if label in income_stmt.index:
                revenue = safe_first(income_stmt.loc[label])
                break
        for label in ["Gross Profit", "GrossProfit"]:
            if label in income_stmt.index:
                gross_profit = safe_first(income_stmt.loc[label])
                break
    except Exception:
        pass
    gross_margin = (
        (gross_profit / revenue)
        if revenue and not pd.isna(revenue) and revenue != 0
        else np.nan
    )

    ebit = np.nan
    try:
        if "Operating Income" in income_stmt.index:
            ebit = safe_first(income_stmt.loc["Operating Income"])
        elif "EBIT" in income_stmt.index:
            ebit = safe_first(income_stmt.loc["EBIT"])
    except Exception:
        pass

    tax_rate = effective_tax_rate(income_stmt) if not income_stmt.empty else 0.21
    nopat = (ebit * (1.0 - tax_rate)) if not pd.isna(ebit) else np.nan

    total_debt = np.nan
    try:
        short_debt = 0.0
        long_debt = 0.0
        if "Short Long Term Debt" in balance_sheet.index:
            short_debt = (
                safe_first(balance_sheet.loc["Short Long Term Debt"], prefer_positive=True)
                or 0.0
            )
        if "Short Term Debt" in balance_sheet.index:
            short_debt = (
                safe_first(balance_sheet.loc["Short Term Debt"], prefer_positive=True)
                or short_debt
            )
        if "Long Term Debt" in balance_sheet.index:
            long_debt = (
                safe_first(balance_sheet.loc["Long Term Debt"], prefer_positive=True) or 0.0
            )
        total_debt = (short_debt or 0.0) + (long_debt or 0.0)
    except Exception:
        pass

    total_equity = np.nan
    try:
        if "Total Stockholder Equity" in balance_sheet.index:
            total_equity = safe_first(balance_sheet.loc["Total Stockholder Equity"])
        elif "Total Equity Gross Minority Interest" in balance_sheet.index:
            total_equity = safe_first(
                balance_sheet.loc["Total Equity Gross Minority Interest"]
            )
    except Exception:
        pass

    cash = np.nan
    try:
        for label in [
            "Cash And Cash Equivalents",
            "Cash And Cash Equivalents And Short Term Investments",
            "Cash",
        ]:
            if label in balance_sheet.index:
                cash = safe_first(balance_sheet.loc[label])
                break
    except Exception:
        pass

    rou_assets = np.nan
    try:
        for label in [
            "Operating Lease Right Of Use Assets",
            "Right Of Use Assets",
            "Operating Lease Assets",
        ]:
            if label in balance_sheet.index:
                rou_assets = safe_first(balance_sheet.loc[label], prefer_positive=True)
                break
    except Exception:
        pass

    invested_capital = np.nan
    try:
        if not pd.isna(total_debt) or not pd.isna(total_equity) or not pd.isna(cash):
            invested_capital = (
                (0.0 if pd.isna(total_debt) else total_debt)
                + (0.0 if pd.isna(total_equity) else total_equity)
                - (0.0 if pd.isna(cash) else cash)
                + (0.0 if pd.isna(rou_assets) else rou_assets)
            )
            if invested_capital == 0:
                invested_capital = np.nan
    except Exception:
        pass

    roic = (
        (nopat / invested_capital)
        if (not pd.isna(nopat) and not pd.isna(invested_capital) and invested_capital != 0)
        else np.nan
    )

    operating_cf = np.nan
    capex = np.nan
    try:
        for label in ["Operating Cash Flow", "Total Cash From Operating Activities"]:
            if label in cashflow.index:
                operating_cf = safe_first(cashflow.loc[label])
                break
        for label in [
            "Capital Expenditure",
            "Investments In Property Plant And Equipment",
        ]:
            if label in cashflow.index:
                capex = safe_first(cashflow.loc[label])
                break
    except Exception:
        pass

    # yfinance CapEx is typically negative → FCF = OCF + CapEx
    free_cash_flow = np.nan
    if not pd.isna(operating_cf) and not pd.isna(capex):
        free_cash_flow = float(operating_cf) + float(capex)

    fcf_margin = (
        (free_cash_flow / revenue)
        if (not pd.isna(free_cash_flow) and not pd.isna(revenue) and revenue != 0)
        else np.nan
    )

    interest_expense = np.nan
    try:
        for label in ["Interest Expense", "Interest Expense Non Operating"]:
            if label in income_stmt.index:
                interest_expense = safe_first(income_stmt.loc[label])
                break
    except Exception:
        pass

    interest_coverage = np.nan
    if not pd.isna(ebit) and not pd.isna(interest_expense):
        denom = abs(float(interest_expense)) if float(interest_expense) != 0 else np.nan
        interest_coverage = (
            (float(ebit) / denom) if denom and not pd.isna(denom) else np.nan
        )

    # TTM quality twins (side-by-side with annual)
    ttm_revenue = ttm_sum(q_income, ["Total Revenue", "Revenues", "Revenue"])
    ttm_gross_profit = ttm_sum(q_income, ["Gross Profit", "GrossProfit"])
    ttm_ebit = ttm_sum(q_income, ["Operating Income", "EBIT"])
    ttm_gross_margin = (
        (ttm_gross_profit / ttm_revenue)
        if (not pd.isna(ttm_gross_profit) and not pd.isna(ttm_revenue) and ttm_revenue != 0)
        else np.nan
    )
    ttm_ocf = ttm_sum(
        q_cashflow, ["Operating Cash Flow", "Total Cash From Operating Activities"]
    )
    ttm_capex = ttm_sum(
        q_cashflow,
        ["Capital Expenditure", "Investments In Property Plant And Equipment"],
    )
    ttm_fcf = np.nan
    if not pd.isna(ttm_ocf) and not pd.isna(ttm_capex):
        ttm_fcf = float(ttm_ocf) + float(ttm_capex)
    ttm_fcf_margin = (
        (ttm_fcf / ttm_revenue)
        if (not pd.isna(ttm_fcf) and not pd.isna(ttm_revenue) and ttm_revenue != 0)
        else np.nan
    )

    # TTM ROIC uses TTM NOPAT over latest invested capital (point-in-time BS)
    ttm_tax = tax_rate
    ttm_nopat = (ttm_ebit * (1.0 - ttm_tax)) if not pd.isna(ttm_ebit) else np.nan
    ttm_roic = (
        (ttm_nopat / invested_capital)
        if (
            not pd.isna(ttm_nopat)
            and not pd.isna(invested_capital)
            and invested_capital != 0
        )
        else np.nan
    )

    market_cap = float(info.get("marketCap", np.nan) or np.nan)

    ev_ebitda = np.nan
    enterprise_value = info.get("enterpriseValue", np.nan)
    ebitda = info.get("ebitda", np.nan)
    try:
        if (
            enterprise_value not in (None,)
            and ebitda not in (None, 0)
            and not pd.isna(enterprise_value)
            and not pd.isna(ebitda)
            and float(ebitda) != 0
        ):
            ev_ebitda = float(enterprise_value) / float(ebitda)
        else:
            dep_amort = ttm_sum(
                q_cashflow,
                [
                    "Depreciation And Amortization",
                    "Depreciation Amortization Depletion",
                    "Depreciation",
                ],
            )
            if pd.isna(dep_amort) and "Depreciation And Amortization" in cashflow.index:
                dep_amort = safe_first(cashflow.loc["Depreciation And Amortization"])
            ebitda_calc = (
                (ebit + dep_amort)
                if (not pd.isna(ebit) and not pd.isna(dep_amort))
                else np.nan
            )
            ev_calc = np.nan
            if not pd.isna(market_cap):
                ev_calc = (
                    market_cap
                    + (0.0 if pd.isna(total_debt) else total_debt)
                    - (0.0 if pd.isna(cash) else cash)
                )
            if not pd.isna(ev_calc) and not pd.isna(ebitda_calc) and ebitda_calc != 0:
                ev_ebitda = ev_calc / ebitda_calc
    except Exception:
        pass

    p_fcf = np.nan
    fcf_for_ratio = ttm_fcf if not pd.isna(ttm_fcf) else free_cash_flow
    if not pd.isna(market_cap) and not pd.isna(fcf_for_ratio) and fcf_for_ratio != 0:
        p_fcf = market_cap / fcf_for_ratio

    peg = info.get("pegRatio", np.nan)
    if peg in (None,) or (isinstance(peg, float) and pd.isna(peg)):
        peg = info.get("trailingPegRatio", np.nan)
    try:
        peg = float(peg) if peg not in (None,) else np.nan
    except Exception:
        peg = np.nan

    return {
        "Ticker": tick,
        "as_of_run_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "annual_income_as_of": annual_as_of,
        "balance_sheet_as_of": balance_as_of,
        "Price": price,
        "Revenue_annual": revenue,
        "Gross_Margin_annual": gross_margin,
        "Gross_Margin_TTM": ttm_gross_margin,
        "ROIC_annual": roic,
        "ROIC_TTM": ttm_roic,
        "FCF_Margin_annual": fcf_margin,
        "FCF_Margin_TTM": ttm_fcf_margin,
        "Interest_Coverage_annual": interest_coverage,
        "Effective_Tax_Rate": tax_rate,
        "Invested_Capital": invested_capital,
        "P/E_trailing": trailing_pe,
        "EV/EBITDA": ev_ebitda,
        "P/FCF_TTM_preferred": p_fcf,
        "PEG_yahoo": peg,
        "vendor": "yfinance/Yahoo",
        "roic_definition": "NOPAT/(Debt+Equity-Cash+ROU)",
    }


def screen_tickers(tickers: List[str]) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for tick in tickers:
        try:
            rows.append(compute_metrics(tick))
        except Exception as exc:  # noqa: BLE001
            rows.append({"Ticker": tick, "Error": str(exc)})
    df = pd.DataFrame(rows)

    # Display screen with annual + TTM side-by-side (percentages)
    out = pd.DataFrame()
    out["Ticker"] = df.get("Ticker")
    if "Error" in df.columns:
        out["Error"] = df["Error"]
    for src, dst, scale in [
        ("annual_income_as_of", "Annual As-Of", False),
        ("balance_sheet_as_of", "Balance As-Of", False),
        ("Gross_Margin_annual", "Gross Margin % (annual)", True),
        ("Gross_Margin_TTM", "Gross Margin % (TTM)", True),
        ("ROIC_annual", "ROIC % (annual)", True),
        ("ROIC_TTM", "ROIC % (TTM)", True),
        ("FCF_Margin_annual", "FCF Margin % (annual)", True),
        ("FCF_Margin_TTM", "FCF Margin % (TTM)", True),
        ("Interest_Coverage_annual", "Interest Coverage (annual)", False),
        ("P/E_trailing", "P/E", False),
        ("EV/EBITDA", "EV/EBITDA", False),
        ("P/FCF_TTM_preferred", "P/FCF", False),
        ("PEG_yahoo", "PEG (Yahoo)", False),
        ("as_of_run_utc", "Run UTC", False),
    ]:
        if src in df.columns:
            series = df[src]
            if scale:
                out[dst] = (series.astype(float) * 100.0).round(2)
            else:
                if src in (
                    "Interest_Coverage_annual",
                    "P/E_trailing",
                    "EV/EBITDA",
                    "P/FCF_TTM_preferred",
                    "PEG_yahoo",
                ):
                    out[dst] = pd.to_numeric(series, errors="coerce").round(2)
                else:
                    out[dst] = series
    return out
