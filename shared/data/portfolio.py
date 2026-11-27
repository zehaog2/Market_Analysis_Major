"""Load the single shared portfolio watchlist.

Canonical user-edited file (Desktop original):
  /Users/henryguo/Desktop/Market_Analysis_Major/Stock_corr/config.json

This suite symlinks that path at shared/data/portfolio_config.json so both
apps and the user's existing editor stay on one file.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import List

# Desktop original the user edits (absolute).
DESKTOP_CONFIG_PATH = Path(
    "/Users/henryguo/Desktop/Market_Analysis_Major/Stock_corr/config.json"
)

# Suite-local symlink (or copy) of the same file.
_SUITE_ROOT = Path(__file__).resolve().parents[2]
SUITE_CONFIG_PATH = Path(__file__).resolve().parent / "portfolio_config.json"

# Prefer env override → suite symlink → Desktop absolute → Stock_corr in clone.
DEFAULT_CONFIG_PATH = Path(
    os.environ.get(
        "PORTFOLIO_CONFIG",
        str(
            SUITE_CONFIG_PATH
            if SUITE_CONFIG_PATH.exists()
            else DESKTOP_CONFIG_PATH
        ),
    )
)


def resolve_config_path(config_path: str | Path | None = None) -> Path:
    if config_path is not None:
        return Path(config_path).expanduser().resolve()
    # Follow symlink to the real file when present.
    path = DEFAULT_CONFIG_PATH.expanduser()
    if path.exists():
        return path.resolve()
    if DESKTOP_CONFIG_PATH.exists():
        return DESKTOP_CONFIG_PATH.resolve()
    # Fall back to clone's Stock_corr/config.json (from Market_Analysis history).
    clone_cfg = _SUITE_ROOT / "Stock_corr" / "config.json"
    if clone_cfg.exists():
        return clone_cfg.resolve()
    raise FileNotFoundError(
        "No portfolio config found. Set PORTFOLIO_CONFIG or create "
        f"{DESKTOP_CONFIG_PATH}"
    )


def load_portfolio_tickers(config_path: str | Path | None = None) -> List[str]:
    """Return uppercased tickers from portfolio.stocks."""
    path = resolve_config_path(config_path)
    with open(path, "r", encoding="utf-8") as f:
        config = json.load(f)
    stocks = (config.get("portfolio") or {}).get("stocks") or []
    tickers = [str(t).strip().upper() for t in stocks if str(t).strip()]
    if not tickers:
        raise ValueError(f"No stocks in portfolio.stocks at {path}")
    return tickers


def config_path_used(config_path: str | Path | None = None) -> str:
    return str(resolve_config_path(config_path))
