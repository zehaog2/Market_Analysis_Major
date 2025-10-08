# Intro
- All of the results & resources here are to help myself in narrowing queries and mitigating risk all based on techincal analysis. Please read the disclaimer.
- I hope you find anything here helpful.


## Portfolio
- Account Opening: Nov 2024.
- As of Oct, 2025: 
- Top Holdings: GME, SILJ, CC, ONDS, INTC
- - Cash position: 13%
- 2025 YoY Yield: 41%

## Resources
- [Insider trading](https://www.insiderdashboard.com/?tab=insider-trading-buys-sells)
- [FED rate](https://www.cmegroup.com/markets/interest-rates/cme-fedwatch-tool.html)
- [Earnings Calendar](https://earningshub.com/earnings-calendar/this-month)
- [Correlation Finder](https://www.portfoliovisualizer.com/asset-correlations?s=y&sl=5Pc1Rn5TUqsb9Xl7tbDnLS)
- [Prediction Market](https://polymarket.com/)


## USEFUL command lines
```
python3 portfolio_manager.py
# General charts based on portfolio (with different timeframe)

Run Script for all sectors:
python3 fear_greed_timeseries.py --sectors-only
python3 fear_greed_timeseries.py --industries-only

Run Web scrape script: python3 stock_scraper_upgraded.py


python3 sector_fear_greed_dashboard.py
python3 sector_fear_greed_dashboard.py --continue-on-error

python3 industry_lookup_tool.py
# Search Directly
python3 industry_lookup_tool.py --industry "Semiconductors"
# search indirectly
python industry_lookup_tool.py --search "software"
# custom
python industry_lookup_tool.py --industry "Banks" --days 90


# single stock analysis:
Rscript stock_corr_advanced.R 
# portfolio analysis:
Rscript stock_corr_advanced.R --batch config.json

Run this always!:
Rscript stock_corr_integrated.R --batch config.json 3 --advanced
```
## Risk Analysis
All files in R are dependent on each other. Check stock_corr, where my_strategy_analysis.pdf guides the usage.

## Fear & Greed Tracker
Track sector price momentum with buy/sell signals, work well with looking at sector ETF's momentum indicators side by side.

![sector_industrials_enhanced](https://github.com/user-attachments/assets/9757b99c-0903-481e-8f17-18b9b647ec77)
<img width="1851" alt="Screenshot 2025-06-17 at 11 42 12 AM" src="https://github.com/user-attachments/assets/c9c90459-302d-4376-9ef5-26c1a8b6be24" />


### 📈 Fear & Greed Details

#### Maths

The sentiment score (0-100) is calculated using: (adjust weight to your own liking please)
- **Price Momentum** (40%): Recent price performance
- **RSI** (30%): Relative Strength Index
- **Volume Analysis** (20%): Volume vs average
- **Volatility** (10%): Market volatility (inverse relationship)

#### Data Sources

- **Market Data**: Yahoo Finance API (yfinance)
- **Sector ETFs**: SPDR Sector ETFs (XLK, XLF, XLV, etc.)
- **Updates**: Real-time during market hours

## Improvements I might work on:
- I am considering setting up a dashboard to check my historical trades, aka visualizing my glory and failures.

## Suggested usage
-Not developed for day traders, option trading, or short-selling. Focus on the most basic: buying the stocks of growth companies. Can always replace anchor "SPY" with your favorite ETF. Complement short to mid term investment strategies, narrow down queries & help risk management. 

## Disclaimer
-This project is developed for educational and speculative purposes only. It is not intended to provide financial, investment, or trading advice. All analyses, tools, and results are for informational use only, and should not be relied upon for making investment decisions. Always do your own research (DYOR) and consult with a licensed financial advisor before making any investment decisions. The author of this project, Henry, is not liable for any financial losses or damages incurred from the use of this code. This project is not affiliated with any brokerage, trading platform, or financial institution.

-Licensed under the MIT License. See [LICENSE](./LICENSE) for details.
---
