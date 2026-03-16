# Why Average Price Alone Is Too Narrow for Market Monitoring

A data analyst project on market monitoring, KPI design, and stress-day prioritization in Spain’s day-ahead electricity market using OMIE data (2018–2026).

## Project overview

This project evaluates whether **average daily wholesale price** is enough to summarize market stress in Spain’s day-ahead electricity market. The short answer is no.

Average daily price is a useful headline KPI, but it can hide important operational information such as:

- intraday dispersion,
- concentration of high-price hours,
- and broader stress across the trading day.

To address that limitation, the project builds and evaluates a broader monitoring framework based on hourly OMIE market data and cleared-curve summaries.

## Business question

If a market monitoring team only tracks average daily price, which types of stress days are likely to be missed?

## Main contribution

The project develops a **composite market stress score** designed for monitoring and prioritization, not for structural modeling or causal inference.

The final framework distinguishes between:

- a **core score** based on average price, daily range, and number of high-price hours,
- and an **extended score** that adds cleared-curve complexity as a secondary interpretive signal.

## Key findings

- Average daily price is useful, but incomplete as a standalone monitoring KPI.
- Daily intraday range adds meaningful information that the headline average can miss.
- The composite score does **not** outperform simple one-metric benchmarks on an internal recall-style validation.
- The score still adds value by identifying a **different and interpretable set of days** that single-metric rules do not prioritize in the same way.
- The strongest use case for the score is **triage and review prioritization**, not universal benchmark dominance.

## Data source

- **OMIE** (Operador del Mercado Ibérico de Energía): Spanish day-ahead hourly prices and day-ahead cleared-curve files across multiple histocical file formats

## Methodology

The pipeline is organized as a script-based analytical workflow:

1. Build a clean daily/hourly price panel
2. Parse raw OMIE cleared-curve files across different formats
3. Standardize and summarize cleared curves into hourly features
4. Merge hourly price data with hourly curve summaries
5. Aggregate the merged panel to daily monitoring features
6. Build and evaluate stress-score variants
7. Generate QA tables, figures, and the final report

## Repository structure

```text
R/
scripts/
outputs/
  figures/
  tables/
data_clean/
```
