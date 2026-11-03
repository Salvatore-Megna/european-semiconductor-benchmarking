# Dataset and definitions

**Canonical source**: [Corrected financial workbook](raw/benchmark_semiconductor_europe_2020_2025_CORRETTO.xlsx), recovered unchanged.

The workbook has five sheets: `README` (methodology), `Raw_Data` (36 company–years with official source URLs and notes), `Indicators` (30 calculated analysis rows with Excel formulas), `Sources` (reports and disclosures), and `Data_Dictionary` (units and formulas).

Companies: ASML, ASM International, Besi, AIXTRON, VAT Group and Technoprobe. Main analysis period **2021–2025**; **2020** is an auxiliary base year for growth, average assets and revenue index charts, never a PCA observation.

All monetary totals in `Raw_Data` are **millions of local currency (EUR or CHF)**. Cross-company comparisons use dimensionless ratios and 2020-indexed series, not absolute cross-currency amounts.

| Indicator | Definition |
|---|---|
| Revenue growth | `revenue[t]/revenue[t-1] - 1` |
| ROS | `EBIT/revenue` |
| R&D intensity | `R&D expense/revenue` |
| Asset turnover | `revenue[t]/mean(assets[t],assets[t-1])` |
| Equity autonomy | `equity/assets` |
| Current ratio | `current assets/current liabilities` |

The **current ratio is excluded from PCA** but retained in descriptive benchmarking. No data are invented or imputed. Check `Sources`, `Raw_Data` notes and company accounting disclosures for restatements, rounded values and changes in consolidation perimeter.
