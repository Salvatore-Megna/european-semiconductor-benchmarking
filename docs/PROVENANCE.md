# Provenance and validation

This is a reconstruction of a 2026 academic financial benchmarking study. The historical source files are genuinely recovered; the portable execution entry point is an **adaptation** of the final original script v7, not a new research methodology.

| Artifact | Where | Provenance |
|---|---|---|
| Corrected financial workbook | `data/raw/` | Recovered source, with source URLs and Excel formulas |
| Original R analysis v7 | `docs/source_archive/` | Preserved historical source with local Windows paths |
| Portable R entry point | `scripts/run_all.R` | Adapted from v7: relative paths and explicit data checks |
| Historical 15-sheet results | `results/risultati_storici.xlsx` | Archived comparison; v6 vs v7 generation cannot be proven from file alone |
| Advanced original LaTeX | `docs/source_archive/INDICATORI_2_revisionato.tex` | Recovered advanced revision |
| Report LaTeX + PDF | `report/` | Relative figure path added to source for portability; PDF from recovered report |
| Five original figures | `report/figures/` | Recovered study illustrations |

The last stylistic changes mentioned in the earlier chat are **not guaranteed** to be present in this particular report. Original R scripts v1–v6 and earlier workbooks are intentionally excluded from the canonical GitHub tree; their development history remains preserved in the separate user-provided recovery ZIP and previous Git commits.

**Methodology**: six companies × five years (2021–2025), 2020 as auxiliary base year; shared annual medians among all six companies; 1.5×IQR flags (no outlier removal); standardized PCA fitted to five ratios using `stats::prcomp`, with current ratio deliberately excluded. Currency differences, segment heterogeneity, accounting restatements and the small panel limit interpretation; no causal inference on AI demand is made.

**Audit completed independently**: all 180 computed indicator values, benchmark medians, five PCA eigenvalues and PCA association magnitudes agree with the recovered reference workbook. Two IQR outliers are Besi 2021 revenue growth and VAT Group 2022 asset turnover. A clean R CI run is still required to establish end-to-end reproducibility; workflow status is authoritative.

**Publication gate**: keep private until R CI succeeds, detailed report/figure review is finished, and third-party source-data/report redistribution rights and the code license are reviewed. Do not mislabel adapted scripts as untouched originals.
