# ============================================================
# ANALISI BENCHMARKING - SEMICONDUCTOR TECHNOLOGY EUROPE
# Imprese: ASML, ASM International, Besi, AIXTRON, VAT Group, Technoprobe
# Periodo principale: 2021-2025
# ============================================================

# 0. PACCHETTI -------------------------------------------------
pacchetti <- c("readxl", "dplyr", "tidyr", "ggplot2", "writexl")

da_installare <- pacchetti[!pacchetti %in% rownames(installed.packages())]
if (length(da_installare) > 0) {
  install.packages(da_installare)
}

invisible(lapply(pacchetti, library, character.only = TRUE))

# 1. FILE E CARTELLE DI OUTPUT ---------------------------------
# Si apre una finestra: seleziona benchmark_semiconductor_europe_2021_2025.xlsx
percorso_excel <- file.choose()

# Cartelle automatiche nei Download di Windows
download_dir <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
if (!dir.exists(download_dir)) {
  download_dir <- path.expand("~/Downloads")
}

cartella_grafici <- file.path(download_dir, "grafici_semiconductor")
cartella_risultati <- file.path(download_dir, "risultati_semiconductor")

dir.create(cartella_grafici, showWarnings = FALSE, recursive = TRUE)
dir.create(cartella_risultati, showWarnings = FALSE, recursive = TRUE)

# Funzione: salva ogni grafico sia in PDF vettoriale sia in PNG a 600 dpi
salva_grafico <- function(grafico, nome, width = 10, height = 6) {
  ggsave(
    filename = file.path(cartella_grafici, paste0(nome, ".pdf")),
    plot = grafico, width = width, height = height, units = "in",
    device = "pdf", bg = "white"
  )
  ggsave(
    filename = file.path(cartella_grafici, paste0(nome, ".png")),
    plot = grafico, width = width, height = height, units = "in",
    dpi = 600, bg = "white"
  )
}

# 2. LETTURA DATI GREZZI ---------------------------------------
raw <- read_excel(percorso_excel, sheet = "Raw_Data") |>
  rename(
    company = Company,
    country = Country,
    currency = Currency,
    year = Year,
    revenue = `Revenue (LCY m)`,
    operating_profit = `Operating Profit / EBIT (LCY m)`,
    rd_expense = `R&D Expense (LCY m)`,
    total_assets = `Total Assets (LCY m)`,
    current_assets = `Current Assets (LCY m)`,
    current_liabilities = `Current Liabilities (LCY m)`,
    total_equity = `Total Equity (LCY m)`
  ) |>
  arrange(company, year)

# 3. CALCOLO DEGLI INDICATORI ----------------------------------
# Il 2020 viene mantenuto solo per calcolare Growth e Asset Turnover 2021.
indicatori <- raw |>
  group_by(company) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    revenue_growth = revenue / lag(revenue) - 1,
    ros = operating_profit / revenue,
    rd_intensity = rd_expense / revenue,
    asset_turnover = revenue / ((total_assets + lag(total_assets)) / 2),
    autonomia_finanziaria = total_equity / total_assets,
    current_ratio = current_assets / current_liabilities
  ) |>
  ungroup() |>
  filter(year >= 2021, year <= 2025) |>
  select(
    company, country, year,
    revenue_growth, ros, rd_intensity,
    asset_turnover, autonomia_finanziaria, current_ratio
  )

# Controlli essenziali
stopifnot(nrow(indicatori) == 30)

variabili <- c(
  "revenue_growth", "ros", "rd_intensity",
  "asset_turnover", "autonomia_finanziaria", "current_ratio"
)

if (anyNA(indicatori[, variabili])) {
  stop("Sono presenti valori mancanti negli indicatori: controllare il file Excel.")
}

cat("\nDataset analitico:", nrow(indicatori), "osservazioni e",
    length(variabili), "indicatori.\n")

# 4. ANALISI DESCRITTIVA ---------------------------------------
statistiche_descrittive <- indicatori |>
  pivot_longer(
    cols = all_of(variabili),
    names_to = "indicatore",
    values_to = "valore"
  ) |>
  group_by(indicatore) |>
  summarise(
    n = n(),
    media = mean(valore),
    mediana = median(valore),
    sd = sd(valore),
    minimo = min(valore),
    massimo = max(valore),
    .groups = "drop"
  )

mediane_annuali <- indicatori |>
  group_by(year) |>
  summarise(
    across(all_of(variabili), median),
    .groups = "drop"
  )

# Controllo outlier IQR: non vengono eliminati automaticamente
outlier_iqr <- indicatori |>
  pivot_longer(
    cols = all_of(variabili),
    names_to = "indicatore",
    values_to = "valore"
  ) |>
  group_by(indicatore) |>
  mutate(
    q1 = quantile(valore, 0.25),
    q3 = quantile(valore, 0.75),
    iqr = q3 - q1,
    limite_inf = q1 - 1.5 * iqr,
    limite_sup = q3 + 1.5 * iqr
  ) |>
  filter(valore < limite_inf | valore > limite_sup) |>
  ungroup() |>
  select(company, year, indicatore, valore, limite_inf, limite_sup)

# 5. BENCHMARK: MEDIANA DELLE ALTRE 5 IMPRESE -----------------
loo_median <- function(x) {
  vapply(
    seq_along(x),
    function(i) median(x[-i], na.rm = TRUE),
    numeric(1)
  )
}

benchmark_long <- indicatori |>
  pivot_longer(
    cols = all_of(variabili),
    names_to = "indicatore",
    values_to = "valore"
  ) |>
  group_by(year, indicatore) |>
  mutate(
    benchmark = loo_median(valore),
    scostamento = valore - benchmark
  ) |>
  ungroup() |>
  mutate(
    indicatore_label = case_when(
      indicatore == "revenue_growth" ~ "Crescita ricavi (p.p.)",
      indicatore == "ros" ~ "ROS (p.p.)",
      indicatore == "rd_intensity" ~ "Intensità R&S (p.p.)",
      indicatore == "asset_turnover" ~ "Asset Turnover",
      indicatore == "autonomia_finanziaria" ~ "Autonomia finanziaria (p.p.)",
      indicatore == "current_ratio" ~ "Current Ratio"
    ),
    scostamento_plot = case_when(
      indicatore %in% c(
        "revenue_growth", "ros",
        "rd_intensity", "autonomia_finanziaria"
      ) ~ scostamento * 100,
      TRUE ~ scostamento
    )
  )

# 6. GRAFICI DI BENCHMARKING -----------------------------------
tema_report <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_blank()
  )

grafico_benchmark <- function(indicatori_scelti, titolo) {
  benchmark_long |>
    filter(indicatore %in% indicatori_scelti) |>
    ggplot(
      aes(
        x = year,
        y = scostamento_plot,
        color = company,
        group = company
      )
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.2) +
    facet_wrap(~ indicatore_label, scales = "free_y") +
    scale_x_continuous(breaks = 2021:2025) +
    labs(
      title = titolo,
      y = "Scostamento dalla mediana delle altre imprese",
      color = "Impresa"
    ) +
    tema_report
}

p1 <- grafico_benchmark(
  c("revenue_growth", "rd_intensity"),
  "Crescita e innovazione"
)
salva_grafico(p1, "01_crescita_innovazione", 10, 6)

p2 <- grafico_benchmark(
  c("ros", "asset_turnover"),
  "Redditività ed efficienza"
)
salva_grafico(p2, "02_redditivita_efficienza", 10, 6)

p3 <- grafico_benchmark(
  c("autonomia_finanziaria", "current_ratio"),
  "Struttura finanziaria"
)
salva_grafico(p3, "03_struttura_finanziaria", 10, 6)

# 7. PCA --------------------------------------------------------
variabili_pca <- c(
  "revenue_growth",
  "ros",
  "rd_intensity",
  "asset_turnover",
  "autonomia_finanziaria"
)

matrice_pca <- indicatori |>
  select(all_of(variabili_pca))

# PCA su variabili standardizzate
pca <- prcomp(
  matrice_pca,
  center = TRUE,
  scale. = TRUE
)

# Autovalori e varianza spiegata
autovalori <- pca$sdev^2
pca_varianza <- data.frame(
  componente = paste0("PC", seq_along(autovalori)),
  autovalore = autovalori,
  varianza_spiegata = autovalori / sum(autovalori),
  varianza_cumulata = cumsum(autovalori / sum(autovalori))
)

# Loading
pca_loadings <- data.frame(
  variabile = rownames(pca$rotation),
  pca$rotation,
  row.names = NULL
)

# Score
pca_scores <- data.frame(
  company = indicatori$company,
  year = indicatori$year,
  pca$x,
  row.names = NULL
)

# Correlazioni tra le 5 variabili della PCA
correlazioni_pca <- as.data.frame(cor(matrice_pca))
correlazioni_pca <- cbind(
  variabile = rownames(correlazioni_pca),
  correlazioni_pca
)
rownames(correlazioni_pca) <- NULL

# Scree plot
p_scree <- ggplot(
  pca_varianza,
  aes(x = componente, y = varianza_spiegata * 100)
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(round(varianza_spiegata * 100, 1), "%")),
    vjust = -0.4, size = 3.5
  ) +
  labs(
    title = "Varianza spiegata dalle componenti principali",
    x = "Componente",
    y = "Varianza spiegata (%)"
  ) +
  ylim(0, max(pca_varianza$varianza_spiegata * 100) * 1.15) +
  tema_report +
  theme(legend.position = "none")

salva_grafico(p_scree, "04_pca_varianza_spiegata", 8, 5.5)

# Mappa PCA con traiettorie 2021-2025
scores_plot <- pca_scores |>
  select(company, year, PC1, PC2)

loadings_plot <- pca_loadings |>
  select(variabile, PC1, PC2)

# Scala le frecce dei loading per renderle leggibili nello spazio degli score
scala_frecce <- 0.70 * min(
  diff(range(scores_plot$PC1)) / diff(range(loadings_plot$PC1)),
  diff(range(scores_plot$PC2)) / diff(range(loadings_plot$PC2))
)

loadings_plot <- loadings_plot |>
  mutate(
    PC1_plot = PC1 * scala_frecce,
    PC2_plot = PC2 * scala_frecce,
    label = case_when(
      variabile == "revenue_growth" ~ "Crescita ricavi",
      variabile == "ros" ~ "ROS",
      variabile == "rd_intensity" ~ "R&S",
      variabile == "asset_turnover" ~ "Asset Turnover",
      variabile == "autonomia_finanziaria" ~ "Autonomia finanziaria"
    )
  )

p_pca <- ggplot(
  scores_plot,
  aes(x = PC1, y = PC2, color = company, group = company)
) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70") +
  geom_path(linewidth = 0.75, alpha = 0.65) +
  geom_point(size = 2.5) +
  geom_text(
    data = scores_plot |> filter(year %in% c(2021, 2025)),
    aes(label = year),
    vjust = -0.8,
    size = 3,
    show.legend = FALSE
  ) +
  geom_segment(
    data = loadings_plot,
    aes(
      x = 0, y = 0,
      xend = PC1_plot, yend = PC2_plot
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = grid::unit(0.18, "cm")),
    linewidth = 0.6,
    color = "grey25"
  ) +
  geom_text(
    data = loadings_plot,
    aes(
      x = PC1_plot * 1.10,
      y = PC2_plot * 1.10,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.3,
    color = "grey15"
  ) +
  labs(
    title = "Mappa competitiva delle imprese",
    subtitle = paste0(
      "PC1 = ", round(pca_varianza$varianza_spiegata[1] * 100, 1),
      "% | PC2 = ", round(pca_varianza$varianza_spiegata[2] * 100, 1), "%"
    ),
    x = "Prima componente principale",
    y = "Seconda componente principale",
    color = "Impresa"
  ) +
  tema_report

salva_grafico(p_pca, "05_pca_mappa_competitiva", 9, 6.5)

# 8. ESPORTAZIONE DEI RISULTATI --------------------------------
file_risultati <- file.path(
  cartella_risultati,
  "risultati_analisi_semiconductor.xlsx"
)

write_xlsx(
  list(
    Indicatori_R = indicatori,
    Statistiche_descrittive = statistiche_descrittive,
    Mediane_annuali = mediane_annuali,
    Benchmark_LOO = benchmark_long,
    Outlier_IQR = outlier_iqr,
    PCA_varianza = pca_varianza,
    PCA_loadings = pca_loadings,
    PCA_scores = pca_scores,
    PCA_correlazioni = correlazioni_pca
  ),
  path = file_risultati
)

# 9. OUTPUT PRINCIPALI IN CONSOLE ------------------------------
cat("\n--- MEDIANE ANNUALI ---\n")
print(mediane_annuali)

cat("\n--- OUTLIER IQR (solo segnalazione) ---\n")
print(outlier_iqr)

cat("\n--- PCA: AUTOVALORI E VARIANZA SPIEGATA ---\n")
print(pca_varianza)

cat("\n--- PCA: LOADING ---\n")
print(pca_loadings[, c("variabile", "PC1", "PC2")])

cat("\nGrafici salvati in:\n", cartella_grafici, "\n")
cat("\nRisultati salvati in:\n", file_risultati, "\n")
