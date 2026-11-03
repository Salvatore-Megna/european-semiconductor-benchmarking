# ============================================================
# ANALISI BENCHMARKING - TECNOLOGIE EUROPEE PER SEMICONDUTTORI
# Imprese: ASML, ASM International, Besi, AIXTRON, VAT Group, Technoprobe
# Periodo principale: 2021-2025
# ============================================================

# ------------------------------------------------------------
# 0. CONTROLLO PACCHETTI
# ------------------------------------------------------------

pacchetti <- c("readxl", "dplyr", "tidyr", "ggplot2", "writexl")

mancanti <- pacchetti[
  !vapply(pacchetti, requireNamespace, logical(1), quietly = TRUE)
]

if (length(mancanti) > 0) {
  stop(
    paste0(
      "Mancano questi pacchetti: ",
      paste(mancanti, collapse = ", "),
      "\nInstallali una sola volta con:\ninstall.packages(c(",
      paste0('"', mancanti, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

message("1/9 - Pacchetti disponibili.")

# ------------------------------------------------------------
# 1. FILE DI INPUT E CARTELLE DI OUTPUT
# ------------------------------------------------------------

file_corretto <- "C:/Users/totim/Downloads/benchmark_semiconductor_europe_2020_2025_CORRETTO.xlsx"
file_precedente <- "C:/Users/totim/Downloads/benchmark_semiconductor_europe_2021_2025.xlsx"

if (file.exists(file_corretto)) {
  percorso_excel <- file_corretto
} else if (file.exists(file_precedente)) {
  percorso_excel <- file_precedente
} else {
  stop(
    paste0(
      "File Excel non trovato. Cerca uno di questi file nella cartella Download:\n",
      "- benchmark_semiconductor_europe_2020_2025_CORRETTO.xlsx\n",
      "- benchmark_semiconductor_europe_2021_2025.xlsx"
    )
  )
}

download_dir <- "C:/Users/totim/Downloads"

cartella_grafici <- file.path(
  download_dir,
  "grafici_semiconductor_finali_v7"
)

cartella_risultati <- file.path(
  download_dir,
  "risultati_semiconductor_finali_v7"
)

dir.create(
  cartella_grafici,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  cartella_risultati,
  showWarnings = FALSE,
  recursive = TRUE
)

message("2/9 - File Excel trovato e cartelle di output pronte.")

# Funzione per salvare ogni grafico:
# - PDF vettoriale: consigliato per Overleaf
# - PNG a 600 dpi: versione raster ad alta risoluzione
salva_grafico <- function(grafico, nome, width = 10, height = 6) {

  ggsave(
    filename = file.path(
      cartella_grafici,
      paste0(nome, ".pdf")
    ),
    plot = grafico,
    width = width,
    height = height,
    units = "in",
    device = "pdf",
    bg = "white"
  )

  ggsave(
    filename = file.path(
      cartella_grafici,
      paste0(nome, ".png")
    ),
    plot = grafico,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
}

# ------------------------------------------------------------
# 2. LETTURA DEI DATI GREZZI
# ------------------------------------------------------------

raw <- readxl::read_excel(
  percorso_excel,
  sheet = "Raw_Data"
) |>
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


# Fallback per i tre valori R&S 2020 che mancavano nella prima versione
# dell'Excel. Se si usa il file CORRETTO, questi valori sono già presenti.
# Fonti ufficiali:
# ASM Annual Report 2020: €139.002m
# VAT Annual Report 2020: CHF 41.5m
# Technoprobe bilancio consolidato aggregato 2019-2020: €20.559m

raw <- raw |>
  mutate(
    rd_expense = case_when(
      company == "ASM International" & year == 2020 & is.na(rd_expense) ~ 139.002,
      company == "VAT Group" & year == 2020 & is.na(rd_expense) ~ 41.500,
      company == "Technoprobe" & year == 2020 & is.na(rd_expense) ~ 20.559,
      TRUE ~ rd_expense
    )
  )

message(
  "3/9 - Raw_Data letto correttamente: ",
  nrow(raw),
  " righe."
)

# ------------------------------------------------------------
# 3. CALCOLO DEGLI INDICATORI
# ------------------------------------------------------------

# Il 2020 è usato solo come base per:
# - crescita ricavi 2021
# - Asset Turnover 2021

indicatori <- raw |>
  group_by(company) |>
  arrange(year, .by_group = TRUE) |>
  mutate(

    revenue_growth =
      revenue / lag(revenue) - 1,

    ros =
      operating_profit / revenue,

    rd_intensity =
      rd_expense / revenue,

    asset_turnover =
      revenue /
      ((total_assets + lag(total_assets)) / 2),

    autonomia_finanziaria =
      total_equity / total_assets,

    current_ratio =
      current_assets / current_liabilities
  ) |>
  ungroup() |>
  filter(
    year >= 2021,
    year <= 2025
  ) |>
  select(
    company,
    country,
    year,
    revenue_growth,
    ros,
    rd_intensity,
    asset_turnover,
    autonomia_finanziaria,
    current_ratio
  )

variabili <- c(
  "revenue_growth",
  "ros",
  "rd_intensity",
  "asset_turnover",
  "autonomia_finanziaria",
  "current_ratio"
)

if (nrow(indicatori) != 30) {
  stop(
    paste0(
      "Il dataset analitico dovrebbe avere 30 osservazioni, ma ne ha ",
      nrow(indicatori),
      "."
    )
  )
}

if (anyNA(indicatori[, variabili])) {
  stop(
    "Sono presenti valori mancanti negli indicatori. Controllare il file Excel."
  )
}

message(
  "4/9 - Indicatori calcolati: 30 osservazioni x 6 indicatori."
)


# ------------------------------------------------------------
# 3A. RICAVI INDICIZZATI - BASE 2020 = 100
# ------------------------------------------------------------

# I ricavi indicizzati servono nel grafico di crescita per mostrare
# la traiettoria cumulativa di ciascuna impresa, indipendentemente
# dalla diversa scala e valuta di bilancio.

ricavi_indicizzati <- raw |>
  group_by(company) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    revenue_index_2020 = revenue / revenue[year == 2020] * 100
  ) |>
  ungroup() |>
  filter(year >= 2020, year <= 2025) |>
  select(
    company,
    country,
    year,
    revenue_index_2020
  )

benchmark_ricavi_indicizzati <- ricavi_indicizzati |>
  group_by(year) |>
  summarise(
    benchmark = median(revenue_index_2020, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 4. ANALISI DESCRITTIVA
# ------------------------------------------------------------

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
    across(
      all_of(variabili),
      median
    ),
    .groups = "drop"
  )

# Identificazione outlier tramite regola IQR.
# Gli outlier vengono solo segnalati, non eliminati.
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
  filter(
    valore < limite_inf |
      valore > limite_sup
  ) |>
  ungroup() |>
  select(
    company,
    year,
    indicatore,
    valore,
    limite_inf,
    limite_sup
  )

message("5/9 - Analisi descrittiva completata.")

# ------------------------------------------------------------
# 5. BENCHMARK:
#    MEDIANA ANNUALE COMUNE DELLE 6 IMPRESE
# ------------------------------------------------------------

# Per ogni anno e indicatore calcoliamo una sola mediana comune.
# Ogni impresa viene quindi confrontata con lo stesso benchmark annuale.

benchmark_long <- indicatori |>
  pivot_longer(
    cols = all_of(variabili),
    names_to = "indicatore",
    values_to = "valore"
  ) |>
  group_by(
    year,
    indicatore
  ) |>
  mutate(
    benchmark = median(valore, na.rm = TRUE),
    scostamento = valore - benchmark
  ) |>
  ungroup() |>
  mutate(

    indicatore_label = case_when(
      indicatore == "revenue_growth" ~ "Crescita ricavi",
      indicatore == "ros" ~ "ROS",
      indicatore == "rd_intensity" ~ "Intensità R&S",
      indicatore == "asset_turnover" ~ "Asset Turnover",
      indicatore == "autonomia_finanziaria" ~ "Autonomia finanziaria",
      indicatore == "current_ratio" ~ "Current Ratio"
    ),

    # Gli indicatori espressi come proporzioni vengono
    # trasformati in punti percentuali solo nei grafici.
    valore_plot = case_when(
      indicatore %in% c(
        "revenue_growth",
        "ros",
        "rd_intensity",
        "autonomia_finanziaria"
      ) ~ valore * 100,
      TRUE ~ valore
    ),

    benchmark_plot = case_when(
      indicatore %in% c(
        "revenue_growth",
        "ros",
        "rd_intensity",
        "autonomia_finanziaria"
      ) ~ benchmark * 100,
      TRUE ~ benchmark
    ),

    scostamento_plot = valore_plot - benchmark_plot
  )

message("6/9 - Benchmark annuale comune calcolato.")

# ------------------------------------------------------------
# 6. GRAFICI FINALI PER IL REPORT
# ------------------------------------------------------------

tema_report <- theme_minimal(
  base_size = 12
) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10.5
    ),
    strip.text = element_text(
      face = "bold"
    ),
    axis.title.x = element_blank()
  )

sottotitolo_benchmark <-
  "La linea nera tratteggiata rappresenta la mediana annuale del campione"

# ------------------------------------------------------------
# FIGURA 1 - CRESCITA E INNOVAZIONE
# Ricavi indicizzati (2020=100) + intensità R&S effettiva
# ------------------------------------------------------------

rd_effettiva <- raw |>
  filter(
    year >= 2020,
    year <= 2025
  ) |>
  transmute(
    company,
    year,
    valore = rd_expense / revenue * 100
  )

if (anyNA(rd_effettiva$valore)) {
  stop("Manca almeno un valore R&S 2020-2025: controllare il foglio Raw_Data.")
}

benchmark_rd <- rd_effettiva |>
  group_by(year) |>
  summarise(
    benchmark = median(valore, na.rm = TRUE),
    .groups = "drop"
  )

fig1_ricavi <- ricavi_indicizzati |>
  transmute(
    company,
    year,
    indicatore = "Ricavi indicizzati (2020 = 100)",
    valore = revenue_index_2020
  )

fig1_rd <- rd_effettiva |>
  transmute(
    company,
    year,
    indicatore = "Intensità R&S (%)",
    valore
  )

fig1_dati <- bind_rows(
  fig1_ricavi,
  fig1_rd
) |>
  mutate(
    indicatore = factor(
      indicatore,
      levels = c(
        "Ricavi indicizzati (2020 = 100)",
        "Intensità R&S (%)"
      )
    )
  )

fig1_benchmark <- bind_rows(
  benchmark_ricavi_indicizzati |>
    transmute(
      year,
      indicatore = "Ricavi indicizzati (2020 = 100)",
      benchmark
    ),
  benchmark_rd |>
    transmute(
      year,
      indicatore = "Intensità R&S (%)",
      benchmark
    )
) |>
  mutate(
    indicatore = factor(
      indicatore,
      levels = c(
        "Ricavi indicizzati (2020 = 100)",
        "Intensità R&S (%)"
      )
    )
  )

p1 <- ggplot(
  fig1_dati,
  aes(
    x = year,
    y = valore,
    color = company,
    group = company
  )
) +
  geom_line(
    linewidth = 0.85
  ) +
  geom_point(
    size = 2.2
  ) +
  geom_line(
    data = fig1_benchmark,
    aes(
      x = year,
      y = benchmark,
      group = indicatore
    ),
    inherit.aes = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 0.75
  ) +
  facet_wrap(
    ~ indicatore,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2020:2025
  ) +
  labs(
    title = "Evoluzione dei ricavi e dell'intensità di R&S",
    subtitle = sottotitolo_benchmark,
    y = NULL,
    color = "Impresa"
  ) +
  tema_report

salva_grafico(
  p1,
  "01_crescita_innovazione",
  10,
  6
)

# ------------------------------------------------------------
# FUNZIONE GENERALE:
# VALORI EFFETTIVI + MEDIANA ANNUALE
# ------------------------------------------------------------

benchmark_linee <- benchmark_long |>
  distinct(
    year,
    indicatore,
    indicatore_label,
    benchmark_plot
  )

grafico_valori_benchmark <- function(
  indicatori_scelti,
  titolo,
  ordine_pannelli
) {

  dati_plot <- benchmark_long |>
    filter(
      indicatore %in% indicatori_scelti
    ) |>
    mutate(
      indicatore_label = factor(
        indicatore_label,
        levels = ordine_pannelli
      )
    )

  benchmark_plot_df <- benchmark_linee |>
    filter(
      indicatore %in% indicatori_scelti
    ) |>
    mutate(
      indicatore_label = factor(
        indicatore_label,
        levels = ordine_pannelli
      )
    )

  ggplot(
    dati_plot,
    aes(
      x = year,
      y = valore_plot,
      color = company,
      group = company
    )
  ) +
    geom_line(
      linewidth = 0.85
    ) +
    geom_point(
      size = 2.2
    ) +
    geom_line(
      data = benchmark_plot_df,
      aes(
        x = year,
        y = benchmark_plot,
        group = indicatore_label
      ),
      inherit.aes = FALSE,
      color = "black",
      linetype = "dashed",
      linewidth = 0.75
    ) +
    facet_wrap(
      ~ indicatore_label,
      scales = "free_y"
    ) +
    scale_x_continuous(
      breaks = 2021:2025
    ) +
    labs(
      title = titolo,
      subtitle = sottotitolo_benchmark,
      y = NULL,
      color = "Impresa"
    ) +
    tema_report
}

# ------------------------------------------------------------
# FIGURA 2 - REDDITIVITÀ ED EFFICIENZA
# ------------------------------------------------------------

p2 <- grafico_valori_benchmark(
  indicatori_scelti = c(
    "ros",
    "asset_turnover"
  ),
  titolo =
    "Evoluzione della redditività e dell'efficienza",
  ordine_pannelli = c(
    "ROS",
    "Asset Turnover"
  )
)

salva_grafico(
  p2,
  "02_redditivita_efficienza",
  10,
  6
)

# ------------------------------------------------------------
# FIGURA 3 - STRUTTURA FINANZIARIA
# ------------------------------------------------------------

p3 <- grafico_valori_benchmark(
  indicatori_scelti = c(
    "autonomia_finanziaria",
    "current_ratio"
  ),
  titolo =
    "Evoluzione della struttura finanziaria",
  ordine_pannelli = c(
    "Autonomia finanziaria",
    "Current Ratio"
  )
)

salva_grafico(
  p3,
  "03_struttura_finanziaria",
  10,
  6
)

message(
  "7/9 - Grafici finali con valori effettivi e benchmark salvati."
)

# ------------------------------------------------------------
# 7. ANALISI IN COMPONENTI PRINCIPALI (PCA)
# ------------------------------------------------------------

variabili_pca <- c(
  "revenue_growth",
  "ros",
  "rd_intensity",
  "asset_turnover",
  "autonomia_finanziaria"
)

matrice_pca <- indicatori |>
  select(
    all_of(variabili_pca)
  )

# center = TRUE e scale. = TRUE:
# PCA eseguita su variabili standardizzate.
pca <- prcomp(
  matrice_pca,
  center = TRUE,
  scale. = TRUE
)

# Autovalori e varianza spiegata
autovalori <- pca$sdev^2

pca_varianza <- data.frame(
  componente =
    paste0(
      "PC",
      seq_along(autovalori)
    ),

  autovalore =
    autovalori,

  varianza_spiegata =
    autovalori /
    sum(autovalori),

  varianza_cumulata =
    cumsum(
      autovalori /
        sum(autovalori)
    )
)

# Loading
pca_loadings <- data.frame(
  variabile =
    rownames(pca$rotation),

  pca$rotation,

  row.names = NULL
)

# Associazione (correlazione) tra variabili originarie e componenti.
# È più immediata da interpretare dei soli coefficienti degli autovettori.
pca_associazioni <- as.data.frame(
  sweep(
    pca$rotation,
    2,
    pca$sdev,
    FUN = "*"
  )
)

pca_associazioni <- cbind(
  variabile = rownames(pca_associazioni),
  pca_associazioni
)

rownames(pca_associazioni) <- NULL

pca_associazioni_report <- pca_associazioni |>
  transmute(
    Variabile = case_when(
      variabile == "revenue_growth" ~ "ΔRIC",
      variabile == "ros" ~ "ROS",
      variabile == "rd_intensity" ~ "R&S",
      variabile == "asset_turnover" ~ "AT",
      variabile == "autonomia_finanziaria" ~ "AF"
    ),
    PC1 = round(PC1, 3),
    PC2 = round(PC2, 3)
  )

pca_componenti_report <- pca_varianza |>
  transmute(
    Componente = componente,
    Autovalore = round(autovalore, 3),
    `Varianza spiegata (%)` = round(varianza_spiegata * 100, 1),
    `Varianza cumulata (%)` = round(varianza_cumulata * 100, 1)
  )

# Score delle 30 osservazioni impresa-anno
pca_scores <- data.frame(
  company = indicatori$company,
  year = indicatori$year,
  pca$x,
  row.names = NULL
)

# Matrice di correlazione delle cinque variabili PCA
correlazioni_pca <- as.data.frame(
  cor(matrice_pca)
)

correlazioni_pca <- cbind(
  variabile =
    rownames(correlazioni_pca),
  correlazioni_pca
)

rownames(
  correlazioni_pca
) <- NULL

# ------------------------------------------------------------
# 7A. SCREE PLOT - OUTPUT DI SUPPORTO
# ------------------------------------------------------------

# Questo grafico viene prodotto e conservato, ma non è previsto
# tra le quattro figure principali del report.

blu_report <- "#3E6F8E"

p_scree <- ggplot(
  pca_varianza,
  aes(
    x = componente,
    y = varianza_spiegata * 100
  )
) +
  geom_col(
    width = 0.7,
    fill = blu_report,
    color = blu_report
  ) +
  geom_text(
    aes(
      label = paste0(
        round(varianza_spiegata * 100, 1),
        "%"
      )
    ),
    vjust = -0.45,
    size = 3.6
  ) +
  labs(
    title = "Varianza spiegata dalle componenti principali",
    x = "Componente",
    y = "Varianza spiegata (%)"
  ) +
  expand_limits(y = 0) +
  coord_cartesian(
    ylim = c(
      0,
      max(pca_varianza$varianza_spiegata * 100) * 1.15
    )
  ) +
  tema_report +
  theme(
    legend.position = "none",
    axis.title.x = element_text()
  )

salva_grafico(
  p_scree,
  "SUPPORTO_pca_varianza_spiegata",
  8,
  5.5
)

# ------------------------------------------------------------
# 7B. MAPPA PCA FINALE PER IL REPORT
# ------------------------------------------------------------

scores_plot <- pca_scores |>
  select(
    company,
    year,
    PC1,
    PC2
  ) |>
  mutate(
    anno_label = if_else(
      year %in% c(2021, 2025),
      substr(as.character(year), 3, 4),
      ""
    )
  )

loadings_plot <- pca_loadings |>
  select(
    variabile,
    PC1,
    PC2
  )

# Frecce volutamente più corte rispetto alla prima versione:
# servono a leggere la direzione delle variabili senza invadere
# l'area occupata dalle osservazioni impresa-anno.
scala_frecce <- 0.34 * min(
  diff(range(scores_plot$PC1)) /
    diff(range(loadings_plot$PC1)),
  diff(range(scores_plot$PC2)) /
    diff(range(loadings_plot$PC2))
)

loadings_plot <- loadings_plot |>
  mutate(
    PC1_plot = PC1 * scala_frecce,
    PC2_plot = PC2 * scala_frecce,

    label = case_when(
      variabile == "revenue_growth" ~ "ΔRIC",
      variabile == "ros" ~ "ROS",
      variabile == "rd_intensity" ~ "R&S",
      variabile == "asset_turnover" ~ "AT",
      variabile == "autonomia_finanziaria" ~ "AF"
    ),

    # Piccoli spostamenti manuali e deterministici:
    # separano soprattutto ΔRIC e AT, che puntano nella stessa zona.
    label_x = case_when(
      label == "ΔRIC" ~ PC1_plot - 0.02,
      label == "AT"   ~ PC1_plot - 0.18,
      label == "ROS"  ~ PC1_plot - 0.08,
      label == "R&S"  ~ PC1_plot + 0.10,
      label == "AF"   ~ PC1_plot + 0.07
    ),

    label_y = case_when(
      label == "ΔRIC" ~ PC2_plot + 0.18,
      label == "AT"   ~ PC2_plot - 0.12,
      label == "ROS"  ~ PC2_plot - 0.10,
      label == "R&S"  ~ PC2_plot - 0.10,
      label == "AF"   ~ PC2_plot + 0.12
    )
  )

pc1_pct <- round(
  pca_varianza$varianza_spiegata[1] * 100,
  1
)

pc2_pct <- round(
  pca_varianza$varianza_spiegata[2] * 100,
  1
)

p_pca <- ggplot(
  scores_plot,
  aes(
    x = PC1,
    y = PC2,
    color = company,
    group = company
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    color = "grey75"
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    color = "grey75"
  ) +

  # Traiettorie temporali
  geom_path(
    linewidth = 0.70,
    alpha = 0.55
  ) +

  geom_point(
    size = 2.6
  ) +

  # Solo 21 e 25: inizio e fine del periodo.
  geom_text(
    data = scores_plot |>
      filter(anno_label != ""),
    aes(label = anno_label),
    vjust = -0.85,
    size = 3.1,
    fontface = "bold",
    show.legend = FALSE
  ) +

  # Frecce delle variabili
  geom_segment(
    data = loadings_plot,
    aes(
      x = 0,
      y = 0,
      xend = PC1_plot,
      yend = PC2_plot
    ),
    inherit.aes = FALSE,
    arrow = arrow(
      length = grid::unit(0.17, "cm")
    ),
    linewidth = 0.65,
    color = "grey20"
  ) +

  # Solo acronimi: nessun nome esteso sovrapposto ai punti.
  geom_text(
    data = loadings_plot,
    aes(
      x = label_x,
      y = label_y,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = "grey15"
  ) +

  labs(
    title = "Posizionamento competitivo nel piano PCA",
    subtitle = "Osservazioni impresa-anno, 2021–2025",
    x = paste0(
      "PC1 (",
      format(pc1_pct, decimal.mark = ","),
      "% della varianza)"
    ),
    y = paste0(
      "PC2 (",
      format(pc2_pct, decimal.mark = ","),
      "% della varianza)"
    ),
    color = "Impresa"
  ) +
  tema_report +
  theme(
    axis.title.x = element_text(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 11)
  )

salva_grafico(
  p_pca,
  "04_pca_mappa_competitiva",
  9,
  6.5
)

message("8/9 - PCA completata e grafici PCA finali salvati.")

# ------------------------------------------------------------
# 8. ESPORTAZIONE RISULTATI IN EXCEL
# ------------------------------------------------------------

file_risultati <- file.path(
  cartella_risultati,
  "risultati_analisi_semiconductor.xlsx"
)

writexl::write_xlsx(
  list(
    Indicatori_R =
      indicatori,

    Statistiche_descrittive =
      statistiche_descrittive,

    Mediane_annuali =
      mediane_annuali,

    Ricavi_indicizzati =
      ricavi_indicizzati,

    Benchmark_ricavi_ind =
      benchmark_ricavi_indicizzati,

    Benchmark_annuale =
      benchmark_long,

    Benchmark_linee =
      benchmark_linee,

    Outlier_IQR =
      outlier_iqr,

    PCA_varianza =
      pca_varianza,

    PCA_loadings =
      pca_loadings,

    PCA_associazioni =
      pca_associazioni,

    PCA_componenti_report =
      pca_componenti_report,

    PCA_associazioni_report =
      pca_associazioni_report,

    PCA_scores =
      pca_scores,

    PCA_correlazioni =
      correlazioni_pca
  ),
  path = file_risultati
)

message("9/9 - Risultati esportati.")

# ------------------------------------------------------------
# 9. OUTPUT PRINCIPALI IN CONSOLE
# ------------------------------------------------------------

cat(
  "\n\n==============================\n",
  "MEDIANE ANNUALI\n",
  "==============================\n"
)

print(
  mediane_annuali,
  width = Inf
)

cat(
  "\n\n==============================\n",
  "RICAVI INDICIZZATI - 2020=100\n",
  "==============================\n"
)

print(
  ricavi_indicizzati |>
    filter(year %in% c(2020, 2025)) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = revenue_index_2020,
      names_prefix = "anno_"
    ),
  width = Inf
)

cat(
  "\nBenchmark mediano dei ricavi indicizzati:\n"
)

print(
  benchmark_ricavi_indicizzati,
  width = Inf
)

cat(
  "\n\n==============================\n",
  "OUTLIER IQR - SOLO SEGNALAZIONE\n",
  "==============================\n"
)

print(
  outlier_iqr,
  width = Inf
)

cat(
  "\n\n==============================\n",
  "PCA - AUTOVALORI E VARIANZA\n",
  "==============================\n"
)

print(
  pca_varianza,
  row.names = FALSE
)

cat(
  "\n\n==============================\n",
  "PCA - ASSOCIAZIONI VARIABILI CON PC1 E PC2\n",
  "==============================\n"
)

print(
  pca_associazioni_report,
  row.names = FALSE
)

cat(
  "\n\nGrafici salvati in:\n",
  cartella_grafici,
  "\n"
)

cat(
  "\nRisultati numerici salvati in:\n",
  file_risultati,
  "\n\n"
)
