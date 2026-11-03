# Regressione numerica contro il workbook dei risultati originale.
# Eseguire dopo Rscript scripts/run_all.R, dalla radice del repository.
if (!requireNamespace("readxl", quietly = TRUE)) stop("Pacchetto readxl mancante")
nuovo <- "outputs/tables/risultati_analisi_semiconductor.xlsx"
storico <- "results/risultati_storici.xlsx"
if (!file.exists(nuovo) || !file.exists(storico)) {
  stop("Eseguire prima la pipeline e controllare il workbook storico.")
}
leggi <- function(percorso, foglio) {
  as.data.frame(readxl::read_excel(percorso, sheet = foglio))
}
stopifnot(identical(sort(readxl::excel_sheets(nuovo)), sort(readxl::excel_sheets(storico))))

variabili <- c("revenue_growth", "ros", "rd_intensity",
               "asset_turnover", "autonomia_finanziaria", "current_ratio")
ordina <- function(x) x[order(x$company, x$year), , drop = FALSE]

indicatori <- ordina(leggi(nuovo, "Indicatori_R"))
riferimento <- ordina(leggi(storico, "Indicatori_R"))
stopifnot(nrow(indicatori) == 30L)
stopifnot(length(unique(indicatori$company)) == 6L)
stopifnot(identical(sort(unique(as.integer(indicatori$year))), 2021:2025))
stopifnot(!anyDuplicated(indicatori[c("company", "year")]))
stopifnot(identical(as.character(indicatori$company), as.character(riferimento$company)))
stopifnot(identical(as.integer(indicatori$year), as.integer(riferimento$year)))
stopifnot(isTRUE(all.equal(as.matrix(indicatori[, variabili]),
                           as.matrix(riferimento[, variabili]), tolerance = 1e-9,
                           check.attributes = FALSE)))

mediane <- leggi(nuovo, "Mediane_annuali")
mediane_storiche <- leggi(storico, "Mediane_annuali")
stopifnot(nrow(mediane) == 5L)
stopifnot(isTRUE(all.equal(as.matrix(mediane[, variabili]),
                           as.matrix(mediane_storiche[, variabili]),
                           tolerance = 1e-9, check.attributes = FALSE)))

pca <- leggi(nuovo, "PCA_varianza")
pca_storica <- leggi(storico, "PCA_varianza")
stopifnot(nrow(pca) == 5L)
stopifnot(isTRUE(all.equal(pca$autovalore, pca_storica$autovalore,
                           tolerance = 1e-8, check.attributes = FALSE)))
stopifnot(abs(sum(pca$varianza_spiegata[1:2]) - 0.7264552514878493) < 1e-6)

associazioni <- leggi(nuovo, "PCA_associazioni")
associazioni_storiche <- leggi(storico, "PCA_associazioni")
pcs <- paste0("PC", 1:5)
stopifnot(isTRUE(all.equal(abs(as.matrix(associazioni[, pcs])),
                           abs(as.matrix(associazioni_storiche[, pcs])),
                           tolerance = 1e-7, check.attributes = FALSE)))
outliers <- leggi(nuovo, "Outlier_IQR")
stopifnot(nrow(outliers) == 2L)
stopifnot(any(outliers$company == "Besi" & outliers$year == 2021 &
              outliers$indicatore == "revenue_growth"))
stopifnot(any(outliers$company == "VAT Group" & outliers$year == 2022 &
              outliers$indicatore == "asset_turnover"))
grafici <- c("01_crescita_innovazione", "02_redditivita_efficienza",
             "03_struttura_finanziaria", "04_pca_mappa_competitiva",
             "SUPPORTO_pca_varianza_spiegata")
for (nome in grafici) for (ext in c("pdf", "png")) {
  stopifnot(file.exists(file.path("outputs", "figures", paste0(nome, ".", ext))))
}
message("Verified 180 indicator values, five annual medians, PCA and plots.")
