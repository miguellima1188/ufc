# =============================================================================
# 01_download.R
# Baixa os arquivos CSV do TidyTuesday (UFC Athletes and Fight Data, 2026-07-07)
# e salva em data/raw/.
#
# Uso: source("R/01_download.R")
# =============================================================================

library(here)

# -----------------------------------------------------------------------------
# Configuração
# -----------------------------------------------------------------------------

base_url <- paste0(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
  "main/data/2026/2026-07-07/"
)

arquivos <- c(
  "ufc_athletes.csv",
  "ufc_fights.csv",
  "ufc_rankings_dataset.csv",
  "ufcstats_data.csv",
  "ultimate_ufc_dataset.csv"
)

dir_raw <- here::here("data", "raw")

# -----------------------------------------------------------------------------
# Preparação do diretório
# -----------------------------------------------------------------------------

if (!dir.exists(dir_raw)) {
  dir.create(dir_raw, recursive = TRUE)
  message("Diretório criado: ", dir_raw)
}

# -----------------------------------------------------------------------------
# Download
# -----------------------------------------------------------------------------

baixar_arquivo <- function(nome_arquivo, url_base, destino) {
  url_completa <- paste0(url_base, nome_arquivo)
  caminho <- file.path(destino, nome_arquivo)
  
  # Pula o download se o arquivo já existe e não está vazio
  if (file.exists(caminho) && file.size(caminho) > 0) {
    message("Já existe, pulando: ", nome_arquivo)
    return(invisible(caminho))
  }
  
  message("Baixando: ", nome_arquivo)
  
  tryCatch(
    {
      utils::download.file(
        url = url_completa,
        destfile = caminho,
        mode = "wb",
        quiet = TRUE
      )
      message("  ok (", round(file.size(caminho) / 1024), " KB)")
    },
    error = function(e) {
      warning("Falha ao baixar ", nome_arquivo, ": ", conditionMessage(e))
    }
  )
  
  invisible(caminho)
}

for (arq in arquivos) {
  baixar_arquivo(arq, base_url, dir_raw)
}