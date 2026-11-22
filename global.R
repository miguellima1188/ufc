library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)

arquivos_rds <- list.files('dados_limpos', pattern = '\\.rds$', full.names = TRUE)
for (arq in arquivos_rds) {
  nome_obj <- tools::file_path_sans_ext(basename(arq))
  assign(nome_obj, readRDS(arq), envir = .GlobalEnv)
}

COR_UFC_VERMELHO <- '#D20A0A'
COR_UFC_ESCURO   <- '#1A1A1A'
COR_UFC_CINZA    <- '#6C757D'
COR_UFC_AZUL     <- '#0D6EFD'
COR_UFC_DOURADO  <- '#C9A227'

tema_ufc <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = 'bold', color = COR_UFC_ESCURO),
        panel.grid.minor = element_blank(), legend.position = 'bottom')

opcoes_dt_padrao <- list(pageLength = 10, scrollX = TRUE)
