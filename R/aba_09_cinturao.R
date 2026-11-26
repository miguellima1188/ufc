# ============================================================================
# ABA 9 — CINTURÃO: HISTÓRICO DE DISPUTAS, REINADOS E MAIORES DISPUTADORES
# ----------------------------------------------------------------------------
# Funções auxiliares começam com "cin_" para não dar conflito com as outras abaa
# ============================================================================

CIN_GIF_ARQUIVO <- file.path("www", "cinturao_corrida.gif")

CIN_TEM_GGANIMATE <- requireNamespace("gganimate", quietly = TRUE) &&
  requireNamespace("gifski", quietly = TRUE)

# Ordem das categorias: do mais leve ao mais pesado (masculino, depois feminino)
CIN_ORDEM_CATEGORIAS <- c(
  "Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight",
  "Middleweight", "Light Heavyweight", "Heavyweight",
  "Women's Strawweight", "Women's Flyweight", "Women's Bantamweight",
  "Women's Featherweight"
)

# Limpeza específica das disputas de cinturão 
# A coluna title_bout da base tem ruído. Corrigi aqui o que se segue:
#     Disputa de cinturão tem 5 rounds; lutas marcadas como title_bout com 3 ou 4
#     rounds saem. Exceção é a data 11/07/2020, em que as
#     duas disputas reais estão com no_of_rounds = 3 na base.
#     E lutas marcadas como title_bout com 5 rounds mas que não valem cinturão
#     Lista abaixo, identificada por data + lutador
CIN_DATAS_5R_CORRIGIDAS <- as.Date("2020-07-11")

CIN_EXCLUIR <- tibble::tibble(
  date = as.Date(c(
    "2026-01-31", "2026-01-31", "2026-01-31", "2025-09-13", "2025-08-16",
    "2023-02-04", "2023-02-04", "2023-02-04", "2023-02-04"
  )),
  r_fighter = c(
    "Sangwook Kim", "Keiichiro Nakamura", "Sulangrangbo", "Rodrigo Sezinando",
    "Alibi Idiris", "Jeka Saragih", "JeongYeong Lee", "Toshiomi Kazama",
    "SeungGuk Choi"
  )
)

# Funções auxiliares (começo)

# Percentual formatado em PT-BR
cin_pct <- function(x, d = 1) {
  paste0(format(round(100 * x, d), decimal.mark = ",", nsmall = d, trim = TRUE), "%")
}

# Inteiro com separador de milhar em PT-BR
cin_int <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

# Data em dd/mm/aaaa
cin_data <- function(x) {
  format(x, "%d/%m/%Y")
}