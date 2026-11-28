# ============================================================================
# 03 — GERA O GIF DA ABA 9 (CINTURÃO) COM O GGANIMATE
# ----------------------------------------------------------------------------
# Rodar a partir da raiz do projeto (a pasta que tem app.R):
#   source("scripts/03_gerar_gif_cinturao.R")
#
# Pacotes: install.packages(c("gganimate", "gifski"))
# Saída: www/cinturao_corrida.gif (o app exibe esse arquivo na aba Cinturão).
# Rode de novo sempre que os .rds de dados_limpos/ mudarem.
# ============================================================================

source("global.R")
source("R/aba_09_cinturao.R")

if (!CIN_TEM_GGANIMATE) {
  stop("Instale os pacotes: install.packages(c('gganimate', 'gifski'))")
}

dir.create("www", showWarnings = FALSE)

lutas <- cin_preparar_base()

# Todas as categorias e todo o período, top 10 por ano
cin_gerar_gif(lutas, CIN_GIF_ARQUIVO, top_n = 10, quadros_por_ano = 12, fps = 20)

message("GIF salvo em '", CIN_GIF_ARQUIVO, "' (",
        round(file.size(CIN_GIF_ARQUIVO) / 1024^2, 1), " MB).")
