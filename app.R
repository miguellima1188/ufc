ui <- page_navbar(
  title = 'UFC Analytics',
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = 'flatly', primary = COR_UFC_VERMELHO, secondary = COR_UFC_ESCURO),
  nav_panel('Visão Geral',       aba_01_ui('aba1')),
  nav_panel('Atletas',           aba_02_ui('aba2')),
  nav_panel('Rankings',          aba_03_ui('aba3')),
  nav_panel('Histórico Ranking', aba_04_ui('aba4')),
  nav_panel('Recordes',          aba_05_ui('aba5')),
  nav_panel('Comparador',        aba_06_ui('aba6')),
  nav_panel('Categorias',        aba_07_ui('aba7')),
  nav_panel('Finalizações',      aba_08_ui('aba8')),
  nav_panel('Cinturão',          aba_09_ui('aba9')),
  nav_panel('Locais & Eventos',  aba_10_ui('aba10'))
)

server <- function(input, output, session) {
  aba_01_server('aba1'); aba_02_server('aba2'); aba_03_server('aba3')
  aba_04_server('aba4'); aba_05_server('aba5'); aba_06_server('aba6')
  aba_07_server('aba7'); aba_08_server('aba8'); aba_09_server('aba9')
  aba_10_server('aba10')
}

shinyApp(ui, server)
