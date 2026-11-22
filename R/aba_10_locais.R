aba_10_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Locais & Eventos', helpText('Em construção.')),
    card(card_header('Gráfico — Locais & Eventos'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Locais & Eventos'), DTOutput(ns('tabela')))
  )
}
aba_10_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Locais & Eventos') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Locais & Eventos'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
