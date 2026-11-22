aba_05_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Recordes', helpText('Em construção.')),
    card(card_header('Gráfico — Recordes'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Recordes'), DTOutput(ns('tabela')))
  )
}
aba_05_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Recordes') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Recordes'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
