aba_08_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Finalizações', helpText('Em construção.')),
    card(card_header('Gráfico — Finalizações'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Finalizações'), DTOutput(ns('tabela')))
  )
}
aba_08_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Finalizações') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Finalizações'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
