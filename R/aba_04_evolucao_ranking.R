aba_04_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Histórico Ranking', helpText('Em construção.')),
    card(card_header('Gráfico — Histórico Ranking'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Histórico Ranking'), DTOutput(ns('tabela')))
  )
}
aba_04_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Histórico Ranking') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Histórico Ranking'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
