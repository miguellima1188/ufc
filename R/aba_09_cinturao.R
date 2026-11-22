aba_09_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Cinturão', helpText('Em construção.')),
    card(card_header('Gráfico — Cinturão'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Cinturão'), DTOutput(ns('tabela')))
  )
}
aba_09_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Cinturão') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Cinturão'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
