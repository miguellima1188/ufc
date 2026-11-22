aba_01_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Visão Geral', helpText('Em construção.')),
    card(card_header('Gráfico — Visão Geral'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Visão Geral'), DTOutput(ns('tabela')))
  )
}
aba_01_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Visão Geral') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Visão Geral'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
