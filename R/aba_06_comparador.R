aba_06_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Comparador', helpText('Em construção.')),
    card(card_header('Gráfico — Comparador'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Comparador'), DTOutput(ns('tabela')))
  )
}
aba_06_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Comparador') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Comparador'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
