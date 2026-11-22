aba_07_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Categorias', helpText('Em construção.')),
    card(card_header('Gráfico — Categorias'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Categorias'), DTOutput(ns('tabela')))
  )
}
aba_07_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Categorias') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Categorias'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
