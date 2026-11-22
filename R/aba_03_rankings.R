aba_03_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Rankings por Peso', helpText('Em construção.')),
    card(card_header('Gráfico — Rankings por Peso'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Rankings por Peso'), DTOutput(ns('tabela')))
  )
}
aba_03_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Rankings por Peso') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Rankings por Peso'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
