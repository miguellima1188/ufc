aba_02_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(title = 'Filtros — Atletas', helpText('Em construção.')),
    card(card_header('Gráfico — Atletas'), plotlyOutput(ns('grafico'), height = '400px')),
    card(card_header('Tabela — Atletas'), DTOutput(ns('tabela')))
  )
}
aba_02_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({ plotly_empty() %>% layout(title = 'Em construção — Atletas') })
    output$tabela  <- renderDT({ datatable(data.frame(Status = 'Em construção — Atletas'), options = opcoes_dt_padrao, rownames = FALSE) })
  })
}
