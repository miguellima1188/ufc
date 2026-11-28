aba_03_ui <- function(id) {
  ns <- NS(id)
  
  categorias_rank <- c("Todas", sort(unique(na.omit(rankings_atual$weight_class))))
  
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Líder / Campeão Atual",
        value = textOutput(ns("kpi_lider")),
        theme = "danger"
      ),
      value_box(
        title = "Atletas no Histórico",
        value = textOutput(ns("kpi_total_atletas")),
        theme = "dark"
      ),
      value_box(
        title = "Recorde de Semanas",
        value = textOutput(ns("kpi_recorde")),
        theme = "secondary"
      ),
      value_box(
        title = "Última Atualização",
        value = textOutput(ns("kpi_data")),
        theme = "dark"
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros",
        width = 280,
        selectInput(
          ns("categoria"),
          "Categoria de Peso:",
          choices  = categorias_rank,
          selected = "Todas"
        ),
        radioButtons(
          ns("metrica"),
          "Métrica de Longevidade:",
          choices = c(
            "Semanas Ranqueado"      = "semanas_ranqueado",
            "Semanas no Top 5"       = "semanas_top5",
            "Semanas Campeão / Top 1" = "semanas_campeao_ou_top1"
          ),
          selected = "semanas_ranqueado"
        ),
        sliderInput(
          ns("top_n"),
          "Quantidade de Atletas no Gráfico:",
          min   = 5,
          max   = 25,
          value = 15,
          step  = 1
        )
      ),
      
      card(
        card_header("Longevidade no Ranking"),
        plotlyOutput(ns("grafico_longevidade"), height = "420px")
      ),
      
      card(
        card_header("Ranking Atual"),
        DTOutput(ns("tabela_ranking"))
      )
    )
  )
}
aba_03_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    atual_filtrado <- reactive({
      df <- rankings_atual
      if (input$categoria != "Todas") {
        df <- df %>% filter(weight_class == input$categoria)
      }
      df
    })
    
    longevidade_filtrada <- reactive({
      df <- ranking_longevidade
      if (input$categoria != "Todas") {
        df <- df %>% filter(weight_class == input$categoria)
      }
      df
    })
    
    output$kpi_lider <- renderText({
      df <- atual_filtrado()
      if (nrow(df) == 0) return("—")
      if (input$categoria == "Todas") {
        return(paste0(n_distinct(df$weight_class), " categorias"))
      }
      lider <- df %>%
        arrange(rank) %>%
        slice(1) %>%
        pull(fighter)
      if (length(lider) == 0 || is.na(lider)) "—" else lider
    })
    
    output$kpi_total_atletas <- renderText({
      df <- longevidade_filtrada()
      format(n_distinct(df$fighter), big.mark = ".")
    })
    
    output$kpi_recorde <- renderText({
      df <- longevidade_filtrada()
      if (nrow(df) == 0) return("—")
      val <- max(df[[input$metrica]], na.rm = TRUE)
      if (!is.finite(val)) return("—")
      paste0(format(val, big.mark = "."), " sem.")
    })
    
    output$kpi_data <- renderText({
      df <- atual_filtrado()
      if (nrow(df) == 0 || all(is.na(df$date))) return("—")
      format(max(as.Date(df$date), na.rm = TRUE), "%d/%m/%Y")
    })
    output$grafico_longevidade <- renderPlotly({
      rotulo_metrica <- switch(
        input$metrica,
        "semanas_ranqueado"       = "Semanas Ranqueado",
        "semanas_top5"            = "Semanas no Top 5",
        "semanas_campeao_ou_top1" = "Semanas Campeão / Top 1"
      )
      
      df_plot <- longevidade_filtrada() %>%
        mutate(
          valor    = .data[[input$metrica]],
          etiqueta = if (input$categoria == "Todas") {
            paste0(fighter, " (", weight_class, ")")
          } else {
            fighter
          }
        ) %>%
        filter(!is.na(valor), valor > 0) %>%
        arrange(desc(valor)) %>%
        slice_head(n = input$top_n)
      
      validate(need(nrow(df_plot) > 0, "Nenhum atleta encontrado para essa métrica e categoria."))
      
      p <- ggplot(
        df_plot,
        aes(
          x    = reorder(etiqueta, valor),
          y    = valor,
          text = paste0(
            "<b>", fighter, "</b>",
            "<br>Categoria: ", weight_class,
            "<br>", rotulo_metrica, ": ", valor, " semanas",
            "<br>Melhor posição: ", if_else(melhor_posicao == 0, "Campeão", paste0("#", melhor_posicao))
          )
        )
      ) +
        geom_col(fill = "#D20A0A", alpha = 0.85) +
        coord_flip() +
        labs(
          x = NULL,
          y = rotulo_metrica
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    output$tabela_ranking <- renderDT({
      df_tab <- atual_filtrado() %>%
        arrange(weight_class, rank) %>%
        transmute(
          Categoria = weight_class,
          Posição   = if_else(rank == 0, "Campeão", paste0("#", rank)),
          Atleta    = fighter,
          Data      = format(as.Date(date), "%d/%m/%Y")
        )
      
      datatable(
        df_tab,
        options  = opcoes_dt_padrao,
        rownames = FALSE
      )
    })
    
  })
}