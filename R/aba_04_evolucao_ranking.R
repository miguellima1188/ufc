aba_04_ui <- function(id) {
  ns <- NS(id)
  
  categorias_evo <- sort(unique(na.omit(rankings_limpo$weight_class)))
  anos_disponiveis <- as.numeric(format(as.Date(rankings_limpo$date), "%Y"))
  ano_min <- min(anos_disponiveis, na.rm = TRUE)
  ano_max <- max(anos_disponiveis, na.rm = TRUE)
  
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Melhor Posição",
        value = textOutput(ns("kpi_melhor_rank")),
        theme = "danger"
      ),
      value_box(
        title = "Semanas no Período",
        value = textOutput(ns("kpi_semanas")),
        theme = "dark"
      ),
      value_box(
        title = "Semanas como Campeão",
        value = textOutput(ns("kpi_semanas_champ")),
        theme = "secondary"
      ),
      value_box(
        title = "Atletas Comparados",
        value = textOutput(ns("kpi_atletas")),
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
          choices  = categorias_evo,
          selected = categorias_evo[1]
        ),
        selectizeInput(
          ns("atletas"),
          "Atletas para Comparar:",
          choices  = NULL,
          multiple = TRUE,
          options  = list(
            maxItems    = 6,
            placeholder = "Selecione até 6 atletas..."
          )
        ),
        sliderInput(
          ns("periodo"),
          "Período (Anos):",
          min   = ano_min,
          max   = ano_max,
          value = c(ano_min, ano_max),
          sep   = ""
        )
      ),
      
      card(
        card_header("Trajetória Histórica no Ranking"),
        plotlyOutput(ns("grafico_evolucao"), height = "460px")
      ),
      
      card(
        card_header("Resumo dos Atletas no Período"),
        DTOutput(ns("tabela_evolucao"))
      )
    )
  )
}
aba_04_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$categoria, {
      req(input$categoria)
      
      atletas_cat <- rankings_limpo %>%
        filter(weight_class == input$categoria) %>%
        count(fighter, sort = TRUE)
      
      top_iniciais <- head(atletas_cat$fighter, 3)
      
      updateSelectizeInput(
        session,
        "atletas",
        choices  = sort(atletas_cat$fighter),
        selected = top_iniciais,
        server   = FALSE
      )
    })
    
    dados_filtrados <- reactive({
      req(input$categoria)
      if (is.null(input$atletas) || length(input$atletas) == 0) {
        return(rankings_limpo[0, ])
      }
      
      rankings_limpo %>%
        mutate(
          date = as.Date(date),
          ano  = as.numeric(format(date, "%Y"))
        ) %>%
        filter(
          weight_class == input$categoria,
          fighter %in% input$atletas,
          ano >= input$periodo[1],
          ano <= input$periodo[2]
        )
    })
    
    output$kpi_melhor_rank <- renderText({
      df <- dados_filtrados()
      if (nrow(df) == 0 || all(is.na(df$rank))) return("—")
      melhor <- min(df$rank, na.rm = TRUE)
      if (melhor == 0) "Campeão" else paste0("#", melhor)
    })
    
    output$kpi_semanas <- renderText({
      df <- dados_filtrados()
      if (nrow(df) == 0) return("0")
      format(n_distinct(df$date), big.mark = ".")
    })
    
    output$kpi_semanas_champ <- renderText({
      df <- dados_filtrados()
      if (nrow(df) == 0) return("0")
      format(sum(df$rank == 0, na.rm = TRUE), big.mark = ".")
    })
    
    output$kpi_atletas <- renderText({
      df <- dados_filtrados()
      as.character(n_distinct(df$fighter))
    })
    output$grafico_evolucao <- renderPlotly({
      df <- dados_filtrados() %>%
        arrange(fighter, date)
      
      validate(need(nrow(df) > 0, "Selecione ao menos um atleta com registros no período."))
      
      p <- ggplot(
        df,
        aes(
          x     = date,
          y     = rank,
          color = fighter,
          group = fighter,
          text  = paste0(
            "<b>", fighter, "</b>",
            "<br>Data: ", format(date, "%d/%m/%Y"),
            "<br>Posição: ", if_else(rank == 0, "Campeão", paste0("#", rank))
          )
        )
      ) +
        geom_line(linewidth = 0.9, alpha = 0.9) +
        geom_point(size = 1.4, alpha = 0.75) +
        scale_y_reverse(
          breaks = c(0, 1, 3, 5, 10, 15),
          labels = c("Campeão", "#1", "#3", "#5", "#10", "#15")
        ) +
        labs(
          x     = "Data",
          y     = "Posição no Ranking",
          color = "Atleta"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    output$tabela_evolucao <- renderDT({
      df <- dados_filtrados()
      validate(need(nrow(df) > 0, "Nenhum dado disponível para a seleção atual."))
      
      df_tab <- df %>%
        group_by(fighter) %>%
        summarise(
          melhor_num        = min(rank, na.rm = TRUE),
          `Semanas`         = n_distinct(date),
          `Melhor Posição`  = if_else(melhor_num == 0, "Campeão", paste0("#", melhor_num)),
          `Semanas Campeão` = sum(rank == 0, na.rm = TRUE),
          `Primeira Data`   = format(min(date, na.rm = TRUE), "%d/%m/%Y"),
          `Última Data`     = format(max(date, na.rm = TRUE), "%d/%m/%Y"),
          .groups           = "drop"
        ) %>%
        arrange(melhor_num, desc(`Semanas`)) %>%
        select(Atleta = fighter, everything(), -melhor_num)
      
      datatable(
        df_tab,
        options  = opcoes_dt_padrao,
        rownames = FALSE
      )
    })
    
  })
}
