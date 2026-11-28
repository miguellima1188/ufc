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
        radioButtons(
          ns("genero"),
          "Gênero:",
          choices  = c("Todos", "Masculino", "Feminino"),
          selected = "Todos"
        ),
        sliderInput(
          ns("top_n"),
          "Quantidade de Atletas no Gráfico:",
          min   = 5,
          max   = 25,
          value = 15,
          step  = 1
        ),
        tags$hr(),
        tags$small(
          class = "text-muted",
          tags$p("Cobertura: 2010 a 2026. O último ano está incompleto."),
          tags$p(
            "Nota: na base original, o campeão é codificado como posição 0 (exibido como 'Campeão').",
            "Atletas que mudaram de categoria aparecem em registros separados por divisão."
          )
        )
      ),
      
      card(
        card_header("Longevidade no Ranking"),
        plotlyOutput(ns("grafico_longevidade"), height = "420px")
      ),
      
      card(
        card_header("Leitura rápida dos dados (segue os filtros)"),
        uiOutput(ns("leitura_rapida"))
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
    
    observeEvent(input$genero, {
      cats <- sort(unique(na.omit(ranking_longevidade$weight_class)))
      
      if (input$genero == "Feminino") {
        cats <- cats[grepl("^Women", cats, ignore.case = TRUE)]
      } else if (input$genero == "Masculino") {
        cats <- cats[!grepl("^Women", cats, ignore.case = TRUE)]
      }
      
      updateSelectInput(
        session,
        "categoria",
        choices  = c("Todas", cats),
        selected = "Todas"
      )
    })
    atual_filtrado <- reactive({
      df <- rankings_atual
      
      if (input$genero == "Feminino") {
        df <- df %>% filter(grepl("^Women", weight_class, ignore.case = TRUE))
      } else if (input$genero == "Masculino") {
        df <- df %>% filter(!grepl("^Women", weight_class, ignore.case = TRUE))
      }
      
      if (input$categoria != "Todas") {
        df <- df %>% filter(weight_class == input$categoria)
      }
      df
    })
    
    longevidade_filtrada <- reactive({
      df <- ranking_longevidade
      
      if (input$genero == "Feminino") {
        df <- df %>% filter(grepl("^Women", weight_class, ignore.case = TRUE))
      } else if (input$genero == "Masculino") {
        df <- df %>% filter(!grepl("^Women", weight_class, ignore.case = TRUE))
      }
      
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
    
    output$leitura_rapida <- renderUI({
      df_long  <- longevidade_filtrada()
      df_atual <- atual_filtrado()
      
      if (nrow(df_long) == 0) {
        return(tags$p("Nenhum dado disponível para os filtros selecionados."))
      }
      
      rotulo_metrica <- switch(
        input$metrica,
        "semanas_ranqueado"       = "semanas ranqueado",
        "semanas_top5"            = "semanas no Top 5",
        "semanas_campeao_ou_top1" = "semanas como Campeão / Top 1"
      )
      
      top_atleta <- df_long %>%
        mutate(valor = .data[[input$metrica]]) %>%
        filter(!is.na(valor)) %>%
        arrange(desc(valor)) %>%
        slice(1)
      
      n_campeoes <- df_long %>%
        filter(melhor_posicao == 0) %>%
        summarise(n = n_distinct(fighter)) %>%
        pull(n)
      
      media_semanas <- round(mean(df_long$semanas_ranqueado, na.rm = TRUE), 1)
      escopo <- if (input$categoria == "Todas") "todas as categorias" else paste0("a categoria ", input$categoria)
      
      itens <- list(
        tags$li(
          "No recorte atual (", escopo, "), há ",
          tags$strong(format(n_distinct(df_long$fighter), big.mark = ".")),
          " atletas no histórico de rankings e ",
          tags$strong(format(nrow(df_atual), big.mark = ".")),
          " posições registradas no ranking mais recente."
        ),
        tags$li(
          "Na métrica selecionada, o destaque é ", tags$strong(top_atleta$fighter),
          " (", top_atleta$weight_class, "), com ",
          tags$strong(format(top_atleta$valor, big.mark = ".")),
          " ", rotulo_metrica, "."
        ),
        tags$li(
          "Ao todo, ", tags$strong(format(n_campeoes, big.mark = ".")),
          " atletas já alcançaram o cinturão (posição 0) nesse recorte, e a permanência média no ranking é de ",
          tags$strong(format(media_semanas, decimal.mark = ",")),
          " semanas."
        )
      )
      
      tags$ul(itens)
    })
    
  })
}