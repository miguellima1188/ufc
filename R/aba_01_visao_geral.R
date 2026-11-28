aba_01_ui <- function(id) {
  ns <- NS(id)
  
  fmt_pct <- function(x) {
    val <- ifelse(x <= 1, x* 100, x)
    paste0(format(round(val, 1), decimal.mark = ","), "%")
  }
  
  min_ano <- min(evolucao_anual$ano, na.rm = TRUE)
  max_ano <- max(evolucao_anual$ano, na.rm = TRUE)
  
  tagList(
    layout_columns(
      col_widths = c(3, 3, 2, 2, 2),
      value_box(
        title = "Total de Lutas",
        value = format(kpis_gerais$total_lutas, big.mark = "."),
        theme = "danger"
      ),
      value_box(
        title = "Total de Eventos",
        value = format(kpis_gerais$total_eventos, big.mark = "."),
        theme = "dark"
      ),
      value_box(
        title = "Atletas",
        value = format(kpis_gerais$total_atletas, big.mark = "."),
        theme = "secondary"
      ),
      value_box(
        title = "Países",
        value = format(kpis_gerais$total_paises, big.mark = "."),
        theme = "secondary"
      ),
      value_box(
        title = "Taxa de KO/TKO",
        value = fmt_pct(kpis_gerais$pct_ko_geral),
        theme = "danger"
      )
    ),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros - Visão Geral",
        sliderInput(
          inputId = ns("periodo"),
          label = "Período (Anos):",
          min = min_ano,
          max = max_ano,
          value = c(max(min_ano, 2005), max_ano),
          sep = ""
        ),
        radioButtons(
          inputId = ns("visao_grafico"),
          label = "Métrica do gráfico:",
          choices = c(
            "Volume de Lutas e Eventos"         = "volume",
            "Lutas Masculinas vs. Femininas"    = "genero",
            "Métodos de Vitória (% KO/Sub/Dec)" = "metodos"
          ),
          selected = "volume"
        ),
        hr(),
        helpText(
          paste0(
            "Cobertura da base: ",
            format(kpis_gerais$data_inicio, "%d/%m/%Y"), " a ",
            format(kpis_gerais$data_fim, "%d/%m/%Y"), "."
          )
        )
      ),
      
      card(
        card_header("Evolução Histórica do UFC"),
        plotlyOutput(ns("grafico_evolucao"), height = "420px")
      ),
      
      card(
        card_header("Resumo Estatístico por Ano"),
        DTOutput(ns("tabela_anual"))
      )
    )
  )
}

aba_01_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    normalizar_pct <- function(x) {
      if (all(is.na(x))) return(x)
      if (max(x, na.rm = TRUE) <= 1) round(x * 100, 1) else round(x, 1)
    }
    
    dados_filtrados <- reactive({
      req(input$periodo)
      evolucao_anual %>%
        filter(ano >= input$periodo[1], ano <= input$periodo[2]) %>%
        mutate(
          pct_ko_100  = normalizar_pct(pct_ko),
          pct_sub_100 = normalizar_pct(pct_sub),
          pct_dec_100 = normalizar_pct(pct_dec),
          tempo_min   = round(tempo_medio_seg / 60, 2)
        )
    })
    
    output$grafico_evolucao <- renderPlotly({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      if (input$visao_grafico == "volume") {
        df_long <- df %>%
          select(ano, Lutas = total_lutas, Eventos = total_eventos) %>%
          pivot_longer(-ano, names_to = "Metrica", values_to = "Total")
        
        p <- ggplot(df_long, aes(x = ano, y = Total, color = Metrica)) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2) +
          scale_color_manual(values = c("Lutas" = COR_UFC_VERMELHO, "Eventos" = COR_UFC_ESCURO)) +
          labs(title = "Crescimento Anual de Lutas e Eventos", x = "Ano", y = "Quantidade", color = NULL) +
          tema_ufc
        
      } else if (input$visao_grafico == "genero") {
        df_long <- df %>%
          select(ano, Masculino = lutas_masculinas, Feminino = lutas_femininas) %>%
          pivot_longer(-ano, names_to = "Genero", values_to = "Lutas")
        
        p <- ggplot(df_long, aes(x = ano, y = Lutas, fill = Genero)) +
          geom_col(position = "stack") +
          scale_fill_manual(values = c("Masculino" = COR_UFC_ESCURO, "Feminino" = COR_UFC_VERMELHO)) +
          labs(title = "Distribuição Anual de Lutas por Género", x = "Ano", y = "Total de Lutas", fill = NULL) +
          tema_ufc
        
      } else {
        df_long <- df %>%
          select(ano, `KO/TKO` = pct_ko_100, Finalização = pct_sub_100, Decisão = pct_dec_100) %>%
          pivot_longer(-ano, names_to = "Metodo", values_to = "Percentual")
        
        p <- ggplot(df_long, aes(x = ano, y = Percentual, color = Metodo)) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2) +
          scale_color_manual(
            values = c("KO/TKO" = COR_UFC_VERMELHO, "Finalização" = COR_UFC_AZUL, "Decisão" = COR_UFC_CINZA)
          ) +
          labs(title = "Evolução dos Métodos de Vitória (%)", x = "Ano", y = "Percentual (%)", color = NULL) +
          tema_ufc
      }
      
      ggplotly(p) %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
    })
    output$tabela_anual <- renderDT({
      df_tab <- dados_filtrados() %>%
        arrange(desc(ano)) %>%
        transmute(
          Ano                 = ano,
          Eventos             = total_eventos,
          Lutas               = total_lutas,
          `Lutas Masc.`       = lutas_masculinas,
          `Lutas Fem.`        = lutas_femininas,
          `KO/TKO (%)`        = pct_ko_100,
          `Finalização (%)`   = pct_sub_100,
          `Decisão (%)`       = pct_dec_100,
          `Tempo Médio (min)` = tempo_min
        )
      
      datatable(
        df_tab,
        options  = opcoes_dt_padrao,
        rownames = FALSE
      )
    })
    
  })
}

