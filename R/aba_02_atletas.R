aba_02_ui <- function(id) {
  ns <- NS(id)
  
  categorias <- c("Todas", sort(unique(na.omit(atletas_limpo$weight_class))))
  generos    <- c("Todos", sort(unique(na.omit(atletas_limpo$gender))))
  bases      <- c("Todas", sort(unique(na.omit(atletas_limpo$stance))))
  
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Atletas Filtrados",
        value = textOutput(ns("kpi_total")),
        theme = "danger"
      ),
      value_box(
        title = "Idade Média",
        value = textOutput(ns("kpi_idade")),
        theme = "dark"
      ),
      value_box(
        title = "Altura Média",
        value = textOutput(ns("kpi_altura")),
        theme = "secondary"
      ),
      value_box(
        title = "Taxa Média de Vitória",
        value = textOutput(ns("kpi_winrate")),
        theme = "danger"
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros",
        width = 280,
        selectInput(
          ns("categoria"),
          "Categoria de Peso:",
          choices  = categorias,
          selected = "Todas"
        ),
        selectInput(
          ns("genero"),
          "Gênero:",
          choices  = generos,
          selected = "Todos"
        ),
        selectInput(
          ns("base"),
          "Base (Stance):",
          choices  = bases,
          selected = "Todas"
        ),
        sliderInput(
          ns("min_lutas"),
          "Mínimo de Lutas:",
          min   = 0,
          max   = 30,
          value = 5,
          step  = 1
        ),
        hr(),
        radioButtons(
          ns("tipo_grafico"),
          "Visualização:",
          choices = c(
            "Altura x Envergadura"    = "fisico",
            "Distribuição de Idade"   = "idade",
            "Taxa de Vitória x Lutas" = "desempenho"
          ),
          selected = "fisico"
        )
      ),
      
      card(
        card_header("Perfil dos Atletas"),
        plotlyOutput(ns("grafico_atletas"), height = "420px")
      ),
      
      card(
        card_header("Base de Atletas Filtrados"),
        DTOutput(ns("tabela_atletas"))
      )
    )
  )
}
aba_02_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    dados_filtrados <- reactive({
      df <- atletas_limpo %>%
        filter(total_fights >= input$min_lutas)
      
      if (input$categoria != "Todas") {
        df <- df %>% filter(weight_class == input$categoria)
      }
      if (input$genero != "Todos") {
        df <- df %>% filter(gender == input$genero)
      }
      if (input$base != "Todas") {
        df <- df %>% filter(stance == input$base)
      }
      
      df
    })
    
    output$kpi_total <- renderText({
      format(nrow(dados_filtrados()), big.mark = ".")
    })
    
    output$kpi_idade <- renderText({
      df <- dados_filtrados()
      if (nrow(df) == 0 || all(is.na(df$age))) return("—")
      paste0(round(mean(df$age, na.rm = TRUE), 1), " anos")
    })
    
    output$kpi_altura <- renderText({
      df <- dados_filtrados()
      if (nrow(df) == 0 || all(is.na(df$height_cm))) return("—")
      paste0(round(mean(df$height_cm, na.rm = TRUE), 1), " cm")
    })
    
    output$kpi_winrate <- renderText({
      df <- dados_filtrados()
      if (nrow(df) == 0 || all(is.na(df$win_rate))) return("—")
      media <- mean(df$win_rate, na.rm = TRUE)
      if (media <= 1) media <- media * 100
      paste0(round(media, 1), "%")
    })
    
    output$grafico_atletas <- renderPlotly({
      df <- dados_filtrados()
      validate(need(nrow(df) > 0, "Nenhum atleta encontrado com os filtros selecionados."))
      
      if (input$tipo_grafico == "fisico") {
        df_plot <- df %>% filter(!is.na(height_cm), !is.na(reach_cm))
        
        p <- ggplot(
          df_plot,
          aes(
            x     = height_cm,
            y     = reach_cm,
            color = stance,
            text  = paste0(
              "<b>", name, "</b>",
              "<br>Categoria: ", weight_class,
              "<br>Altura: ", height_cm, " cm",
              "<br>Envergadura: ", reach_cm, " cm"
            )
          )
        ) +
          geom_point(alpha = 0.7, size = 2.2) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
          labs(
            x     = "Altura (cm)",
            y     = "Envergadura (cm)",
            color = "Base"
          ) +
          theme_minimal()
        
      } else if (input$tipo_grafico == "idade") {
        df_plot <- df %>% filter(!is.na(age))
        
        p <- ggplot(df_plot, aes(x = age)) +
          geom_histogram(binwidth = 2, fill = "#D20A0A", color = "white", alpha = 0.85) +
          labs(
            x = "Idade (anos)",
            y = "Quantidade de Atletas"
          ) +
          theme_minimal()
        
      } else {
        df_plot <- df %>%
          filter(!is.na(total_fights), !is.na(win_rate)) %>%
          mutate(win_rate_pct = if_else(win_rate <= 1, win_rate * 100, win_rate))
        
        p <- ggplot(
          df_plot,
          aes(
            x     = total_fights,
            y     = win_rate_pct,
            color = weight_class,
            text  = paste0(
              "<b>", name, "</b>",
              "<br>Lutas: ", total_fights,
              "<br>Vitórias: ", wins, " | Derrotas: ", losses,
              "<br>Taxa de Vitória: ", round(win_rate_pct, 1), "%"
            )
          )
        ) +
          geom_point(alpha = 0.7, size = 2.2) +
          labs(
            x     = "Total de Lutas",
            y     = "Taxa de Vitória (%)",
            color = "Categoria"
          ) +
          theme_minimal()
      }
      
      ggplotly(p, tooltip = "text")
    })
    
    output$tabela_atletas <- renderDT({
      df_tab <- dados_filtrados() %>%
        mutate(
          win_rate_pct = round(if_else(win_rate <= 1, win_rate * 100, win_rate), 1)
        ) %>%
        arrange(desc(wins)) %>%
        transmute(
          Atleta              = name,
          Categoria           = weight_class,
          Base                = stance,
          Idade               = age,
          `Altura (cm)`       = height_cm,
          `Envergadura (cm)`  = reach_cm,
          Vitórias            = wins,
          Derrotas            = losses,
          `Total de Lutas`    = total_fights,
          `Taxa Vitória (%)`  = win_rate_pct
        )
      
      datatable(
        df_tab,
        options  = opcoes_dt_padrao,
        rownames = FALSE
      )
    })
    
  })
}