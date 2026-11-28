# ============================================================================
# ABA 8 FINALIZAÇÕES: RAIO-X DOS GOLPES E MÉTODOS POR ANO
# ----------------------------------------------------------------------------
# Funções auxiliares começam com "fin_" para não dar conflito com as outras abas.
# ============================================================================

# Funções auxiliares

# Percentual formatado em PT-BR
fin_pct <- function(x, d = 1) {
  paste0(format(round(100 * x, d), decimal.mark = ",", nsmall = d, trim = TRUE), "%")
}

# Inteiro com separador de milhar em PT-BR.
fin_int <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

# Segundos -> "m:ss".
fin_mmss <- function(seg) {
  s <- as.integer(round(seg))
  sprintf("%d:%02d", s %/% 60L, s %% 60L)
}

# Valor mais frequente (ignora NA). Devolve NA se não houver nenhum.
fin_moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Média de tempo em minutos (ignora NA). Devolve NA se não houver nenhum.
fin_media_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  round(mean(x) / 60, 2)
}

# Dicionário: detalhe original (em inglês) -> técnica (PT) e família.
fin_dicionario <- function() {
  tibble::tribble(
    ~detalhe,               ~tecnica,                      ~familia,
    # --- KO/TKO: golpes
    "Punch",                "Socos",                       "Socos",
    "Punches",              "Socos",                       "Socos",
    "Elbow",                "Cotoveladas",                 "Cotoveladas",
    "Elbows",               "Cotoveladas",                 "Cotoveladas",
    "Knee",                 "Joelhadas",                   "Joelhadas",
    "Knees",                "Joelhadas",                   "Joelhadas",
    "Flying Knee",          "Joelhada voadora",            "Joelhadas",
    "Kick",                 "Chutes",                      "Chutes",
    "Kicks",                "Chutes",                      "Chutes",
    "Spinning Back Fist",   "Backfist giratório",          "Golpes giratórios",
    "Spinning Back Kick",   "Chute giratório",             "Golpes giratórios",
    "Spinning Back Elbow",  "Cotovelada giratória",        "Golpes giratórios",
    "Slam",                 "Slam",                        "Slam",
    # --- Finalizações: estrangulamentos
    "Rear Naked Choke",     "Mata-leão",                   "Estrangulamentos",
    "Guillotine Choke",     "Guilhotina",                  "Estrangulamentos",
    "Arm Triangle",         "Triângulo de braço",          "Estrangulamentos",
    "Triangle Choke",       "Triângulo",                   "Estrangulamentos",
    "D'arce Choke",         "D'Arce",                      "Estrangulamentos",
    "Anaconda Choke",       "Anaconda",                    "Estrangulamentos",
    "North-South Choke",    "Norte-sul",                   "Estrangulamentos",
    "Ezekiel Choke",        "Ezequiel",                    "Estrangulamentos",
    "Von Flue Choke",       "Von Flue",                    "Estrangulamentos",
    "Peruvian Necktie",     "Peruvian necktie",            "Estrangulamentos",
    "Inverted Triangle",    "Triângulo invertido",         "Estrangulamentos",
    "Other - Choke",        "Outro estrangulamento",       "Estrangulamentos",
    # --- Finalizações: chaves de braço
    "Armbar",               "Chave de braço",              "Chaves de braço",
    "Straight Armbar",      "Chave de braço reta",         "Chaves de braço",
    "Kimura",               "Kimura",                      "Chaves de braço",
    "Keylock",              "Americana",                   "Chaves de braço",
    "Triangle Armbar",      "Triângulo + chave de braço",  "Chaves de braço",
    "Omoplata",             "Omoplata",                    "Chaves de braço",
    # --- Finalizações: chaves de perna
    "Kneebar",              "Chave de joelho",             "Chaves de perna",
    "Heel Hook",            "Chave de calcanhar",          "Chaves de perna",
    "Ankle Lock",           "Chave de tornozelo",          "Chaves de perna",
    # --- Finalizações: outras
    "Neck Crank",           "Neck crank",                  "Outras",
    "Twister",              "Twister",                     "Outras",
    "Scarf Hold",           "Scarf hold",                  "Outras",
    "Other - Lock",         "Outra chave",                 "Outras",
    "Injury",               "Lesão",                       "Outras"
  )
}

# Base de análise: uma linha por luta com resultado conhecido.
#   metodo  : KO/TKO | Finalização | Decisão | Outros (DQ, CNC...)
#   detalhe : finish_details original (só para KO/TKO e Finalização)
#   tecnica : nome da técnica em PT   (NA quando o detalhe não foi registrado)
#   familia : grupo da técnica        (NA quando o detalhe não foi registrado)
fin_preparar_base <- function() {
  ufc_lutas_completo %>%
    filter(!is.na(finish)) %>%
    transmute(
      ano = as.integer(format(date, "%Y")),
      gender,
      weight_class,
      finish,
      metodo = case_when(
        finish == "KO/TKO" ~ "KO/TKO",
        finish == "SUB" ~ "Finalização",
        finish %in% c("U-DEC", "S-DEC", "M-DEC") ~ "Decisão",
        TRUE ~ "Outros"
      ),
      detalhe = if_else(finish %in% c("KO/TKO", "SUB"),
                        na_if(finish_details, ""), NA_character_),
      tempo_seg = total_fight_time_secs,
      finish_round
    ) %>%
    left_join(fin_dicionario(), by = "detalhe") %>%
    mutate(
      # detalhe que não está no dicionário: mantém o nome original
      tecnica = if_else(!is.na(detalhe) & is.na(tecnica), detalhe, tecnica),
      familia = case_when(
        !is.na(familia) ~ familia,
        !is.na(detalhe) & metodo == "KO/TKO" ~ "Outros",
        !is.na(detalhe) ~ "Outras",
        TRUE ~ NA_character_
      )
    )
}

# Conta lutas por ano x grupo
fin_contagem_ano <- function(df, niveis) {
  df %>%
    count(ano, grupo, name = "n") %>%
    complete(ano = sort(unique(df$ano)), grupo = niveis, fill = list(n = 0L)) %>%
    group_by(ano) %>%
    mutate(total_ano = sum(n),
           pct = ifelse(total_ano > 0, 100 * n / total_ano, 0)) %>%
    ungroup() %>%
    mutate(grupo = factor(grupo, levels = niveis))
}

# Gráfico de colunas empilhadas por ano
fin_graf_empilhado <- function(d, paleta, titulo, usar_pct) {
  d$valor <- if (usar_pct) d$pct else d$n
  d$texto <- paste0(
    "<b>", d$ano, "</b><br>", d$grupo, ": ", d$n, " lutas (",
    formatC(d$pct, format = "f", digits = 1, decimal.mark = ","), "%)"
  )
  p <- ggplot(d, aes(x = ano, y = valor, fill = grupo, text = texto)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = paleta, drop = FALSE) +
    scale_x_continuous(breaks = function(x) {
      seq(ceiling(x[1]), floor(x[2]), by = ifelse(diff(x) > 12, 2, 1))
    }) +
    labs(title = titulo, x = "Ano", fill = NULL,
         y = if (usar_pct) "% das finalizações" else "Quantidade de lutas") +
    tema_ufc
  if (usar_pct) {
    p <- p + scale_y_continuous(labels = function(v) paste0(v, "%"))
  }
  p
}

# ---- UI ----------------------------------------------------------------------

aba_08_ui <- function(id) {
  ns <- NS(id)
  
  lutas <- fin_preparar_base()
  min_ano <- min(lutas$ano, na.rm = TRUE)
  max_ano <- max(lutas$ano, na.rm = TRUE)
  categorias <- sort(unique(lutas$weight_class[!is.na(lutas$weight_class)]))
  
  # Anos em que muitas finalizações não têm técnica registrada (aviso de qualidade)
  cobertura <- lutas %>%
    filter(metodo %in% c("KO/TKO", "Finalização")) %>%
    group_by(ano) %>%
    summarise(sem_detalhe = mean(is.na(detalhe)), .groups = "drop") %>%
    filter(sem_detalhe > 0.2)
  aviso_cobertura <- if (nrow(cobertura) > 0) {
    paste0(
      "Nota: em ", paste(cobertura$ano, collapse = " e "),
      " boa parte dos KO/TKO e finalizações está sem a técnica registrada na base ",
      "(aparece como 'Sem detalhe'). Variações nesses anos podem ser efeito da coleta."
    )
  } else {
    NULL
  }
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros - Finalizações",
      width = 300,
      sliderInput(
        inputId = ns("periodo"),
        label = "Período (anos):",
        min = min_ano, max = max_ano,
        value = c(min_ano, max_ano),
        step = 1, sep = ""
      ),
      radioButtons(
        inputId = ns("genero"),
        label = "Gênero:",
        choices = c("Todos" = "todos", "Masculino" = "MALE", "Feminino" = "FEMALE"),
        selected = "todos"
      ),
      selectInput(
        inputId = ns("categoria"),
        label = "Categoria de peso:",
        choices = c("Todas as categorias" = "todas", categorias),
        selected = "todas"
      ),
      hr(),
      radioButtons(
        inputId = ns("visao"),
        label = "Visão do gráfico principal:",
        choices = c(
          "Métodos de vitória por ano"          = "metodos",
          "Golpes dos KO/TKO por ano"           = "golpes",
          "Tipos de finalização por ano"        = "submissoes",
          "Mapa de calor: técnica x ano"        = "calor",
          "Tempo médio até o fim da luta"       = "tempo"
        ),
        selected = "metodos"
      ),
      conditionalPanel(
        condition = "['metodos','golpes','submissoes'].indexOf(input.visao) >= 0",
        ns = ns,
        radioButtons(
          inputId = ns("escala"),
          label = "Escala:",
          choices = c("Percentual (%)" = "pct", "Quantidade" = "n"),
          selected = "pct"
        )
      ),
      hr(),
      helpText(
        paste0("Cobertura: ", min_ano, " a ", max_ano,
               ". O último ano está incompleto.")
      ),
      if (!is.null(aviso_cobertura)) helpText(aviso_cobertura)
    ),
    
    # KPIs
    layout_columns(
      col_widths = c(2, 2, 2, 3, 3),
      value_box(title = "Taxa de finalização",
                value = textOutput(ns("kpi_taxa")),
                textOutput(ns("kpi_taxa_sub")),
                theme = "danger"),
      value_box(title = "KO/TKO",
                value = textOutput(ns("kpi_ko")),
                textOutput(ns("kpi_ko_sub")),
                theme = "dark"),
      value_box(title = "Finalizações",
                value = textOutput(ns("kpi_sub")),
                textOutput(ns("kpi_sub_sub")),
                theme = "secondary"),
      value_box(title = "Golpe mais letal",
                value = textOutput(ns("kpi_golpe")),
                textOutput(ns("kpi_golpe_sub")),
                theme = "danger"),
      value_box(title = "Finalização mais comum",
                value = textOutput(ns("kpi_tecnica")),
                textOutput(ns("kpi_tecnica_sub")),
                theme = "dark")
    ),
    
    card(
      full_screen = TRUE,
      card_header("Raio-X anual dos métodos de vitória"),
      plotlyOutput(ns("grafico"), height = "440px")
    ),
    
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Top 10 técnicas de finalização"),
        plotlyOutput(ns("grafico_tecnicas"), height = "380px")
      ),
      card(
        full_screen = TRUE,
        card_header("Em que round a luta termina"),
        plotlyOutput(ns("grafico_round"), height = "380px")
      )
    ),
    
    card(
      card_header("Leitura rápida dos dados (segue os filtros)"),
      uiOutput(ns("insights"))
    ),
    
    navset_card_tab(
      title = "Tabelas",
      nav_panel("Resumo anual", DTOutput(ns("tabela_anual"))),
      nav_panel("Técnica x ano", DTOutput(ns("tabela_tecnicas")))
    )
  )
}

# ---- Server ------------------------------------------------------------------

aba_08_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    lutas <- fin_preparar_base()
    
    msg_vazio <- "Nenhuma luta para a combinação de filtros selecionada."
    
    # Paletas em concordância com o resto
    pal_metodo <- c(
      "KO/TKO"      = COR_UFC_VERMELHO,
      "Finalização" = COR_UFC_AZUL,
      "Decisão"     = COR_UFC_CINZA,
      "Outros"      = COR_UFC_DOURADO
    )
    pal_golpes <- c(
      "Socos"             = COR_UFC_VERMELHO,
      "Chutes"            = COR_UFC_DOURADO,
      "Cotoveladas"       = COR_UFC_ESCURO,
      "Joelhadas"         = COR_UFC_AZUL,
      "Golpes giratórios" = "#6F42C1",
      "Slam"              = "#20C997",
      "Outros"            = "#ADB5BD",
      "Sem detalhe"       = "#DEE2E6"
    )
    pal_subs <- c(
      "Estrangulamentos" = COR_UFC_AZUL,
      "Chaves de braço"  = COR_UFC_VERMELHO,
      "Chaves de perna"  = COR_UFC_DOURADO,
      "Outras"           = COR_UFC_ESCURO,
      "Sem detalhe"      = "#DEE2E6"
    )
    
    dados <- reactive({
      req(input$periodo, input$genero, input$categoria)
      d <- lutas %>%
        filter(ano >= input$periodo[1], ano <= input$periodo[2])
      if (input$genero != "todos") {
        d <- d %>% filter(gender == input$genero)
      }
      if (input$categoria != "todas") {
        d <- d %>% filter(weight_class == input$categoria)
      }
      d
    })
    
    # KPIs
    kpis <- reactive({
      df <- dados()
      vazio <- list(
        taxa = "-", taxa_sub = "", ko = "-", ko_sub = "", sub = "-", sub_sub = "",
        golpe = "-", golpe_sub = "", tecnica = "-", tecnica_sub = ""
      )
      if (nrow(df) == 0) return(vazio)
      
      n <- nrow(df)
      n_ko <- sum(df$metodo == "KO/TKO")
      n_sub <- sum(df$metodo == "Finalização")
      
      golpes <- df %>%
        filter(metodo == "KO/TKO", !is.na(familia)) %>%
        count(familia, sort = TRUE)
      tecnicas <- df %>%
        filter(metodo == "Finalização", !is.na(tecnica)) %>%
        count(tecnica, sort = TRUE)
      
      list(
        taxa = fin_pct((n_ko + n_sub) / n),
        taxa_sub = paste0(fin_int(n_ko + n_sub), " de ", fin_int(n), " lutas"),
        ko = fin_int(n_ko),
        ko_sub = paste0(fin_pct(n_ko / n), " das lutas"),
        sub = fin_int(n_sub),
        sub_sub = paste0(fin_pct(n_sub / n), " das lutas"),
        golpe = if (nrow(golpes) > 0) golpes$familia[1] else "-",
        golpe_sub = if (nrow(golpes) > 0) {
          paste0(fin_pct(golpes$n[1] / sum(golpes$n)), " dos KO/TKO com golpe registrado")
        } else "",
        tecnica = if (nrow(tecnicas) > 0) tecnicas$tecnica[1] else "-",
        tecnica_sub = if (nrow(tecnicas) > 0) {
          paste0(fin_pct(tecnicas$n[1] / sum(tecnicas$n)), " das finalizações com técnica registrada")
        } else ""
      )
    })
    
    output$kpi_taxa        <- renderText(kpis()$taxa)
    output$kpi_taxa_sub    <- renderText(kpis()$taxa_sub)
    output$kpi_ko          <- renderText(kpis()$ko)
    output$kpi_ko_sub      <- renderText(kpis()$ko_sub)
    output$kpi_sub         <- renderText(kpis()$sub)
    output$kpi_sub_sub     <- renderText(kpis()$sub_sub)
    output$kpi_golpe       <- renderText(kpis()$golpe)
    output$kpi_golpe_sub   <- renderText(kpis()$golpe_sub)
    output$kpi_tecnica     <- renderText(kpis()$tecnica)
    output$kpi_tecnica_sub <- renderText(kpis()$tecnica_sub)
    
    # Gráfico principal
    output$grafico <- renderPlotly({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      usar_pct <- !identical(input$escala, "n")
      
      if (input$visao == "metodos") {
        d <- df %>% mutate(grupo = metodo)
        d <- fin_contagem_ano(d, c("KO/TKO", "Finalização", "Decisão", "Outros"))
        p <- fin_graf_empilhado(
          d, pal_metodo, "Como as lutas terminam, ano a ano",
          usar_pct
        )
        if (usar_pct) {
          p <- p + labs(y = "% das lutas")
        }
        
      } else if (input$visao == "golpes") {
        d <- df %>%
          filter(metodo == "KO/TKO") %>%
          mutate(grupo = coalesce(familia, "Sem detalhe"))
        validate(need(nrow(d) > 0, msg_vazio))
        d <- fin_contagem_ano(d, names(pal_golpes))
        p <- fin_graf_empilhado(
          d, pal_golpes, "Com que golpe os KO/TKO acontecem", usar_pct
        )
        if (usar_pct) {
          p <- p + labs(y = "% dos KO/TKO")
        }
        
      } else if (input$visao == "submissoes") {
        d <- df %>%
          filter(metodo == "Finalização") %>%
          mutate(grupo = coalesce(familia, "Sem detalhe"))
        validate(need(nrow(d) > 0, msg_vazio))
        d <- fin_contagem_ano(d, names(pal_subs))
        p <- fin_graf_empilhado(
          d, pal_subs, "Que tipo de finalização domina o solo", usar_pct
        )
        if (usar_pct) {
          p <- p + labs(y = "% das finalizações")
        }
        
      } else if (input$visao == "calor") {
        com_tecnica <- df %>% filter(!is.na(tecnica))
        validate(need(nrow(com_tecnica) > 0, msg_vazio))
        top <- com_tecnica %>% count(tecnica, sort = TRUE) %>% slice_head(n = 12) %>% pull(tecnica)
        total_ano <- com_tecnica %>% count(ano, name = "total_ano")
        d <- com_tecnica %>%
          filter(tecnica %in% top) %>%
          count(ano, tecnica, name = "n") %>%
          complete(ano = total_ano$ano, tecnica = top, fill = list(n = 0L)) %>%
          left_join(total_ano, by = "ano") %>%
          mutate(
            pct = 100 * n / total_ano,
            tecnica = factor(tecnica, levels = rev(top)),
            texto = paste0(
              "<b>", ano, " - ", tecnica, "</b><br>", n, " lutas (",
              formatC(pct, format = "f", digits = 1, decimal.mark = ","),
              "% das finalizações do ano com técnica registrada)"
            )
          )
        p <- ggplot(d, aes(x = ano, y = tecnica, fill = pct, text = texto)) +
          geom_tile(color = "white", linewidth = 0.4) +
          scale_fill_gradient(low = "#FBEAEA", high = COR_UFC_VERMELHO) +
          scale_x_continuous(breaks = function(x) {
            seq(ceiling(x[1]), floor(x[2]), by = ifelse(diff(x) > 12, 2, 1))
          }) +
          labs(title = "Técnicas mais frequentes (% do ano)", x = "Ano", y = NULL,
               fill = "% do ano") +
          tema_ufc
        
      } else {
        d <- df %>%
          filter(metodo %in% c("KO/TKO", "Finalização"), !is.na(tempo_seg)) %>%
          group_by(ano, metodo) %>%
          summarise(tempo_min = mean(tempo_seg) / 60, n = n(), .groups = "drop") %>%
          mutate(texto = paste0(
            "<b>", ano, " - ", metodo, "</b><br>Tempo médio: ",
            fin_mmss(tempo_min * 60), " (", n, " lutas)"
          ))
        validate(need(nrow(d) > 0, msg_vazio))
        p <- ggplot(d, aes(x = ano, y = tempo_min, color = metodo, group = metodo, text = texto)) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2) +
          scale_color_manual(values = pal_metodo) +
          scale_x_continuous(breaks = function(x) {
            seq(ceiling(x[1]), floor(x[2]), by = ifelse(diff(x) > 12, 2, 1))
          }) +
          labs(title = "Em quanto tempo a luta termina (média, em minutos)",
               x = "Ano", y = "Minutos de luta", color = NULL) +
          tema_ufc
      }
      
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
    })
    
    # Top 10 técnicas
    output$grafico_tecnicas <- renderPlotly({
      df <- dados()
      d <- df %>%
        filter(!is.na(tecnica)) %>%
        count(tecnica, metodo, name = "n")
      validate(need(nrow(d) > 0, msg_vazio))
      total <- sum(d$n)
      d <- d %>%
        arrange(desc(n)) %>%
        slice_head(n = 10) %>%
        mutate(
          texto = paste0(
            "<b>", tecnica, "</b><br>", n, " lutas (",
            formatC(100 * n / total, format = "f", digits = 1, decimal.mark = ","),
            "% das finalizações com técnica registrada)"
          ),
          tecnica = factor(tecnica, levels = rev(tecnica))
        )
      p <- ggplot(d, aes(x = tecnica, y = n, fill = metodo, text = texto)) +
        geom_col(width = 0.75) +
        coord_flip() +
        scale_fill_manual(values = pal_metodo) +
        labs(title = NULL, x = NULL, y = "Quantidade de lutas", fill = NULL) +
        tema_ufc
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
    })
    
    # Round da finalização
    output$grafico_round <- renderPlotly({
      df <- dados()
      d <- df %>%
        filter(metodo %in% c("KO/TKO", "Finalização"), !is.na(finish_round)) %>%
        mutate(rodada = factor(paste0(as.integer(finish_round), "º"),
                               levels = paste0(1:5, "º")))
      validate(need(nrow(d) > 0, msg_vazio))
      d <- d %>%
        count(metodo, rodada, name = "n") %>%
        complete(metodo, rodada, fill = list(n = 0L)) %>%
        group_by(metodo) %>%
        mutate(pct = 100 * n / sum(n)) %>%
        ungroup() %>%
        mutate(texto = paste0(
          "<b>", metodo, " - ", rodada, " round</b><br>", n, " lutas (",
          formatC(pct, format = "f", digits = 1, decimal.mark = ","), "%)"
        ))
      p <- ggplot(d, aes(x = rodada, y = pct, fill = metodo, text = texto)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.75) +
        scale_fill_manual(values = pal_metodo) +
        scale_y_continuous(labels = function(v) paste0(v, "%")) +
        labs(title = NULL, x = "Round do fim da luta", y = "% das finalizações do tipo",
             fill = NULL) +
        tema_ufc
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25))
    })
    
    # Leitura rápida
    output$insights <- renderUI({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      n <- nrow(df)
      n_ko <- sum(df$metodo == "KO/TKO")
      n_sub <- sum(df$metodo == "Finalização")
      n_dec <- sum(df$metodo == "Decisão")
      itens <- list()
      
      # 1. Mix geral
      itens[[length(itens) + 1]] <- sprintf(
        "Das <b>%s</b> lutas do filtro, <b>%s</b> terminaram em KO/TKO, <b>%s</b> em finalização e <b>%s</b> foram à decisão.",
        fin_int(n), fin_pct(n_ko / n), fin_pct(n_sub / n), fin_pct(n_dec / n)
      )
      
      # 2. Tendência e picos (anos com pelo menos 30 lutas)
      anual <- df %>%
        group_by(ano) %>%
        summarise(
          n = n(),
          fin = mean(metodo %in% c("KO/TKO", "Finalização")),
          ko = mean(metodo == "KO/TKO"),
          sub = mean(metodo == "Finalização"),
          .groups = "drop"
        ) %>%
        filter(n >= 30) %>%
        arrange(ano)
      # o último ano da base é parcial (vai até março/2026): fora da comparação
      if (sum(anual$ano < max(lutas$ano)) >= 2) {
        anual <- anual %>% filter(ano < max(lutas$ano))
        a <- anual[1, ]
        z <- anual[nrow(anual), ]
        dif <- sub("\\.", ",", sprintf("%+.1f", 100 * (z$fin - a$fin)))
        itens[[length(itens) + 1]] <- sprintf(
          "Considerando só anos completos, a taxa de finalização foi de <b>%s</b> em %d e de <b>%s</b> em %d (%s p.p.).",
          fin_pct(a$fin), a$ano, fin_pct(z$fin), z$ano, dif
        )
        itens[[length(itens) + 1]] <- sprintf(
          "Maior taxa de KO/TKO: <b>%d</b> (%s). Maior taxa de finalização por submissão: <b>%d</b> (%s).",
          anual$ano[which.max(anual$ko)], fin_pct(max(anual$ko)),
          anual$ano[which.max(anual$sub)], fin_pct(max(anual$sub))
        )
      }
      
      # 3. Golpes
      golpes <- df %>%
        filter(metodo == "KO/TKO", !is.na(familia)) %>%
        count(familia, sort = TRUE) %>%
        mutate(p = n / sum(n))
      if (nrow(golpes) >= 2) {
        resto <- head(golpes[-1, ], 2)
        itens[[length(itens) + 1]] <- sprintf(
          "Entre os KO/TKO com golpe registrado, <b>%s</b> lideram com <b>%s</b>, seguidos por %s.",
          golpes$familia[1], fin_pct(golpes$p[1]),
          paste0("<b>", resto$familia, "</b> (", fin_pct(resto$p), ")", collapse = " e ")
        )
      }
      
      # 4. Submissões
      tecnicas <- df %>%
        filter(metodo == "Finalização", !is.na(tecnica)) %>%
        count(tecnica, sort = TRUE) %>%
        mutate(p = n / sum(n))
      estrang <- df %>% filter(metodo == "Finalização", !is.na(familia))
      if (nrow(tecnicas) >= 2) {
        itens[[length(itens) + 1]] <- sprintf(
          "Entre as finalizações com técnica registrada, <b>%s</b> lidera com <b>%s</b>, seguida de <b>%s</b> (%s). Estrangulamentos somam <b>%s</b> do total.",
          tecnicas$tecnica[1], fin_pct(tecnicas$p[1]),
          tecnicas$tecnica[2], fin_pct(tecnicas$p[2]),
          fin_pct(mean(estrang$familia == "Estrangulamentos"))
        )
      }
      
      # 5. Tempo e round
      t_ko <- mean(df$tempo_seg[df$metodo == "KO/TKO"], na.rm = TRUE)
      t_sub <- mean(df$tempo_seg[df$metodo == "Finalização"], na.rm = TRUE)
      r1 <- mean(df$finish_round[df$metodo == "KO/TKO"] == 1, na.rm = TRUE)
      if (is.finite(t_ko) && is.finite(t_sub) && is.finite(r1)) {
        itens[[length(itens) + 1]] <- sprintf(
          "O KO/TKO acontece, em média, aos <b>%s</b> de luta e a finalização aos <b>%s</b>; <b>%s</b> dos KO/TKO saem já no 1º round.",
          fin_mmss(t_ko), fin_mmss(t_sub), fin_pct(r1)
        )
      }
      
      tags$ul(lapply(itens, function(x) tags$li(HTML(x))))
    })
    
    # Tabelas
    output$tabela_anual <- renderDT({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      df_tab <- df %>%
        group_by(ano) %>%
        summarise(
          lutas = n(),
          ko = sum(metodo == "KO/TKO"),
          sub = sum(metodo == "Finalização"),
          dec = sum(metodo == "Decisão"),
          pct_fin = round(100 * (ko + sub) / lutas, 1),
          pct_ko = round(100 * ko / lutas, 1),
          pct_sub = round(100 * sub / lutas, 1),
          golpe = fin_moda(familia[metodo == "KO/TKO"]),
          tecnica = fin_moda(tecnica[metodo == "Finalização"]),
          tempo_ko = fin_media_min(tempo_seg[metodo == "KO/TKO"]),
          tempo_sub = fin_media_min(tempo_seg[metodo == "Finalização"]),
          sem_detalhe = sum(metodo %in% c("KO/TKO", "Finalização") & is.na(detalhe)),
          .groups = "drop"
        ) %>%
        arrange(desc(ano)) %>%
        transmute(
          Ano                      = ano,
          Lutas                    = lutas,
          `KO/TKO`                 = ko,
          `Finalizações`           = sub,
          `Decisões`               = dec,
          `Finalização total (%)`  = pct_fin,
          `KO/TKO (%)`             = pct_ko,
          `Submissão (%)`          = pct_sub,
          `Golpe predominante`     = coalesce(golpe, "-"),
          `Finalização predominante` = coalesce(tecnica, "-"),
          `Tempo médio KO/TKO (min)` = tempo_ko,
          `Tempo médio Sub. (min)`  = tempo_sub,
          `Sem detalhe`            = sem_detalhe
        )
      datatable(df_tab, options = opcoes_dt_padrao, rownames = FALSE)
    })
    
    output$tabela_tecnicas <- renderDT({
      df <- dados() %>% filter(!is.na(tecnica))
      validate(need(nrow(df) > 0, msg_vazio))
      largo <- df %>%
        count(tecnica, metodo, ano, name = "n") %>%
        pivot_wider(names_from = ano, values_from = n, values_fill = 0L)
      anos_cols <- setdiff(names(largo), c("tecnica", "metodo"))
      anos_cols <- anos_cols[order(as.numeric(anos_cols))]
      largo <- largo[, c("tecnica", "metodo", anos_cols)]
      largo$Total <- rowSums(largo[, anos_cols, drop = FALSE])
      largo <- largo %>%
        arrange(desc(Total)) %>%
        rename(`Técnica` = tecnica, Tipo = metodo)
      datatable(largo, options = opcoes_dt_padrao, rownames = FALSE)
    })
    
  })
}