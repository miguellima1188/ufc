# ============================================================================
# ABA 9 CINTURÃO: HISTÓRICO DE DISPUTAS, REINADOS E MAIORES DISPUTADORES
# ----------------------------------------------------------------------------
# Funções auxiliares começam com "cin_" para não dar conflito com as outras abaa
# ============================================================================

CIN_GIF_ARQUIVO <- file.path("www", "cinturao_corrida.gif")

CIN_TEM_GGANIMATE <- requireNamespace("gganimate", quietly = TRUE) &&
  requireNamespace("gifski", quietly = TRUE)

# Ordem das categorias: do mais leve ao mais pesado (masculino, depois feminino)
CIN_ORDEM_CATEGORIAS <- c(
  "Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight",
  "Middleweight", "Light Heavyweight", "Heavyweight",
  "Women's Strawweight", "Women's Flyweight", "Women's Bantamweight",
  "Women's Featherweight"
)

# Limpeza específica das disputas de cinturão 
# A coluna title_bout da base tem ruído. Corrigi aqui o que se segue:
#     Disputa de cinturão tem 5 rounds; lutas marcadas como title_bout com 3 ou 4
#     rounds saem. Exceção é a data 11/07/2020, em que as
#     duas disputas reais estão com no_of_rounds = 3 na base.
#     E lutas marcadas como title_bout com 5 rounds mas que não valem cinturão
#     Lista abaixo, identificada por data + lutador
CIN_DATAS_5R_CORRIGIDAS <- as.Date("2020-07-11")

CIN_EXCLUIR <- tibble::tibble(
  date = as.Date(c(
    "2026-01-31", "2026-01-31", "2026-01-31", "2025-09-13", "2025-08-16",
    "2023-02-04", "2023-02-04", "2023-02-04", "2023-02-04"
  )),
  r_fighter = c(
    "Sangwook Kim", "Keiichiro Nakamura", "Sulangrangbo", "Rodrigo Sezinando",
    "Alibi Idiris", "Jeka Saragih", "JeongYeong Lee", "Toshiomi Kazama",
    "SeungGuk Choi"
  )
)

# Funções auxiliares

# Percentual formatado em PT-BR
cin_pct <- function(x, d = 1) {
  paste0(format(round(100 * x, d), decimal.mark = ",", nsmall = d, trim = TRUE), "%")
}

# Inteiro com separador de milhar em PT-BR
cin_int <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

# Data em dd/mm/aaaa
cin_data <- function(x) {
  format(x, "%d/%m/%Y")
}

# Base de análise: uma linha por disputa de cinturão válida
#   vencedor / perdedor : NA quando a luta terminou sem resultado (NC)
#   metodo              : KO/TKO | Finalização | Decisão | Outros / NC
cin_preparar_base <- function() {
  ufc_lutas_completo %>%
    filter(
      title_bout == TRUE,
      no_of_rounds == 5 | date %in% CIN_DATAS_5R_CORRIGIDAS
    ) %>%
    anti_join(CIN_EXCLUIR, by = c("date", "r_fighter")) %>%
    transmute(
      date,
      ano = as.integer(format(date, "%Y")),
      weight_class,
      gender,
      r_fighter,
      b_fighter,
      vencedor = case_when(
        winner == "Red" ~ r_fighter,
        winner == "Blue" ~ b_fighter,
        TRUE ~ NA_character_
      ),
      perdedor = case_when(
        winner == "Red" ~ b_fighter,
        winner == "Blue" ~ r_fighter,
        TRUE ~ NA_character_
      ),
      metodo = case_when(
        finish == "KO/TKO" ~ "KO/TKO",
        finish == "SUB" ~ "Finalização",
        finish %in% c("U-DEC", "S-DEC", "M-DEC") ~ "Decisão",
        TRUE ~ "Outros / NC"
      ),
      finish_round,
      tempo_seg = total_fight_time_secs,
      location,
      country
    ) %>%
    arrange(date, weight_class)
}

# Formato longo: uma linha por lutador em cada disputa
cin_formato_longo <- function(lutas) {
  bind_rows(
    lutas %>% mutate(lutador = r_fighter, oponente = b_fighter),
    lutas %>% mutate(lutador = b_fighter, oponente = r_fighter)
  ) %>%
    mutate(
      resultado = case_when(
        is.na(vencedor) ~ "NC",
        vencedor == lutador ~ "Vitória",
        TRUE ~ "Derrota"
      )
    ) %>%
    arrange(date)
}

# Resumo por lutador (maiores disputadores)
cin_disputadores <- function(lutas) {
  cin_formato_longo(lutas) %>%
    group_by(lutador) %>%
    summarise(
      disputas = n(),
      vitorias = sum(resultado == "Vitória"),
      derrotas = sum(resultado == "Derrota"),
      nc = sum(resultado == "NC"),
      finalizou = sum(resultado == "Vitória" & metodo %in% c("KO/TKO", "Finalização")),
      categorias = paste(unique(weight_class), collapse = " / "),
      primeira = min(date),
      ultima = max(date),
      .groups = "drop"
    ) %>%
    mutate(aproveitamento = ifelse(disputas - nc > 0, vitorias / (disputas - nc), NA_real_)) %>%
    arrange(desc(disputas), desc(vitorias), lutador) %>%
    select(lutador, disputas, vitorias, derrotas, nc, aproveitamento,
           finalizou, categorias, primeira, ultima)
}

# Reinados: sequência de disputas vencidas pelo mesmo lutador na mesma categoria.
#    o reinado começa na primeira vitória e termina quando outro lutador vence
#    uma disputa da categoria
#    sem sucessor na base, o reinado é medido só até a última defesa registrada
#    disputas sem resultado (NC) não encerram o reinado;
#    títulos interinos não são distinguidos na base.
#    reinados anteriores a 2010 são contados a partir da primeira disputa registrada.
cin_reinados <- function(lutas) {
  lutas %>%
    filter(!is.na(vencedor)) %>%
    arrange(weight_class, date) %>%
    group_by(weight_class) %>%
    mutate(reinado_id = cumsum(vencedor != lag(vencedor, default = ""))) %>%
    group_by(weight_class, gender, reinado_id, vencedor) %>%
    summarise(
      inicio = min(date),
      ultima_luta = max(date),
      lutas = n(),
      .groups = "drop"
    ) %>%
    arrange(weight_class, inicio) %>%
    group_by(weight_class) %>%
    mutate(
      sucessor = lead(vencedor),
      fim = dplyr::coalesce(lead(inicio), ultima_luta)
    ) %>%
    ungroup() %>%
    transmute(
      lutador = vencedor,
      categoria = weight_class,
      gender,
      inicio,
      fim,
      dias = as.integer(fim - inicio),
      defesas = lutas - 1L,
      termino = if_else(
        is.na(sucessor),
        "Sem sucessor na base",
        paste0("Sucedido por ", sucessor)
      )
    ) %>%
    arrange(desc(dias))
}

# Campeões por categoria (rotatividade)
cin_rotatividade <- function(rein) {
  rein %>%
    group_by(categoria, gender) %>%
    summarise(
      campeoes = n_distinct(lutador),
      reinados = n(),
      defesas_medias = mean(defesas),
      .groups = "drop"
    ) %>%
    arrange(desc(campeoes))
}

# Dados da corrida: disputas acumuladas por lutador, ano a ano (top N por ano)
cin_corrida_dados <- function(lutas, top_n = 10) {
  longo <- cin_formato_longo(lutas)
  anos <- seq(min(longo$ano), max(longo$ano))
  genero <- longo %>%
    distinct(lutador, .keep_all = TRUE) %>%
    transmute(lutador, genero = if_else(gender == "FEMALE", "Feminino", "Masculino", "Masculino"))
  
  longo %>%
    count(lutador, ano, name = "n") %>%
    complete(lutador, ano = anos, fill = list(n = 0L)) %>%
    arrange(lutador, ano) %>%
    group_by(lutador) %>%
    mutate(acum = cumsum(n)) %>%
    ungroup() %>%
    filter(acum > 0) %>%
    arrange(ano, desc(acum), lutador) %>%
    group_by(ano) %>%
    mutate(posicao = row_number()) %>%
    ungroup() %>%
    filter(posicao <= top_n) %>%
    left_join(genero, by = "lutador")
}

# Gráficos

cin_pal_metodo <- function() {
  c(
    "KO/TKO"      = COR_UFC_VERMELHO,
    "Finalização" = COR_UFC_AZUL,
    "Decisão"     = COR_UFC_CINZA,
    "Outros / NC" = COR_UFC_DOURADO
  )
}

cin_pal_genero <- function() c("Masculino" = COR_UFC_VERMELHO, "Feminino" = COR_UFC_DOURADO)

# Linha do tempo dos reinados
cin_graf_timeline <- function(r) {
  niveis <- rev(intersect(CIN_ORDEM_CATEGORIAS, unique(r$categoria)))
  niveis <- c(setdiff(unique(r$categoria), niveis), niveis)
  r <- r %>%
    mutate(
      categoria = factor(categoria, levels = niveis),
      sobrenome = stringr::word(lutador, -1),
      texto = paste0(
        "<b>", lutador, "</b> - ", categoria, "<br>",
        cin_data(inicio), " a ", cin_data(fim), " (", cin_int(dias), " dias)<br>",
        defesas, ifelse(defesas == 1, " defesa", " defesas"), "<br>", termino
      )
    )
  
  ggplot(r, aes(y = categoria)) +
    geom_segment(
      aes(x = inicio, xend = fim, yend = categoria, color = defesas, text = texto),
      linewidth = 7, lineend = "butt"
    ) +
    geom_text(
      data = r %>% filter(dias >= 450),
      aes(x = inicio + dias / 2, label = sobrenome, text = texto),
      size = 2.7, color = "white"
    ) +
    scale_color_gradient(low = COR_UFC_CINZA, high = COR_UFC_VERMELHO,
                         name = "Defesas no reinado") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Quem foi campeão e por quanto tempo", x = NULL, y = NULL) +
    tema_ufc
}

# Maiores disputadores: barras empilhadas (vitórias, derrotas, NC)
cin_graf_disputadores <- function(disp, top_n) {
  top <- disp %>% slice_head(n = top_n)
  d <- top %>%
    pivot_longer(c(vitorias, derrotas, nc), names_to = "resultado", values_to = "n") %>%
    filter(n > 0) %>%
    mutate(
      resultado = factor(resultado, levels = c("vitorias", "derrotas", "nc"),
                         labels = c("Vitórias", "Derrotas", "Sem resultado (NC)")),
      lutador = factor(lutador, levels = rev(top$lutador)),
      texto = paste0("<b>", lutador, "</b><br>", resultado, ": ", n,
                     "<br>Categorias: ", categorias)
    )
  ggplot(d, aes(x = lutador, y = n, fill = resultado, text = texto)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_fill_manual(values = c("Vitórias" = COR_UFC_VERMELHO,
                                 "Derrotas" = COR_UFC_ESCURO,
                                 "Sem resultado (NC)" = COR_UFC_CINZA)) +
    labs(title = "Quem mais disputou o cinturão", x = NULL,
         y = "Disputas de cinturão", fill = NULL) +
    tema_ufc
}

# Como as disputas terminam, ano a ano
cin_graf_anos <- function(df, usar_pct) {
  niveis <- names(cin_pal_metodo())
  anos <- seq(min(df$ano), max(df$ano))
  d <- df %>%
    count(ano, metodo, name = "n") %>%
    complete(ano = anos, metodo = niveis, fill = list(n = 0L)) %>%
    group_by(ano) %>%
    mutate(total_ano = sum(n), pct = ifelse(total_ano > 0, 100 * n / total_ano, 0)) %>%
    ungroup() %>%
    mutate(
      metodo = factor(metodo, levels = niveis),
      valor = if (usar_pct) pct else n,
      texto = paste0("<b>", ano, "</b><br>", metodo, ": ", n, " disputas (",
                     formatC(pct, format = "f", digits = 1, decimal.mark = ","), "%)")
    )
  p <- ggplot(d, aes(x = ano, y = valor, fill = metodo, text = texto)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = cin_pal_metodo(), drop = FALSE) +
    scale_x_continuous(breaks = function(x) {
      seq(ceiling(x[1]), floor(x[2]), by = ifelse(diff(x) > 12, 2, 1))
    }) +
    labs(title = "Como as disputas de cinturão terminam", x = "Ano", fill = NULL,
         y = if (usar_pct) "% das disputas" else "Disputas de cinturão") +
    tema_ufc
  if (usar_pct) {
    p <- p + scale_y_continuous(labels = function(v) paste0(v, "%"))
  }
  p
}

# Reinados mais longos
cin_graf_reinados_longos <- function(rein) {
  d <- rein %>%
    arrange(desc(dias)) %>%
    slice_head(n = 10) %>%
    mutate(
      rotulo = paste0(lutador, " · ", format(inicio, "%Y")),
      texto = paste0("<b>", lutador, "</b> - ", categoria, "<br>",
                     cin_data(inicio), " a ", cin_data(fim), "<br>",
                     cin_int(dias), " dias · ", defesas,
                     ifelse(defesas == 1, " defesa", " defesas"))
    )
  d$rotulo <- factor(d$rotulo, levels = rev(unique(d$rotulo)))
  ggplot(d, aes(x = rotulo, y = dias, text = texto)) +
    geom_col(fill = COR_UFC_VERMELHO, width = 0.75) +
    coord_flip() +
    labs(title = "10 reinados mais longos", x = NULL, y = "Dias como campeão") +
    tema_ufc
}

# Rotatividade de campeões por categoria
cin_graf_rotatividade <- function(rot) {
  niveis <- intersect(CIN_ORDEM_CATEGORIAS, rot$categoria)
  d <- rot %>%
    mutate(
      categoria = factor(categoria, levels = niveis),
      texto = paste0("<b>", categoria, "</b><br>", campeoes, " campeões em ", reinados,
                     " reinados<br>", formatC(defesas_medias, format = "f", digits = 1,
                                              decimal.mark = ","), " defesas por reinado")
    )
  ggplot(d, aes(x = categoria, y = campeoes, fill = defesas_medias, text = texto)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_fill_gradient(low = COR_UFC_CINZA, high = COR_UFC_VERMELHO,
                        name = "Defesas por reinado") +
    labs(title = "Campeões diferentes por categoria", x = NULL, y = "Campeões distintos") +
    tema_ufc
}

# Corrida em uma data
cin_graf_corrida_estatica <- function(d, ano_ref) {
  d <- d %>%
    mutate(
      lutador = factor(lutador, levels = rev(lutador)),
      texto = paste0("<b>", lutador, "</b><br>", acum, " disputas · ", vit, " vitórias")
    )
  ggplot(d, aes(x = lutador, y = acum, fill = genero, text = texto)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_fill_manual(values = cin_pal_genero(), drop = FALSE) +
    labs(title = paste0("Disputas de cinturão acumuladas até ", ano_ref),
         x = NULL, y = NULL, fill = NULL) +
    tema_ufc
}

# Corrida animada: gráfico de barras que se reordena ano a ano
cin_graf_corrida_anim <- function(d) {
  ggplot(d, aes(x = posicao, y = acum, group = lutador, fill = genero)) +
    geom_col(width = 0.78) +
    geom_text(aes(label = paste0(lutador, "  ", acum)),
              hjust = -0.08, size = 4.2, fontface = "bold", color = COR_UFC_ESCURO) +
    coord_flip(clip = "off") +
    scale_x_reverse() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.32))) +
    scale_fill_manual(values = cin_pal_genero(), drop = FALSE) +
    labs(title = "Disputas de cinturão acumuladas: {closest_state}",
         subtitle = "Os maiores disputadores do UFC, ano a ano",
         x = NULL, y = "Disputas de cinturão acumuladas", fill = NULL) +
    tema_ufc +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(10, 30, 10, 10)
    ) +
    gganimate::transition_states(ano, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out") +
    gganimate::enter_fade() +
    gganimate::exit_fade()
}

# Gera o GIF em `arquivo`
cin_gerar_gif <- function(lutas, arquivo, top_n = 10, quadros_por_ano = 12, fps = 20) {
  if (!CIN_TEM_GGANIMATE) {
    stop("Instale os pacotes gganimate e gifski: install.packages(c('gganimate', 'gifski'))")
  }
  d <- cin_corrida_dados(lutas, top_n)
  n_anos <- length(unique(d$ano))
  if (n_anos < 2) stop("São necessários pelo menos 2 anos de dados para animar.")
  anim <- cin_graf_corrida_anim(d)
  gganimate::animate(
    anim,
    nframes = quadros_por_ano * n_anos,
    fps = fps,
    end_pause = 20,
    width = 900, height = 520, res = 110,
    renderer = gganimate::gifski_renderer(arquivo)
  )
  invisible(arquivo)
}

# UI 

aba_09_ui <- function(id) {
  ns <- NS(id)
  
  lutas <- cin_preparar_base()
  min_ano <- min(lutas$ano, na.rm = TRUE)
  max_ano <- max(lutas$ano, na.rm = TRUE)
  categorias <- union(
    intersect(CIN_ORDEM_CATEGORIAS, unique(lutas$weight_class)),
    sort(unique(lutas$weight_class))
  )
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros - Cinturão",
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
          "Linha do tempo dos reinados" = "timeline",
          "Maiores disputadores"        = "disputadores",
          "Como as disputas terminam"   = "anos"
        ),
        selected = "timeline"
      ),
      conditionalPanel(
        condition = "input.visao == 'anos'",
        ns = ns,
        radioButtons(
          inputId = ns("escala"),
          label = "Escala:",
          choices = c("Percentual (%)" = "pct", "Quantidade" = "n"),
          selected = "pct"
        )
      ),
      sliderInput(
        inputId = ns("top_n"),
        label = "Quantos lutadores mostrar (disputadores e corrida):",
        min = 5, max = 20, value = 10, step = 1
      ),
      hr(),
      helpText(
        paste0("Cobertura: ", min_ano, " a ", max_ano, ". O último ano está incompleto.")
      ),
      helpText(
        "Notas: a coluna title_bout da base foi filtrada (só disputas de 5 rounds e ",
        "sem os falsos positivos conhecidos). A base não separa título interino; ",
        "reinados sem sucessor são medidos até a última defesa registrada; ",
        "reinados anteriores a 2010 contam a partir da primeira disputa da base."
      )
    ),
    
    # KPIs
    layout_columns(
      col_widths = c(2, 2, 3, 3, 2),
      value_box(title = "Disputas de cinturão",
                value = textOutput(ns("kpi_disputas")),
                textOutput(ns("kpi_disputas_sub")),
                theme = "danger"),
      value_box(title = "Campeões diferentes",
                value = textOutput(ns("kpi_campeoes")),
                textOutput(ns("kpi_campeoes_sub")),
                theme = "dark"),
      value_box(title = "Maior disputador",
                value = textOutput(ns("kpi_disputador")),
                textOutput(ns("kpi_disputador_sub")),
                theme = "danger"),
      value_box(title = "Reinado mais longo",
                value = textOutput(ns("kpi_reinado")),
                textOutput(ns("kpi_reinado_sub")),
                theme = "dark"),
      value_box(title = "Terminam antes do fim",
                value = textOutput(ns("kpi_finaliza")),
                textOutput(ns("kpi_finaliza_sub")),
                theme = "secondary")
    ),
    
    card(
      full_screen = TRUE,
      card_header("Histórico do cinturão"),
      plotlyOutput(ns("grafico"), height = "460px")
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Reinados mais longos"),
        plotlyOutput(ns("grafico_reinados"), height = "380px")
      ),
      card(
        full_screen = TRUE,
        card_header("Rotatividade de campeões por categoria"),
        plotlyOutput(ns("grafico_rotatividade"), height = "380px")
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("Corrida dos disputadores ao longo do tempo"),
      radioButtons(
        inputId = ns("formato_corrida"),
        label = NULL,
        choices = c("Gráfico por ano (interativo)" = "estatico",
                    "GIF animado (gganimate)"      = "gif"),
        selected = "estatico",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.formato_corrida == 'estatico'",
        ns = ns,
        sliderInput(
          inputId = ns("ano_corrida"),
          label = "Disputas acumuladas até o ano (use o play para animar):",
          min = min_ano, max = max_ano, value = max_ano, step = 1, sep = "",
          width = "100%",
          animate = animationOptions(interval = 1300, loop = FALSE)
        ),
        plotlyOutput(ns("grafico_corrida"), height = "400px")
      ),
      conditionalPanel(
        condition = "input.formato_corrida == 'gif'",
        ns = ns,
        div(
          class = "d-flex align-items-center gap-3 mb-2",
          actionButton(ns("gerar_gif"), "Gerar GIF com os filtros atuais",
                       class = "btn-danger"),
          helpText(
            "Sem clicar, mostra o GIF pré-gerado (todas as categorias e todo o ",
            "período). Gerar um novo leva cerca de 1 minuto."
          )
        ),
        uiOutput(ns("gif_painel"))
      )
    ),
    
    card(
      card_header("Leitura rápida dos dados (segue os filtros)"),
      uiOutput(ns("insights"))
    ),
    
    navset_card_tab(
      title = "Tabelas",
      nav_panel("Maiores disputadores", DTOutput(ns("tabela_disputadores"))),
      nav_panel("Reinados", DTOutput(ns("tabela_reinados"))),
      nav_panel("Todas as disputas", DTOutput(ns("tabela_disputas")))
    )
  )
}

# Server

aba_09_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    lutas <- cin_preparar_base()
    # reinados dependem do histórico completo da categoria, então saem da base inteira
    reinados_todos <- cin_reinados(lutas)
    
    msg_vazio <- "Nenhuma disputa de cinturão para a combinação de filtros selecionada."
    
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
    
    reinados <- reactive({
      req(input$periodo, input$genero, input$categoria)
      # (nomes diferentes das colunas inicio/fim, senão o filter() usa a coluna)
      periodo_ini <- as.Date(paste0(input$periodo[1], "-01-01"))
      periodo_fim <- as.Date(paste0(input$periodo[2], "-12-31"))
      r <- reinados_todos %>%
        filter(inicio <= periodo_fim, fim >= periodo_ini)
      if (input$genero != "todos") {
        r <- r %>% filter(gender == input$genero)
      }
      if (input$categoria != "todas") {
        r <- r %>% filter(categoria == input$categoria)
      }
      r
    })
    
    disputadores <- reactive(cin_disputadores(dados()))
    
    # KPIs
    kpis <- reactive({
      df <- dados()
      vazio <- list(
        disputas = "-", disputas_sub = "", campeoes = "-", campeoes_sub = "",
        disputador = "-", disputador_sub = "", reinado = "-", reinado_sub = "",
        finaliza = "-", finaliza_sub = ""
      )
      if (nrow(df) == 0) return(vazio)
      
      rein <- reinados()
      disp <- disputadores()
      n <- nrow(df)
      n_fim <- sum(df$metodo %in% c("KO/TKO", "Finalização"))
      
      out <- vazio
      out$disputas <- cin_int(n)
      out$disputas_sub <- paste0(input$periodo[1], " a ", input$periodo[2])
      out$disputador <- disp$lutador[1]
      out$disputador_sub <- paste0(
        disp$disputas[1], " disputas (", disp$vitorias[1], "V-", disp$derrotas[1], "D)"
      )
      out$finaliza <- cin_pct(n_fim / n)
      out$finaliza_sub <- paste0(cin_int(n_fim), " de ", cin_int(n), " por KO/TKO ou finalização")
      
      if (nrow(rein) > 0) {
        longest <- rein %>% slice_max(dias, n = 1, with_ties = FALSE)
        out$campeoes <- cin_int(n_distinct(rein$lutador))
        out$campeoes_sub <- paste0(nrow(rein), " reinados")
        out$reinado <- longest$lutador
        out$reinado_sub <- paste0(cin_int(longest$dias), " dias · ", longest$categoria)
      }
      out
    })
    
    output$kpi_disputas      <- renderText(kpis()$disputas)
    output$kpi_disputas_sub  <- renderText(kpis()$disputas_sub)
    output$kpi_campeoes      <- renderText(kpis()$campeoes)
    output$kpi_campeoes_sub  <- renderText(kpis()$campeoes_sub)
    output$kpi_disputador    <- renderText(kpis()$disputador)
    output$kpi_disputador_sub <- renderText(kpis()$disputador_sub)
    output$kpi_reinado       <- renderText(kpis()$reinado)
    output$kpi_reinado_sub   <- renderText(kpis()$reinado_sub)
    output$kpi_finaliza      <- renderText(kpis()$finaliza)
    output$kpi_finaliza_sub  <- renderText(kpis()$finaliza_sub)
    
    # Gráfico principal
    output$grafico <- renderPlotly({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      
      if (input$visao == "timeline") {
        rein <- reinados()
        validate(need(nrow(rein) > 0, msg_vazio))
        p <- cin_graf_timeline(rein)
        ggplotly(p, tooltip = "text")
        
      } else if (input$visao == "disputadores") {
        p <- cin_graf_disputadores(disputadores(), input$top_n)
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
        
      } else {
        usar_pct <- !identical(input$escala, "n")
        p <- cin_graf_anos(df, usar_pct)
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      }
    })
    
    output$grafico_reinados <- renderPlotly({
      rein <- reinados()
      validate(need(nrow(rein) > 0, msg_vazio))
      ggplotly(cin_graf_reinados_longos(rein), tooltip = "text")
    })
    
    output$grafico_rotatividade <- renderPlotly({
      rein <- reinados()
      validate(need(nrow(rein) > 0, msg_vazio))
      ggplotly(cin_graf_rotatividade(cin_rotatividade(rein)), tooltip = "text")
    })
    
    # Corrida: versão por ano (interativa)
    output$grafico_corrida <- renderPlotly({
      req(input$ano_corrida)
      df <- dados() %>% filter(ano <= input$ano_corrida)
      validate(need(nrow(df) > 0, msg_vazio))
      d <- cin_formato_longo(df) %>%
        group_by(lutador) %>%
        summarise(
          acum = n(),
          vit = sum(resultado == "Vitória"),
          gender = first(gender),
          .groups = "drop"
        ) %>%
        arrange(desc(acum), desc(vit), lutador) %>%
        slice_head(n = input$top_n) %>%
        mutate(genero = if_else(gender == "FEMALE", "Feminino", "Masculino", "Masculino"))
      p <- cin_graf_corrida_estatica(d, input$ano_corrida)
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
    })
    
    # Corrida: versão GIF (gganimate)
    gif_novo <- eventReactive(input$gerar_gif, {
      df <- dados()
      validate(
        need(CIN_TEM_GGANIMATE, "Pacotes gganimate e gifski não instalados."),
        need(length(unique(df$ano)) >= 2,
             "Escolha um período com pelo menos 2 anos de disputas para animar.")
      )
      arq <- tempfile(fileext = ".gif")
      withProgress(message = "Gerando o GIF (cerca de 1 minuto)...", value = 0.3, {
        cin_gerar_gif(df, arq, top_n = input$top_n, quadros_por_ano = 8)
      })
      arq
    })
    
    output$gif_novo <- renderImage({
      list(src = gif_novo(), contentType = "image/gif",
           alt = "Corrida animada dos maiores disputadores de cinturão", width = "100%")
    }, deleteFile = FALSE)
    
    output$gif_painel <- renderUI({
      if (!CIN_TEM_GGANIMATE) {
        div(
          class = "alert alert-warning",
          "Para ver o GIF instale os pacotes: ",
          tags$code("install.packages(c('gganimate', 'gifski'))")
        )
      } else if (input$gerar_gif > 0) {
        imageOutput(session$ns("gif_novo"), height = "auto")
      } else if (file.exists(CIN_GIF_ARQUIVO)) {
        tags$img(
          src = basename(CIN_GIF_ARQUIVO),
          style = "max-width: 100%; height: auto;",
          alt = "Corrida animada dos maiores disputadores de cinturão"
        )
      } else {
        div(
          class = "alert alert-info",
          "O GIF pré-gerado (www/cinturao_corrida.gif) não foi encontrado. ",
          "Rode scripts/03_gerar_gif_cinturao.R ou clique em ",
          "'Gerar GIF com os filtros atuais'."
        )
      }
    })
    
    # Leitura rápida
    output$insights <- renderUI({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      rein <- reinados()
      disp <- disputadores()
      itens <- character()
      
      n <- nrow(df)
      n_fim <- sum(df$metodo %in% c("KO/TKO", "Finalização"))
      n_dec <- sum(df$metodo == "Decisão")
      itens <- c(itens, paste0(
        "<b>", cin_pct(n_fim / n), "</b> das disputas terminaram antes do tempo (KO/TKO ou ",
        "finalização) e <b>", cin_pct(n_dec / n), "</b> foram para a decisão dos juízes."
      ))
      
      if (nrow(rein) > 0) {
        mais_def <- rein %>% slice_max(defesas, n = 1, with_ties = FALSE)
        if (mais_def$defesas > 0) {
          itens <- c(itens, paste0(
            "Maior sequência de defesas: <b>", mais_def$lutador, "</b>, com ",
            mais_def$defesas, " defesas seguidas em ", mais_def$categoria, " (",
            format(mais_def$inicio, "%Y"), " a ", format(mais_def$fim, "%Y"), ")."
          ))
        }
        itens <- c(itens, paste0(
          "<b>", cin_pct(mean(rein$defesas == 0)), "</b> dos reinados terminaram (ou seguem) ",
          "sem nenhuma defesa de título."
        ))
        
        rot <- cin_rotatividade(rein)
        if (nrow(rot) > 1) {
          mais_troca <- rot %>% slice_max(campeoes, n = 1, with_ties = FALSE)
          mais_estavel <- rot %>% slice_max(defesas_medias, n = 1, with_ties = FALSE)
          itens <- c(itens, paste0(
            "Categoria com mais troca de campeões: <b>", mais_troca$categoria, "</b> (",
            mais_troca$campeoes, " campeões). A mais estável: <b>", mais_estavel$categoria,
            "</b>, com ", formatC(mais_estavel$defesas_medias, format = "f", digits = 1,
                                  decimal.mark = ","), " defesas por reinado."
          ))
        }
        
        duplos <- rein %>%
          group_by(lutador) %>%
          summarise(n_cat = n_distinct(categoria), .groups = "drop") %>%
          filter(n_cat >= 2) %>%
          arrange(lutador)
        if (nrow(duplos) > 0) {
          itens <- c(itens, paste0(
            "<b>", nrow(duplos), "</b> lutadores foram campeões em mais de uma categoria: ",
            paste(duplos$lutador, collapse = ", "), "."
          ))
        }
      }
      
      sem_titulo <- disp %>% filter(vitorias == 0) %>% slice_max(disputas, n = 1, with_ties = FALSE)
      if (nrow(sem_titulo) > 0) {
        itens <- c(itens, paste0(
          "Quem mais tentou sem vencer: <b>", sem_titulo$lutador, "</b> (",
          sem_titulo$disputas, ifelse(sem_titulo$disputas == 1, " disputa", " disputas"),
          " e nenhuma vitória)."
        ))
      }
      
      tags$ul(lapply(itens, function(x) tags$li(HTML(x))))
    })
    
    # Tabelas
    output$tabela_disputadores <- renderDT({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      datatable(
        disputadores(),
        colnames = c("Lutador", "Disputas", "Vitórias", "Derrotas", "NC",
                     "Aproveitamento", "Vitórias por KO/finalização", "Categorias",
                     "Primeira disputa", "Última disputa"),
        options = opcoes_dt_padrao, rownames = FALSE
      ) %>%
        formatPercentage(6, digits = 1, dec.mark = ",", mark = ".") %>%
        formatDate(c(9, 10), method = "toLocaleDateString",
                   params = list("pt-BR", list(timeZone = "UTC")))
    })
    
    output$tabela_reinados <- renderDT({
      rein <- reinados()
      validate(need(nrow(rein) > 0, msg_vazio))
      d <- rein %>%
        arrange(categoria, inicio) %>%
        select(lutador, categoria, inicio, fim, dias, defesas, termino)
      datatable(
        d,
        colnames = c("Campeão", "Categoria", "Início", "Fim", "Dias", "Defesas", "Término"),
        options = opcoes_dt_padrao, rownames = FALSE
      ) %>%
        formatDate(c(3, 4), method = "toLocaleDateString",
                   params = list("pt-BR", list(timeZone = "UTC")))
    })
    
    output$tabela_disputas <- renderDT({
      df <- dados()
      validate(need(nrow(df) > 0, msg_vazio))
      d <- df %>%
        arrange(desc(date)) %>%
        transmute(
          date, weight_class, r_fighter, b_fighter,
          vencedor = coalesce(vencedor, "Sem resultado (NC)"),
          metodo, finish_round, location
        )
      datatable(
        d,
        colnames = c("Data", "Categoria", "Canto vermelho", "Canto azul", "Vencedor",
                     "Método", "Round final", "Local"),
        options = opcoes_dt_padrao, rownames = FALSE
      ) %>%
        formatDate(1, method = "toLocaleDateString",
                   params = list("pt-BR", list(timeZone = "UTC")))
    })
  })
}
