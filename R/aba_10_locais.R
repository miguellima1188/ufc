# ============================================================================
# ABA 10 LOCAIS & EVENTOS: ONDE O UFC REALIZA SEUS EVENTOS
# ----------------------------------------------------------------------------
# Bases: locais_mais_eventos.rds (cidade) e paises_mais_eventos.rds (país),
# carregadas pelo global.R.
# Funções auxiliares começam com "loc_" para não dar conflito com as outras abas.
#
# A base não traz latitude/longitude. As coordenadas (centro aproximado de cada
# cidade) estão em dados_auxiliares/coordenadas_cidades.csv, uma linha por
# cidade + país.
# ============================================================================

LOC_ARQ_COORDS <- file.path("dados_auxiliares", "coordenadas_cidades.csv")

# Nomes dos países em português (o que não estiver aqui aparece como na base)
LOC_PAIS_PT <- c(
  "USA" = "Estados Unidos", "Brazil" = "Brasil", "Canada" = "Canadá",
  "United Kingdom" = "Reino Unido", "Australia" = "Austrália",
  "United Arab Emirates" = "Emirados Árabes Unidos", "China" = "China",
  "Japan" = "Japão", "Mexico" = "México", "Germany" = "Alemanha",
  "Singapore" = "Singapura", "Sweden" = "Suécia", "France" = "França",
  "Ireland" = "Irlanda", "New Zealand" = "Nova Zelândia", "Russia" = "Rússia",
  "Azerbaijan" = "Azerbaijão", "Netherlands" = "Países Baixos",
  "Poland" = "Polônia", "Saudi Arabia" = "Arábia Saudita",
  "South Korea" = "Coreia do Sul", "Argentina" = "Argentina", "Chile" = "Chile",
  "Croatia" = "Croácia", "Czech Republic" = "Tchéquia", "Denmark" = "Dinamarca",
  "Philippines" = "Filipinas", "Puerto Rico" = "Porto Rico", "Qatar" = "Catar",
  "Uruguay" = "Uruguai"
)

# Região de cada país (usada no filtro de região)
LOC_REGIAO <- c(
  "USA" = "América do Norte", "Canada" = "América do Norte",
  "Mexico" = "América do Norte", "Puerto Rico" = "América do Norte",
  "Brazil" = "América do Sul", "Argentina" = "América do Sul",
  "Chile" = "América do Sul", "Uruguay" = "América do Sul",
  "United Kingdom" = "Europa", "Germany" = "Europa", "Sweden" = "Europa",
  "France" = "Europa", "Ireland" = "Europa", "Russia" = "Europa",
  "Netherlands" = "Europa", "Poland" = "Europa", "Croatia" = "Europa",
  "Czech Republic" = "Europa", "Denmark" = "Europa",
  "China" = "Ásia", "Japan" = "Ásia", "Singapore" = "Ásia",
  "South Korea" = "Ásia", "Philippines" = "Ásia", "Azerbaijan" = "Ásia",
  "United Arab Emirates" = "Oriente Médio", "Saudi Arabia" = "Oriente Médio",
  "Qatar" = "Oriente Médio",
  "Australia" = "Oceania", "New Zealand" = "Oceania"
)

# Faixas de eventos por cidade (cor do círculo no mapa)
LOC_FAIXAS_CORTES  <- c(0, 1, 3, 7, 19, Inf)
LOC_FAIXAS_ROTULOS <- c("1 evento", "2 a 3", "4 a 7", "8 a 19", "20 ou mais")
LOC_FAIXAS_CORES   <- c("#F7C6C6", "#EE8F8F", "#E04A4A", "#D20A0A", "#7A0000")

# Funções auxiliares

loc_pais_pt <- function(x) {
  r <- unname(LOC_PAIS_PT[x])
  ifelse(is.na(r), x, r)
}

loc_regiao <- function(x) {
  r <- unname(LOC_REGIAO[x])
  ifelse(is.na(r), "Outras", r)
}

# Inteiro com separador de milhar em PT-BR
loc_int <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

# Percentual formatado em PT-BR (x entre 0 e 1)
loc_pct <- function(x, d = 1) {
  paste0(format(round(100 * x, d), decimal.mark = ",", nsmall = d, trim = TRUE), "%")
}

loc_faixa <- function(n) {
  cut(n, breaks = LOC_FAIXAS_CORTES, labels = LOC_FAIXAS_ROTULOS)
}

# Raio do círculo: cresce com a raiz do nº de eventos (área ~ eventos), com teto
loc_raio <- function(n) {
  pmin(5 + 2.3 * sqrt(n), 40)
}

loc_plotly_vazio <- function(msg = "Nenhum local para a combinação de filtros selecionada.") {
  plotly_empty() %>% layout(title = list(text = msg, y = 0.5))
}

# Prepara as duas bases de análise
#   cidades: uma linha por cidade + país (variações de grafia do mesmo lugar,
#            como "Rio de Janeiro, Brazil" e "Rio de Janeiro, Rio de Janeiro,
#            Brazil", são somadas), já com latitude/longitude
#   paises : uma linha por país, com nº de cidades e cidade líder
loc_preparar <- function() {
  if (!file.exists(LOC_ARQ_COORDS)) {
    stop("Arquivo de coordenadas não encontrado: ", LOC_ARQ_COORDS)
  }
  coords <- readr::read_csv(LOC_ARQ_COORDS, show_col_types = FALSE)

  cidades <- locais_mais_eventos %>%
    group_by(cidade, pais) %>%
    summarise(
      local_principal = location[which.max(total_eventos)],
      total_eventos = sum(total_eventos),
      .groups = "drop"
    ) %>%
    left_join(coords, by = c("cidade", "pais")) %>%
    arrange(desc(total_eventos), cidade) %>%
    mutate(
      cidade_id = paste(cidade, pais, sep = "|"),
      pais_pt = loc_pais_pt(pais),
      regiao = loc_regiao(pais),
      pct_total = total_eventos / sum(total_eventos),
      posicao = min_rank(desc(total_eventos)),
      faixa = loc_faixa(total_eventos),
      rotulo = paste0(cidade, ", ", pais_pt, ": ", loc_int(total_eventos), " evento(s)"),
      popup_html = paste0(
        "<strong>", cidade, "</strong><br>", local_principal,
        "<hr style='margin:4px 0'>",
        "<b>", loc_int(total_eventos), "</b> evento(s) · ",
        loc_pct(pct_total), " do total<br>",
        "Posição geral: ", posicao, "º"
      )
    )

  resumo_cidades <- cidades %>%
    group_by(pais) %>%
    summarise(
      n_cidades = n(),
      cidade_lider = cidade[which.max(total_eventos)],
      eventos_cidade_lider = max(total_eventos),
      .groups = "drop"
    )

  paises <- paises_mais_eventos %>%
    arrange(desc(total_eventos), pais) %>%
    mutate(
      pais_pt = loc_pais_pt(pais),
      regiao = loc_regiao(pais),
      pct_total = total_eventos / sum(total_eventos),
      posicao = min_rank(desc(total_eventos))
    ) %>%
    left_join(resumo_cidades, by = "pais")

  list(cidades = cidades, paises = paises)
}

# Mapa-base (camadas e controles); os marcadores entram em loc_desenhar_mapa()
loc_mapa_base <- function() {
  leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
    # Tiles sem chave de API (o CARTO passou a exigir chave)
    addTiles(
      urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
      attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ",
      options = tileOptions(maxZoom = 16),
      group = "Mapa claro"
    ) %>%
    addTiles(
      urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
      options = tileOptions(maxZoom = 16),
      group = "Mapa claro"
    ) %>%
    addTiles(
      urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
      attribution = "&copy; OpenStreetMap contributors",
      options = tileOptions(maxZoom = 19),
      group = "Ruas"
    ) %>%
    addTiles(
      urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      attribution = "Tiles &copy; Esri &mdash; Source: Esri, Maxar, Earthstar Geographics",
      options = tileOptions(maxZoom = 18),
      group = "Satélite"
    ) %>%
    addLayersControl(
      baseGroups = c("Mapa claro", "Ruas", "Satélite"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    setView(lng = 10, lat = 25, zoom = 2)
}

# Desenha os círculos e a legenda (serve para o mapa inicial e para o proxy)
loc_desenhar_mapa <- function(mapa, dados, limpar = TRUE) {
  if (limpar) {
    mapa <- mapa %>% clearMarkers() %>% clearPopups() %>% removeControl("legenda_faixas")
  }
  if (nrow(dados) == 0) return(mapa)

  # maiores primeiro: os círculos pequenos ficam por cima e continuam clicáveis
  dados <- dados %>% arrange(desc(total_eventos))
  pal <- colorFactor(LOC_FAIXAS_CORES, levels = LOC_FAIXAS_ROTULOS)

  mapa %>%
    addCircleMarkers(
      data = dados, lng = ~lon, lat = ~lat, layerId = ~cidade_id,
      radius = ~loc_raio(total_eventos),
      stroke = TRUE, color = "#FFFFFF", weight = 1,
      fillColor = ~pal(as.character(faixa)), fillOpacity = 0.85,
      label = ~rotulo, popup = ~popup_html
    ) %>%
    addLegend(
      position = "bottomright", colors = LOC_FAIXAS_CORES, labels = LOC_FAIXAS_ROTULOS,
      title = "Eventos por cidade", opacity = 0.9, layerId = "legenda_faixas"
    )
}

# Enquadra o mapa na área ocupada pelas cidades filtradas
loc_enquadrar <- function(mapa, dados) {
  if (nrow(dados) == 0) return(mapa)
  mapa %>% fitBounds(
    lng1 = min(dados$lon), lat1 = min(dados$lat),
    lng2 = max(dados$lon), lat2 = max(dados$lat),
    options = list(padding = c(40, 40), maxZoom = 9)
  )
}

# UI

aba_10_ui <- function(id) {
  ns <- NS(id)
  base <- loc_preparar()
  paises <- base$paises
  regioes <- sort(unique(paises$regiao))

  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros - Locais & Eventos",
      width = 300,
      selectInput(
        inputId = ns("regiao"),
        label = "Região:",
        choices = c("Todas as regiões" = "todas", regioes),
        selected = "todas"
      ),
      selectizeInput(
        inputId = ns("paises"),
        label = "País(es):",
        choices = setNames(paises$pais, paises$pais_pt),
        multiple = TRUE,
        options = list(placeholder = "Todos os países")
      ),
      sliderInput(
        inputId = ns("min_eventos"),
        label = "Mínimo de eventos por cidade:",
        min = 1, max = 20, value = 1, step = 1
      ),
      sliderInput(
        inputId = ns("top_n"),
        label = "Quantos mostrar nos rankings:",
        min = 5, max = 25, value = 10, step = 1
      ),
      hr(),
      checkboxInput(
        inputId = ns("zoom_auto"),
        label = "Zoom automático na área filtrada",
        value = TRUE
      ),
      div(
        class = "d-grid gap-2",
        actionButton(ns("enquadrar"), "Enquadrar seleção no mapa",
                     class = "btn-outline-secondary btn-sm"),
        actionButton(ns("mundo"), "Ver mundo inteiro",
                     class = "btn-outline-secondary btn-sm"),
        actionButton(ns("limpar"), "Limpar filtros", class = "btn-danger btn-sm")
      ),
      hr(),
      helpText(
        "Notas: as cidades que aparecem com mais de uma grafia na base ",
        "(por exemplo, \"Rio de Janeiro, Brazil\" e \"Rio de Janeiro, Rio de ",
        "Janeiro, Brazil\") foram somadas. A base não traz coordenadas: a ",
        "posição no mapa é o centro aproximado de cada cidade, cadastrado em ",
        "dados_auxiliares/coordenadas_cidades.csv. O mínimo de eventos vale ",
        "para as cidades; os rankings de países usam a contagem completa."
      )
    ),

    # KPIs
    layout_columns(
      col_widths = c(2, 2, 2, 3, 3),
      value_box(title = "Eventos na seleção",
                value = textOutput(ns("kpi_eventos")),
                textOutput(ns("kpi_eventos_sub")),
                theme = "danger"),
      value_box(title = "Cidades no mapa",
                value = textOutput(ns("kpi_cidades")),
                textOutput(ns("kpi_cidades_sub")),
                theme = "dark"),
      value_box(title = "Países",
                value = textOutput(ns("kpi_paises")),
                textOutput(ns("kpi_paises_sub")),
                theme = "secondary"),
      value_box(title = "Cidade líder",
                value = textOutput(ns("kpi_cidade_lider")),
                textOutput(ns("kpi_cidade_lider_sub")),
                theme = "danger"),
      value_box(title = "País líder",
                value = textOutput(ns("kpi_pais_lider")),
                textOutput(ns("kpi_pais_lider_sub")),
                theme = "dark")
    ),

    card(
      full_screen = TRUE,
      card_header("Mapa interativo - eventos por cidade"),
      leafletOutput(ns("mapa"), height = "540px"),
      div(
        class = "small text-muted mt-2",
        "Tamanho e cor do círculo = número de eventos. Passe o mouse para ver ",
        "a cidade e clique para os detalhes. Escolha região ou países nos ",
        "filtros e o mapa enquadra a área; na tabela de cidades, clique em uma ",
        "linha para voar até ela."
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Cidades com mais eventos"),
        plotlyOutput(ns("grafico_cidades"), height = "440px")
      ),
      card(
        full_screen = TRUE,
        card_header("Países com mais eventos"),
        checkboxInput(
          inputId = ns("log_pais"),
          label = "Escala logarítmica (reduz o peso dos países muito à frente)",
          value = FALSE
        ),
        plotlyOutput(ns("grafico_paises"), height = "400px")
      )
    ),

    navset_card_tab(
      full_screen = TRUE,
      title = "Concentração geográfica",
      nav_panel("País → cidade (treemap)",
                plotlyOutput(ns("treemap"), height = "480px")),
      nav_panel("Quanto poucas cidades concentram (Pareto)",
                plotlyOutput(ns("pareto"), height = "480px"))
    ),

    card(
      card_header("Leitura rápida dos dados (segue os filtros)"),
      uiOutput(ns("insights"))
    ),

    navset_card_tab(
      title = "Tabelas",
      nav_panel("Cidades", DTOutput(ns("tabela_cidades"))),
      nav_panel("Países", DTOutput(ns("tabela_paises")))
    )
  )
}

# Server

aba_10_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    base <- loc_preparar()
    cidades <- base$cidades
    paises <- base$paises
    total_geral <- sum(paises$total_eventos)

    # Filtros -----------------------------------------------------------------

    # trocar a região refaz a lista de países disponíveis
    observeEvent(input$regiao, {
      p <- if (input$regiao == "todas") paises else paises %>% filter(regiao == input$regiao)
      updateSelectizeInput(session, "paises",
                           choices = setNames(p$pais, p$pais_pt),
                           selected = character(0))
    }, ignoreInit = TRUE)

    observeEvent(input$limpar, {
      updateSelectInput(session, "regiao", selected = "todas")
      updateSelectizeInput(session, "paises",
                           choices = setNames(paises$pais, paises$pais_pt),
                           selected = character(0))
      updateSliderInput(session, "min_eventos", value = 1)
      updateSliderInput(session, "top_n", value = 10)
      updateCheckboxInput(session, "zoom_auto", value = TRUE)
    })

    # cidades da região/países escolhidos (sem o mínimo de eventos)
    cidades_base <- reactive({
      d <- cidades
      if (input$regiao != "todas") d <- d %>% filter(regiao == input$regiao)
      if (length(input$paises) > 0) d <- d %>% filter(pais %in% input$paises)
      d
    })

    # cidades que aparecem no mapa e nos rankings de cidades
    cidades_f <- reactive({
      cidades_base() %>% filter(total_eventos >= input$min_eventos)
    })

    paises_f <- reactive({
      d <- paises
      if (input$regiao != "todas") d <- d %>% filter(regiao == input$regiao)
      if (length(input$paises) > 0) d <- d %>% filter(pais %in% input$paises)
      d
    })

    # KPIs --------------------------------------------------------------------

    eventos_sel <- reactive(sum(paises_f()$total_eventos))

    output$kpi_eventos <- renderText(loc_int(eventos_sel()))
    output$kpi_eventos_sub <- renderText(
      paste0(loc_pct(eventos_sel() / total_geral), " dos ", loc_int(total_geral),
             " eventos da base")
    )

    output$kpi_cidades <- renderText(loc_int(nrow(cidades_f())))
    output$kpi_cidades_sub <- renderText({
      txt <- paste0("de ", loc_int(nrow(cidades)), " cidades na base")
      if (input$min_eventos > 1) txt <- paste0(txt, " · mín. ", input$min_eventos, " eventos")
      txt
    })

    output$kpi_paises <- renderText(loc_int(nrow(paises_f())))
    output$kpi_paises_sub <- renderText(paste0("de ", nrow(paises), " países na base"))

    output$kpi_cidade_lider <- renderText({
      d <- cidades_f()
      if (nrow(d) == 0) "-" else d$cidade[1]
    })
    output$kpi_cidade_lider_sub <- renderText({
      d <- cidades_f()
      if (nrow(d) == 0 || eventos_sel() == 0) return("sem dados no filtro")
      paste0(loc_int(d$total_eventos[1]), " eventos (",
             loc_pct(d$total_eventos[1] / eventos_sel()), " da seleção)")
    })

    output$kpi_pais_lider <- renderText({
      d <- paises_f()
      if (nrow(d) == 0) "-" else d$pais_pt[1]
    })
    output$kpi_pais_lider_sub <- renderText({
      d <- paises_f()
      if (nrow(d) == 0 || eventos_sel() == 0) return("sem dados no filtro")
      paste0(loc_int(d$total_eventos[1]), " eventos (",
             loc_pct(d$total_eventos[1] / eventos_sel()), " da seleção)")
    })

    # Mapa --------------------------------------------------------------------

    # primeira renderização: base + marcadores da seleção atual
    output$mapa <- renderLeaflet({
      d <- isolate(cidades_f())
      mapa <- loc_mapa_base() %>% loc_desenhar_mapa(d, limpar = FALSE)
      if (isTRUE(isolate(input$zoom_auto))) mapa <- loc_enquadrar(mapa, d)
      mapa
    })

    # mudanças de filtro atualizam só os marcadores (e o zoom, se estiver ligado)
    observe({
      d <- cidades_f()
      mapa <- leafletProxy("mapa") %>% loc_desenhar_mapa(d)
      if (isTRUE(input$zoom_auto)) loc_enquadrar(mapa, d)
    })

    observeEvent(input$enquadrar, {
      leafletProxy("mapa") %>% clearPopups() %>% loc_enquadrar(cidades_f())
    })

    observeEvent(input$mundo, {
      leafletProxy("mapa") %>% clearPopups() %>% setView(lng = 10, lat = 25, zoom = 2)
    })

    # clicar em uma linha da tabela de cidades leva o mapa até ela
    observeEvent(input$tabela_cidades_rows_selected, {
      i <- input$tabela_cidades_rows_selected
      d <- cidades_f()
      if (length(i) == 1 && i <= nrow(d)) {
        c1 <- d[i, ]
        leafletProxy("mapa") %>%
          clearPopups() %>%
          flyTo(lng = c1$lon, lat = c1$lat, zoom = 7) %>%
          addPopups(lng = c1$lon, lat = c1$lat, popup = c1$popup_html, layerId = "popup_sel")
      }
    }, ignoreNULL = TRUE)

    # Rankings ----------------------------------------------------------------

    output$grafico_cidades <- renderPlotly({
      d <- cidades_f() %>%
        slice_head(n = input$top_n) %>%
        mutate(
          rotulo_y = paste0(cidade, " (", pais_pt, ")"),
          hover = paste0("<b>", cidade, "</b><br>", local_principal, "<br>",
                         loc_int(total_eventos), " evento(s) · ",
                         loc_pct(pct_total), " do total da base")
        )
      if (nrow(d) == 0) return(loc_plotly_vazio())

      plot_ly(
        d, x = ~total_eventos, y = ~reorder(rotulo_y, total_eventos),
        type = "bar", orientation = "h",
        marker = list(color = COR_UFC_VERMELHO),
        text = ~loc_int(total_eventos), textposition = "outside", cliponaxis = FALSE,
        hovertext = ~hover, hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Eventos", rangemode = "tozero"),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(r = 30)
        ) %>%
        config(displaylogo = FALSE)
    })

    output$grafico_paises <- renderPlotly({
      d <- paises_f() %>%
        slice_head(n = input$top_n) %>%
        mutate(
          hover = paste0("<b>", pais_pt, "</b> (", regiao, ")<br>",
                         loc_int(total_eventos), " evento(s) · ",
                         loc_pct(pct_total), " do total da base<br>",
                         n_cidades, " cidade(s) · líder: ", cidade_lider)
        )
      if (nrow(d) == 0) return(loc_plotly_vazio())

      plot_ly(
        d, x = ~total_eventos, y = ~reorder(pais_pt, total_eventos),
        type = "bar", orientation = "h",
        marker = list(color = COR_UFC_ESCURO),
        text = ~loc_int(total_eventos), textposition = "outside", cliponaxis = FALSE,
        hovertext = ~hover, hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(
            title = if (isTRUE(input$log_pais)) "Eventos (escala log)" else "Eventos",
            type = if (isTRUE(input$log_pais)) "log" else "linear"
          ),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(r = 30)
        ) %>%
        config(displaylogo = FALSE)
    })

    # Concentração ------------------------------------------------------------

    output$treemap <- renderPlotly({
      d <- cidades_f()
      if (nrow(d) == 0) return(loc_plotly_vazio())

      pa <- d %>%
        group_by(pais, pais_pt) %>%
        summarise(total_eventos = sum(total_eventos), .groups = "drop")

      plot_ly(
        type = "treemap",
        ids = c(pa$pais, d$cidade_id),
        labels = c(pa$pais_pt, d$cidade),
        parents = c(rep("", nrow(pa)), d$pais),
        values = c(pa$total_eventos, d$total_eventos),
        branchvalues = "total",
        marker = list(
          colors = c(rep(COR_UFC_ESCURO, nrow(pa)), LOC_FAIXAS_CORES[as.integer(d$faixa)]),
          line = list(width = 1, color = "#FFFFFF")
        ),
        textinfo = "label+value",
        hovertemplate = paste0("<b>%{label}</b><br>%{value} evento(s)<br>",
                               "%{percentRoot:.1%} da seleção<extra></extra>")
      ) %>%
        layout(margin = list(l = 5, r = 5, t = 5, b = 5)) %>%
        config(displaylogo = FALSE)
    })

    output$pareto <- renderPlotly({
      d <- cidades_f()
      if (nrow(d) == 0) return(loc_plotly_vazio())

      d <- d %>%
        arrange(desc(total_eventos), cidade) %>%
        mutate(
          ordem = row_number(),
          acumulado = cumsum(total_eventos) / sum(total_eventos),
          hover_barra = paste0("<b>", cidade, "</b> (", pais_pt, ")<br>",
                               loc_int(total_eventos), " evento(s)"),
          hover_linha = paste0("As ", ordem, " cidades com mais eventos somam<br><b>",
                               loc_pct(acumulado), "</b> da seleção")
        )

      plot_ly(d) %>%
        add_bars(
          x = ~ordem, y = ~total_eventos, name = "Eventos por cidade",
          marker = list(color = COR_UFC_VERMELHO),
          hovertext = ~hover_barra, hoverinfo = "text"
        ) %>%
        add_lines(
          x = ~ordem, y = ~100 * acumulado, yaxis = "y2", name = "% acumulado",
          line = list(color = COR_UFC_ESCURO, width = 2),
          hovertext = ~hover_linha, hoverinfo = "text"
        ) %>%
        layout(
          xaxis = list(title = "Cidades, da que mais recebe eventos para a que menos recebe"),
          yaxis = list(title = "Eventos por cidade"),
          yaxis2 = list(title = "% acumulado dos eventos", overlaying = "y",
                        side = "right", range = c(0, 100), ticksuffix = "%",
                        showgrid = FALSE),
          legend = list(orientation = "h", y = -0.2),
          margin = list(r = 60)
        ) %>%
        config(displaylogo = FALSE)
    })

    # Leitura rápida ----------------------------------------------------------

    output$insights <- renderUI({
      cid <- cidades_f()
      cid_base <- cidades_base()
      pai <- paises_f()
      tot <- eventos_sel()

      if (nrow(cid) == 0 || nrow(pai) == 0 || tot == 0) {
        return(p("Nenhum local para a combinação de filtros selecionada."))
      }

      itens <- list()

      # 1. cidade líder e quanto ela concentra
      lider <- cid[1, ]
      txt1 <- tags$li(
        tags$b(lider$cidade), " (", lider$pais_pt, ") é a cidade com mais eventos: ",
        tags$b(loc_int(lider$total_eventos)), ", ",
        loc_pct(lider$total_eventos / tot), " dos eventos da seleção."
      )
      if (nrow(cid) >= 2) {
        txt1 <- tagAppendChildren(
          txt1,
          paste0(" A segunda colocada, ", cid$cidade[2], ", tem ",
                 loc_int(cid$total_eventos[2]), " (",
                 format(round(lider$total_eventos / cid$total_eventos[2], 1),
                        decimal.mark = ","),
                 " vezes menos).")
        )
      }
      itens <- c(itens, list(txt1))

      # 2. concentração no topo
      k <- min(10, nrow(cid))
      if (k >= 2) {
        itens <- c(itens, list(tags$li(
          "As ", tags$b(k, " cidades com mais eventos"), " concentram ",
          tags$b(loc_pct(sum(cid$total_eventos[seq_len(k)]) / tot)),
          " dos eventos da seleção."
        )))
      }

      # 3. país líder e o seguinte
      pl <- pai[1, ]
      txt3 <- tags$li(
        tags$b(pl$pais_pt), " lidera entre os países, com ",
        tags$b(loc_int(pl$total_eventos)), " eventos (",
        loc_pct(pl$total_eventos / tot), "), em ", pl$n_cidades, " cidade(s)."
      )
      if (nrow(pai) >= 2) {
        txt3 <- tagAppendChildren(
          txt3,
          paste0(" Em segundo vem ", pai$pais_pt[2], ", com ",
                 loc_int(pai$total_eventos[2]), " (",
                 loc_pct(pai$total_eventos[2] / tot), ").")
        )
      }
      itens <- c(itens, list(txt3))

      # 4. a melhor cidade fora do país líder
      fora <- cid_base %>% filter(pais != pl$pais)
      if (nrow(fora) > 0 && nrow(pai) >= 2) {
        itens <- c(itens, list(tags$li(
          "Fora de ", pl$pais_pt, ", a cidade com mais eventos é ",
          tags$b(fora$cidade[1]), " (", fora$pais_pt[1], "), com ",
          loc_int(fora$total_eventos[1]), "."
        )))
      }

      # 5. cauda longa: cidades com um único evento (ignora o filtro de mínimo)
      uni <- sum(cid_base$total_eventos == 1)
      if (nrow(cid_base) > 0) {
        itens <- c(itens, list(tags$li(
          tags$b(loc_int(uni)), " de ", loc_int(nrow(cid_base)), " cidades (",
          loc_pct(uni / nrow(cid_base), 0),
          ") receberam um único evento; o UFC visita muitos lugares uma só vez, ",
          "mas volta sempre aos mesmos poucos."
        )))
      }

      # 6. países de passagem única
      uni_p <- sum(pai$total_eventos == 1)
      if (uni_p > 0) {
        itens <- c(itens, list(tags$li(
          tags$b(uni_p), " de ", nrow(pai), " país(es) da seleção sediaram apenas um evento."
        )))
      }

      tags$ul(itens)
    })

    # Tabelas -----------------------------------------------------------------

    output$tabela_cidades <- renderDT({
      d <- cidades_f() %>%
        transmute(
          `Posição` = posicao,
          Cidade = cidade,
          `País` = pais_pt,
          `Local completo (base)` = local_principal,
          Eventos = total_eventos,
          `% do total` = 100 * pct_total
        )
      datatable(d, rownames = FALSE, selection = "single", options = opcoes_dt_padrao) %>%
        formatRound("% do total", digits = 2, dec.mark = ",", mark = ".")
    })

    output$tabela_paises <- renderDT({
      d <- paises_f() %>%
        transmute(
          `Posição` = posicao,
          `País` = pais_pt,
          `Região` = regiao,
          Eventos = total_eventos,
          `% do total` = 100 * pct_total,
          `Cidades distintas` = n_cidades,
          `Eventos por cidade` = total_eventos / n_cidades,
          `Cidade líder` = cidade_lider,
          `% na cidade líder` = 100 * eventos_cidade_lider / total_eventos
        )
      datatable(d, rownames = FALSE, options = opcoes_dt_padrao) %>%
        formatRound(c("% do total", "Eventos por cidade", "% na cidade líder"),
                    digits = 1, dec.mark = ",", mark = ".")
    })

  })
}
