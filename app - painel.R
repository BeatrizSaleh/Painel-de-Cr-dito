# ==============================================================================
# DASHBOARD DE MONITORAMENTO DE CRÉDITO - BANCO CENTRAL
# Versão: 1.0 | Estilo Executivo
# ==============================================================================
# INSTRUÇÕES:
#   - Dados simulados são gerados automaticamente para demonstração
#   - Para usar dados reais do SGS/BCB, edite a seção "CARREGAMENTO DE DADOS"
#   - IDs das séries SGS estão mapeados em `sgs_ids` abaixo
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(plotly)
library(ggtext)
library(patchwork)
library(grDevices)

# ==============================================================================
# 1. MAPEAMENTO DE SÉRIES SGS (edite aqui para alterar IDs)
# ==============================================================================
sgs_ids <- list(
  # Visão Geral
  inadimplencia_pf        = 21082,   # Inadimplência PF - Recursos Livres
  inadimplencia_pj        = 21083,   # Inadimplência PJ - Recursos Livres
  endividamento_familias  = 29037,   # Endividamento das famílias / renda anual
  comprometimento_renda   = 29038,   # Comprometimento de renda c/ serviço da dívida
  
  # Modalidades PF - Saldo (R$ bi)
  saldo_imob_pf           = 4390,
  saldo_veiculos_pf       = 4391,
  saldo_consig_priv_pf    = 20588,
  saldo_consig_serv_pf    = 20589,
  saldo_consig_inss_pf    = 20590,
  saldo_cartao_pf         = 20591,
  saldo_naoconsig_pf      = 20592,
  saldo_cheque_pf         = 20593,
  
  # Modalidades PF - Concessões (R$ bi)
  conc_imob_pf            = 4440,
  conc_veiculos_pf        = 4441,
  conc_consig_priv_pf     = 20600,
  conc_consig_serv_pf     = 20601,
  conc_consig_inss_pf     = 20602,
  conc_cartao_pf          = 20603,
  conc_naoconsig_pf       = 20604,
  conc_cheque_pf          = 20605,
  
  # Modalidades PF - Taxa de Juros (% a.a.)
  juros_imob_pf           = 4390,   # substitua pelo ID correto
  juros_veiculos_pf       = 25475,
  juros_consig_priv_pf    = 25476,
  juros_consig_serv_pf    = 25477,
  juros_consig_inss_pf    = 25478,
  juros_cartao_pf         = 25479,
  juros_naoconsig_pf      = 25480,
  juros_cheque_pf         = 25481,
  
  # Modalidades PF - Inadimplência (%)
  inad_imob_pf            = 21084,
  inad_veiculos_pf        = 21085,
  inad_consig_priv_pf     = 21086,
  inad_consig_serv_pf     = 21087,
  inad_consig_inss_pf     = 21088,
  inad_cartao_pf          = 21089,
  inad_naoconsig_pf       = 21090,
  inad_cheque_pf          = 21091
)

# ==============================================================================
# 2. METADADOS DAS MODALIDADES
# ==============================================================================
modalidades <- list(
  list(
    id    = "imob",
    nome  = "Financiamento Imobiliário",
    cor   = "#1a5276"
  ),
  list(
    id    = "veiculos",
    nome  = "Veículos",
    cor   = "#1f618d"
  ),
  list(
    id    = "consig_priv",
    nome  = "Crédito Pessoal – Consignado Privados",
    cor   = "#2874a6"
  ),
  list(
    id    = "consig_serv",
    nome  = "Crédito Pessoal – Consignado Servidores",
    cor   = "#2e86c1"
  ),
  list(
    id    = "consig_inss",
    nome  = "Crédito Pessoal – Consignado INSS",
    cor   = "#3498db"
  ),
  list(
    id    = "cartao",
    nome  = "Cartão de Crédito (Total)",
    cor   = "#5dade2"
  ),
  list(
    id    = "naoconsig",
    nome  = "Crédito Pessoal Não Consignado",
    cor   = "#85c1e9"
  ),
  list(
    id    = "cheque",
    nome  = "Cheque Especial",
    cor   = "#aed6f1"
  )
)

# ==============================================================================
# 3. GERAÇÃO DE DADOS SIMULADOS
# ==============================================================================
# ATENÇÃO: Substitua esta função por leitura de arquivo .rds/.csv ou chamada
# à API do SGS para usar dados reais.
# Exemplo de leitura real: dados <- readRDS("dados_bcb.rds")
# ==============================================================================
gerar_dados_simulados <- function() {
  set.seed(42)
  datas <- seq(as.Date("2015-01-01"), as.Date("2024-12-01"), by = "month")
  n <- length(datas)
  t <- seq_len(n)
  
  trend  <- function(base, slope, noise_sd) base + slope * t + rnorm(n, 0, noise_sd)
  sazon  <- function(amp) amp * sin(2 * pi * t / 12)
  bounded <- function(x, lo, hi) pmax(lo, pmin(hi, x))
  
  dados <- list()
  
  # --- Visão Geral ---
  dados$inadimplencia_pf       <- bounded(trend(4.5, -0.005, 0.15) + sazon(0.2), 1, 10)
  dados$inadimplencia_pj       <- bounded(trend(3.8, -0.003, 0.12) + sazon(0.15), 1, 10)
  dados$endividamento_familias <- bounded(trend(46, 0.08, 0.4) + sazon(1), 30, 65)
  dados$comprometimento_renda  <- bounded(trend(26, 0.03, 0.2) + sazon(0.5), 18, 35)
  
  # --- Modalidades PF ---
  specs <- list(
    imob       = list(s = c(400, 0.9, 8),   c = c(12, 0.03, 0.5),  j = c(8,   0.01, 0.1),  i = c(1.5, -0.002, 0.05)),
    veiculos   = list(s = c(280, 0.5, 5),   c = c(18, 0.04, 0.6),  j = c(20,  0.02, 0.2),  i = c(4.0, -0.003, 0.10)),
    consig_priv= list(s = c(120, 0.3, 3),   c = c(6,  0.01, 0.2),  j = c(25,  0.01, 0.3),  i = c(2.5, -0.001, 0.06)),
    consig_serv= list(s = c(180, 0.4, 4),   c = c(8,  0.02, 0.3),  j = c(14,  0.005,0.1),  i = c(0.9, -0.001, 0.03)),
    consig_inss= list(s = c(220, 0.6, 5),   c = c(10, 0.02, 0.4),  j = c(21,  0.01, 0.2),  i = c(1.2, -0.001, 0.04)),
    cartao     = list(s = c(350, 0.7, 7),   c = c(90, 0.15, 2.0),  j = c(250, 0.1,  3.0),  i = c(40,   0.02,  0.5)),
    naoconsig  = list(s = c(130, 0.3, 3),   c = c(9,  0.02, 0.3),  j = c(55,  0.03, 0.6),  i = c(6.5,  0.003, 0.15)),
    cheque     = list(s = c(20,  0.0, 0.5), c = c(3,  0.00, 0.1),  j = c(130, 0.05, 1.5),  i = c(7.0,  0.005, 0.20))
  )
  
  for (mod in names(specs)) {
    sp <- specs[[mod]]
    dados[[paste0("saldo_",   mod, "_pf")]] <- bounded(trend(sp$s[1], sp$s[2], sp$s[3]), 0, Inf)
    dados[[paste0("conc_",    mod, "_pf")]] <- bounded(trend(sp$c[1], sp$c[2], sp$c[3]), 0, Inf)
    dados[[paste0("juros_",   mod, "_pf")]] <- bounded(trend(sp$j[1], sp$j[2], sp$j[3]) + sazon(sp$j[3]), 0, Inf)
    dados[[paste0("inad_",    mod, "_pf")]] <- bounded(trend(sp$i[1], sp$i[2], sp$i[3]) + sazon(sp$i[3] * 0.5), 0, 100)
  }
  
  # Converter para data.frame longo
  df <- data.frame(data = datas)
  for (nm in names(dados)) {
    df[[nm]] <- dados[[nm]]
  }
  df
}

# Carregar dados
dados_brutos <- gerar_dados_simulados()

# ==============================================================================
# 4. TEMA VISUAL
# ==============================================================================
cores <- list(
  primaria    = "#1a5276",
  secundaria  = "#2874a6",
  destaque    = "#e74c3c",
  fundo       = "#f8f9fa",
  texto       = "#2c3e50",
  grade       = "#dee2e6",
  cinza_claro = "#ecf0f1"
)

tema_executivo <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major   = element_line(color = cores$grade, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = cores$texto, size = 9),
    axis.title         = element_text(color = cores$texto, size = 9, face = "bold"),
    plot.title         = element_text(color = cores$primaria, size = 11, face = "bold", margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "#7f8c8d", size = 8.5, margin = margin(b = 8)),
    plot.caption       = element_text(color = "#95a5a6", size = 7.5, hjust = 1),
    legend.position    = "none",
    plot.margin        = margin(10, 12, 8, 10)
  )

# ==============================================================================
# 5. FUNÇÕES AUXILIARES
# ==============================================================================

# Filtrar dados por período
filtrar_dados <- function(df, data_ini, data_fim) {
  df %>% filter(data >= data_ini & data <= data_fim)
}

# Criar gráfico de série temporal genérico
criar_grafico <- function(df, col, titulo, subtitulo = NULL,
                          formato_y = "numero", cor_linha = cores$primaria,
                          mostrar_ultimo = TRUE) {
  
  if (!(col %in% names(df))) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Série não disponível",
                 color = "#95a5a6", size = 4) +
        theme_void() +
        ggtitle(titulo)
    )
  }
  
  serie <- df %>%
    select(data, valor = all_of(col)) %>%
    filter(!is.na(valor))
  
  if (nrow(serie) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados no período",
                 color = "#95a5a6", size = 4) +
        theme_void() + ggtitle(titulo)
    )
  }
  
  ultimo <- tail(serie, 1)
  
  # Formatador do eixo Y
  fmt_y <- switch(formato_y,
                  "bilhoes" = label_number(suffix = " bi", big.mark = ".", decimal.mark = ",", accuracy = 0.1),
                  "percent" = label_number(suffix = "%", big.mark = ".", decimal.mark = ",", accuracy = 0.1),
                  label_number(big.mark = ".", decimal.mark = ",", accuracy = 0.1)
  )
  
  # Label do último valor
  label_ultimo <- switch(formato_y,
                         "bilhoes" = paste0("R$ ", format(round(ultimo$valor, 1), nsmall = 1, big.mark = ".", decimal.mark = ","), " bi"),
                         "percent" = paste0(format(round(ultimo$valor, 1), nsmall = 1, decimal.mark = ","), "%"),
                         format(round(ultimo$valor, 1), decimal.mark = ",")
  )
  
  p <- ggplot(serie, aes(x = data, y = valor)) +
    geom_area(fill = cor_linha, alpha = 0.10) +
    geom_line(color = cor_linha, linewidth = 0.9) +
    scale_x_date(date_labels = "%b/%y", date_breaks = "1 year",
                 expand = expansion(mult = c(0.01, 0.05))) +
    scale_y_continuous(labels = fmt_y, expand = expansion(mult = c(0.05, 0.12))) +
    labs(title = titulo, subtitle = subtitulo,
         x = NULL, y = NULL,
         caption = paste0("Último: ", format(ultimo$data, "%b/%Y"))) +
    tema_executivo
  
  if (mostrar_ultimo) {
    p <- p +
      geom_point(data = ultimo, aes(x = data, y = valor),
                 color = cor_linha, size = 2.5) +
      annotate("text", x = ultimo$data, y = ultimo$valor,
               label = label_ultimo, hjust = -0.15, vjust = 0.5,
               color = cor_linha, size = 3, fontface = "bold")
  }
  
  p
}

# Criar gráfico plotly interativo
criar_plotly <- function(df, col, titulo, formato_y = "numero", cor_linha = cores$primaria) {
  p_gg <- criar_grafico(df, col, titulo, formato_y = formato_y, cor_linha = cor_linha)
  
  fmt_hover <- switch(formato_y,
                      "bilhoes" = function(v) paste0("R$ ", round(v, 2), " bi"),
                      "percent" = function(v) paste0(round(v, 2), "%"),
                      function(v) round(v, 2)
  )
  
  if (!(col %in% names(df))) {
    return(plotly::ggplotly(p_gg))
  }
  
  serie <- df %>% select(data, valor = all_of(col)) %>% filter(!is.na(valor))
  
  plotly::plot_ly(serie, x = ~data, y = ~valor, type = "scatter", mode = "lines",
                  fill = "tozeroy",
                  fillcolor = paste0(cor_linha, "15"),
                  line = list(color = cor_linha, width = 2),
                  hovertemplate = paste0("<b>%{x|%b/%Y}</b><br>",
                                         titulo, ": %{y:.2f}<extra></extra>")) %>%
    plotly::layout(
      title = list(text = paste0("<b>", titulo, "</b>"), font = list(size = 12, color = cores$primaria)),
      xaxis = list(title = "", showgrid = TRUE, gridcolor = "#eeeeee"),
      yaxis = list(title = "", showgrid = TRUE, gridcolor = "#eeeeee"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      margin = list(l = 50, r = 20, t = 40, b = 30),
      hoverlabel = list(bgcolor = "white", font = list(size = 11))
    )
}

# Botão de download para ggplot
botao_download_ui <- function(id, label = "⬇ PNG") {
  downloadButton(id, label = label,
                 style = "font-size:10px; padding:3px 8px; background:#1a5276;
                          color:white; border:none; border-radius:4px; cursor:pointer;")
}

botao_download_server <- function(id, plot_fn, filename, session) {
  downloadHandler(
    filename = function() paste0(filename, "_", format(Sys.Date(), "%Y%m%d"), ".png"),
    content = function(file) {
      p <- plot_fn()
      ggsave(file, plot = p, width = 8, height = 4.5, dpi = 180, bg = "white")
    }
  )
}

# ==============================================================================
# 6. UI
# ==============================================================================

# CSS separado para injetar via tags$head
css_dashboard <- tags$head(tags$style(HTML("
    body { background-color: #f0f2f5 !important; }

    .card-monitor {
      background: white;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      overflow: hidden;
    }

    .card-header-monitor {
      background: #1a5276;
      color: white;
      padding: 8px 12px;
      font-size: 12px;
      font-weight: 600;
      display: flex;
      align-items: center;
      justify-content: space-between;
      letter-spacing: 0.3px;
    }

    .card-body-monitor {
      padding: 10px;
    }

    .bloco-modalidade {
      background: white;
      border: 1px solid #dee2e6;
      border-radius: 10px;
      box-shadow: 0 1px 6px rgba(0,0,0,0.07);
      margin-bottom: 20px;
      overflow: hidden;
    }

    .bloco-header {
      padding: 10px 16px;
      font-size: 13px;
      font-weight: 700;
      color: white;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }

    .bloco-body {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 0;
    }

    .grafico-cell {
      padding: 10px;
      border: 0.5px solid #f0f0f0;
    }

    .grafico-cell-header {
      font-size: 10.5px;
      color: #555;
      font-weight: 600;
      margin-bottom: 2px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .shiny-download-link {
      text-decoration: none !important;
    }

    .btn-dl {
      font-size: 10px !important;
      padding: 2px 7px !important;
      background: rgba(255,255,255,0.2) !important;
      color: white !important;
      border: 1px solid rgba(255,255,255,0.4) !important;
      border-radius: 3px !important;
    }

    .barra-periodo {
      background: #f8f9fa;
      border-bottom: 1px solid #dee2e6;
      padding: 8px 20px;
      display: flex;
      align-items: center;
      gap: 16px;
      flex-wrap: wrap;
    }

    .form-group { margin-bottom: 0 !important; }
    .navbar { box-shadow: 0 2px 6px rgba(0,0,0,0.1); }

    @media (max-width: 768px) {
      .bloco-body { grid-template-columns: 1fr; }
    }
")))

# Barra de filtro de período (injetada via header= uma única vez)
barra_periodo <- div(
  class = "barra-periodo",
  div(style = "font-size:12px; color:#555; font-weight:600;", "PERÍODO:"),
  dateRangeInput("periodo", NULL,
                 start = "2019-01-01", end   = "2024-12-01",
                 min   = "2015-01-01", max   = "2024-12-01",
                 format = "mm/yyyy",   language = "pt-BR",
                 width = "260px"),
  div(
    style = "font-size:11px; color:#888; margin-left:auto;",
    paste0("Dados simulados para demonstração | Atualizado: ",
           format(Sys.Date(), "%d/%m/%Y"))
  )
)

ui <- page_navbar(
  title = tags$span(
    tags$b("Monitor de Crédito", style = "color:#1a5276; font-size:18px;"),
    tags$span(" | Banco Central do Brasil",
              style = "color:#7f8c8d; font-size:13px; margin-left:6px;")
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#1a5276",
    base_font  = font_google("Inter"),
    code_font  = font_google("Fira Code")
  ),
  bg       = "white",
  fillable = FALSE,
  
  # Um único header= com CSS + barra de filtro combinados
  header = tagList(css_dashboard, barra_periodo),
  
  # ==================== ABA 1: VISÃO GERAL ====================
  nav_panel(
    title = "📊 Visão Geral",
    value = "geral",
    
    div(style = "padding:16px;",
        
        # Título da seção
        div(style = "margin-bottom:16px;",
            h4("Indicadores Macroeconômicos de Crédito",
               style = "color:#1a5276; font-weight:700; margin:0;"),
            p("Monitoramento dos principais agregados de crédito do sistema financeiro nacional.",
              style = "color:#7f8c8d; font-size:13px; margin:4px 0 0 0;")
        ),
        
        # Linha 1: Inadimplência
        div(style = "display:grid; grid-template-columns:1fr 1fr; gap:16px; margin-bottom:16px;",
            div(class = "card-monitor",
                div(class = "card-header-monitor",
                    span("Inadimplência – Recursos Livres PF"),
                    botao_download_ui("dl_inad_pf")),
                plotlyOutput("plt_inad_pf", height = "240px")),
            div(class = "card-monitor",
                div(class = "card-header-monitor",
                    span("Inadimplência – Recursos Livres PJ"),
                    botao_download_ui("dl_inad_pj")),
                plotlyOutput("plt_inad_pj", height = "240px"))
        ),
        
        # Linha 2: Endividamento e Comprometimento
        div(style = "display:grid; grid-template-columns:1fr 1fr; gap:16px;",
            div(class = "card-monitor",
                div(class = "card-header-monitor",
                    span("Endividamento das Famílias / Renda Anual"),
                    botao_download_ui("dl_endiv")),
                plotlyOutput("plt_endiv", height = "240px")),
            div(class = "card-monitor",
                div(class = "card-header-monitor",
                    span("Comprometimento de Renda c/ Serviço da Dívida"),
                    botao_download_ui("dl_comp")),
                plotlyOutput("plt_comp", height = "240px"))
        )
    )
  ),
  
  # ==================== ABA 2: MODALIDADES PF ====================
  nav_panel(
    title = "🏦 Modalidades PF",
    value = "modalidades",
    
    div(style = "padding:16px;",
        h4("Monitoramento por Modalidade – Pessoa Física",
           style = "color:#1a5276; font-weight:700; margin-bottom:4px;"),
        p("Saldo, Concessões, Taxa de Juros e Inadimplência por modalidade de crédito.",
          style = "color:#7f8c8d; font-size:13px; margin-bottom:16px;"),
        
        # Gerar um bloco para cada modalidade
        uiOutput("blocos_modalidades")
    )
  ),
  
  # ==================== ABA 3: COMPARAÇÕES ====================
  nav_panel(
    title = "🔀 Comparações",
    value = "comparacoes",
    
    div(style = "padding:16px;",
        h4("Comparação entre Modalidades",
           style = "color:#1a5276; font-weight:700; margin-bottom:4px;"),
        p("Selecione duas modalidades e um indicador para comparar.",
          style = "color:#7f8c8d; font-size:13px; margin-bottom:16px;"),
        
        # Controles
        div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:12px; margin-bottom:16px;",
            selectInput("comp_mod1", "Modalidade 1:",
                        choices = setNames(
                          sapply(modalidades, `[[`, "id"),
                          sapply(modalidades, `[[`, "nome")
                        ), selected = "imob"),
            selectInput("comp_mod2", "Modalidade 2:",
                        choices = setNames(
                          sapply(modalidades, `[[`, "id"),
                          sapply(modalidades, `[[`, "nome")
                        ), selected = "veiculos"),
            selectInput("comp_ind", "Indicador:",
                        choices = c(
                          "Saldo (R$ bi)"           = "saldo",
                          "Concessões (R$ bi)"      = "conc",
                          "Taxa de Juros (% a.a.)"  = "juros",
                          "Inadimplência (%)"       = "inad"
                        ))
        ),
        
        div(class = "card-monitor",
            div(class = "card-header-monitor",
                span("Comparação"), botao_download_ui("dl_comp_chart")),
            plotlyOutput("plt_comparacao", height = "380px"))
    )
  ),
  
)

# ==============================================================================
# 7. SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # Dados reativos filtrados pelo período global
  dados <- reactive({
    req(input$periodo)
    filtrar_dados(dados_brutos, input$periodo[1], input$periodo[2])
  })
  
  # ---------- VISÃO GERAL ----------
  output$plt_inad_pf <- renderPlotly({
    criar_plotly(dados(), "inadimplencia_pf", "Inadimplência PF (%)", "percent", "#c0392b")
  })
  output$dl_inad_pf  <- downloadHandler(
    filename = function() "inadimplencia_pf.png",
    content  = function(file) {
      p <- criar_grafico(dados(), "inadimplencia_pf", "Inadimplência – Recursos Livres PF (%)", formato_y = "percent", cor_linha = "#c0392b")
      ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
    }
  )
  
  output$plt_inad_pj <- renderPlotly({
    criar_plotly(dados(), "inadimplencia_pj", "Inadimplência PJ (%)", "percent", "#e67e22")
  })
  output$dl_inad_pj  <- downloadHandler(
    filename = function() "inadimplencia_pj.png",
    content  = function(file) {
      p <- criar_grafico(dados(), "inadimplencia_pj", "Inadimplência – Recursos Livres PJ (%)", formato_y = "percent", cor_linha = "#e67e22")
      ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
    }
  )
  
  output$plt_endiv <- renderPlotly({
    criar_plotly(dados(), "endividamento_familias", "Endividamento das Famílias (%)", "percent", "#1a5276")
  })
  output$dl_endiv  <- downloadHandler(
    filename = function() "endividamento_familias.png",
    content  = function(file) {
      p <- criar_grafico(dados(), "endividamento_familias", "Endividamento das Famílias / Renda Anual (%)", formato_y = "percent")
      ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
    }
  )
  
  output$plt_comp <- renderPlotly({
    criar_plotly(dados(), "comprometimento_renda", "Comprometimento de Renda (%)", "percent", "#2ecc71")
  })
  output$dl_comp  <- downloadHandler(
    filename = function() "comprometimento_renda.png",
    content  = function(file) {
      p <- criar_grafico(dados(), "comprometimento_renda", "Comprometimento de Renda com Serviço da Dívida (%)", formato_y = "percent", cor_linha = "#2ecc71")
      ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
    }
  )
  
  # ---------- MODALIDADES PF – renderUI ----------
  output$blocos_modalidades <- renderUI({
    df <- dados()
    blocos <- lapply(modalidades, function(mod) {
      id  <- mod$id
      cor <- mod$cor
      nm  <- mod$nome
      
      # Gerar IDs únicos para cada gráfico e download
      id_s  <- paste0("plt_saldo_",   id)
      id_c  <- paste0("plt_conc_",    id)
      id_j  <- paste0("plt_juros_",   id)
      id_i  <- paste0("plt_inad_",    id)
      dl_s  <- paste0("dl_saldo_",    id)
      dl_c  <- paste0("dl_conc_",     id)
      dl_j  <- paste0("dl_juros_",    id)
      dl_i  <- paste0("dl_inad_",     id)
      dl_bl <- paste0("dl_bloco_",    id)
      
      div(class = "bloco-modalidade",
          # Cabeçalho do bloco
          div(class = "bloco-header",
              style = paste0("background:", cor, ";"),
              tags$span(nm, style = "font-size:13px;"),
              downloadButton(dl_bl, "⬇ Bloco (PNG)",
                             style = "font-size:10px; padding:3px 8px; background:rgba(255,255,255,0.2);
                                      color:white; border:1px solid rgba(255,255,255,0.4);
                                      border-radius:4px; cursor:pointer;")
          ),
          
          # Grade 2x2
          div(class = "bloco-body",
              # Saldo
              div(class = "grafico-cell",
                  div(class = "grafico-cell-header",
                      span("Saldo (R$ bilhões)"),
                      downloadButton(dl_s, "⬇", style = "font-size:9px; padding:1px 5px; background:#1a5276; color:white; border:none; border-radius:3px;")),
                  plotlyOutput(id_s, height = "180px")),
              # Concessões
              div(class = "grafico-cell",
                  div(class = "grafico-cell-header",
                      span("Concessões (R$ bilhões)"),
                      downloadButton(dl_c, "⬇", style = "font-size:9px; padding:1px 5px; background:#1a5276; color:white; border:none; border-radius:3px;")),
                  plotlyOutput(id_c, height = "180px")),
              # Juros
              div(class = "grafico-cell",
                  div(class = "grafico-cell-header",
                      span("Taxa Média de Juros (% a.a.)"),
                      downloadButton(dl_j, "⬇", style = "font-size:9px; padding:1px 5px; background:#1a5276; color:white; border:none; border-radius:3px;")),
                  plotlyOutput(id_j, height = "180px")),
              # Inadimplência
              div(class = "grafico-cell",
                  div(class = "grafico-cell-header",
                      span("Inadimplência (> 90 dias, %)"),
                      downloadButton(dl_i, "⬇", style = "font-size:9px; padding:1px 5px; background:#1a5276; color:white; border:none; border-radius:3px;")),
                  plotlyOutput(id_i, height = "180px"))
          )
      )
    })
    do.call(tagList, blocos)
  })
  
  # ---------- OUTPUTS DINÂMICOS DAS MODALIDADES ----------
  observe({
    df <- dados()
    lapply(modalidades, function(mod) {
      id  <- mod$id
      cor <- mod$cor
      nm  <- mod$nome
      
      col_s <- paste0("saldo_",  id, "_pf")
      col_c <- paste0("conc_",   id, "_pf")
      col_j <- paste0("juros_",  id, "_pf")
      col_i <- paste0("inad_",   id, "_pf")
      
      local({
        .id   <- id
        .cor  <- cor
        .nm   <- nm
        .cs   <- col_s; .cc <- col_c; .cj <- col_j; .ci <- col_i
        
        output[[paste0("plt_saldo_", .id)]] <- renderPlotly(
          criar_plotly(df, .cs, paste0("Saldo – ", .nm), "bilhoes", .cor)
        )
        output[[paste0("plt_conc_", .id)]] <- renderPlotly(
          criar_plotly(df, .cc, paste0("Concessões – ", .nm), "bilhoes", .cor)
        )
        output[[paste0("plt_juros_", .id)]] <- renderPlotly(
          criar_plotly(df, .cj, paste0("Juros – ", .nm), "percent", .cor)
        )
        output[[paste0("plt_inad_", .id)]] <- renderPlotly(
          criar_plotly(df, .ci, paste0("Inadimplência – ", .nm), "percent", "#c0392b")
        )
        
        # Downloads individuais
        output[[paste0("dl_saldo_", .id)]] <- downloadHandler(
          filename = function() paste0("saldo_", .id, ".png"),
          content  = function(file) {
            p <- criar_grafico(df, .cs, paste0("Saldo – ", .nm, " (R$ bi)"), formato_y = "bilhoes", cor_linha = .cor)
            ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
          }
        )
        output[[paste0("dl_conc_", .id)]] <- downloadHandler(
          filename = function() paste0("concessoes_", .id, ".png"),
          content  = function(file) {
            p <- criar_grafico(df, .cc, paste0("Concessões – ", .nm, " (R$ bi)"), formato_y = "bilhoes", cor_linha = .cor)
            ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
          }
        )
        output[[paste0("dl_juros_", .id)]] <- downloadHandler(
          filename = function() paste0("juros_", .id, ".png"),
          content  = function(file) {
            p <- criar_grafico(df, .cj, paste0("Taxa de Juros – ", .nm, " (% a.a.)"), formato_y = "percent", cor_linha = .cor)
            ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
          }
        )
        output[[paste0("dl_inad_", .id)]] <- downloadHandler(
          filename = function() paste0("inadimplencia_", .id, ".png"),
          content  = function(file) {
            p <- criar_grafico(df, .ci, paste0("Inadimplência – ", .nm, " (%)"), formato_y = "percent", cor_linha = "#c0392b")
            ggsave(file, p, width = 8, height = 4.5, dpi = 180, bg = "white")
          }
        )
        
        # Download do bloco (2x2 em patchwork)
        output[[paste0("dl_bloco_", .id)]] <- downloadHandler(
          filename = function() paste0("bloco_", .id, "_", format(Sys.Date(), "%Y%m%d"), ".png"),
          content  = function(file) {
            p1 <- criar_grafico(df, .cs, "Saldo (R$ bi)",          formato_y = "bilhoes", cor_linha = .cor)
            p2 <- criar_grafico(df, .cc, "Concessões (R$ bi)",     formato_y = "bilhoes", cor_linha = .cor)
            p3 <- criar_grafico(df, .cj, "Taxa Juros (% a.a.)",    formato_y = "percent",  cor_linha = .cor)
            p4 <- criar_grafico(df, .ci, "Inadimplência (% >90d)", formato_y = "percent",  cor_linha = "#c0392b")
            
            bloco <- (p1 | p2) / (p3 | p4) +
              patchwork::plot_annotation(
                title    = .nm,
                subtitle = paste0("Período: ", format(min(df$data), "%b/%Y"),
                                  " – ", format(max(df$data), "%b/%Y")),
                theme    = theme(
                  plot.title    = element_text(size = 14, face = "bold", color = .cor),
                  plot.subtitle = element_text(size = 10, color = "#7f8c8d")
                )
              )
            ggsave(file, bloco, width = 14, height = 8, dpi = 150, bg = "white")
          }
        )
      })
    })
  })
  
  # ---------- COMPARAÇÕES ----------
  output$plt_comparacao <- renderPlotly({
    df   <- dados()
    mod1 <- input$comp_mod1
    mod2 <- input$comp_mod2
    ind  <- input$comp_ind
    
    col1 <- paste0(ind, "_", mod1, "_pf")
    col2 <- paste0(ind, "_", mod2, "_pf")
    
    nome1 <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod1)]]$nome
    nome2 <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod2)]]$nome
    cor1  <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod1)]]$cor
    cor2  <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod2)]]$cor
    
    titulo_ind <- switch(ind,
                         "saldo" = "Saldo (R$ bi)", "conc" = "Concessões (R$ bi)",
                         "juros" = "Taxa de Juros (% a.a.)", "inad" = "Inadimplência (%)"
    )
    
    s1 <- if (col1 %in% names(df)) df %>% select(data, valor = all_of(col1)) %>% mutate(serie = nome1) else NULL
    s2 <- if (col2 %in% names(df)) df %>% select(data, valor = all_of(col2)) %>% mutate(serie = nome2) else NULL
    
    if (is.null(s1) && is.null(s2)) {
      return(plotly::plot_ly() %>% plotly::layout(title = "Séries não disponíveis"))
    }
    
    series_combinadas <- bind_rows(s1, s2)
    
    plotly::plot_ly(series_combinadas, x = ~data, y = ~valor, color = ~serie,
                    colors = c(cor1, cor2),
                    type = "scatter", mode = "lines",
                    hovertemplate = "<b>%{x|%b/%Y}</b><br>%{fullData.name}: %{y:.2f}<extra></extra>") %>%
      plotly::layout(
        title    = list(text = paste0("<b>", titulo_ind, "</b>"),
                        font = list(size = 13, color = "#1a5276")),
        xaxis    = list(title = ""),
        yaxis    = list(title = titulo_ind),
        legend   = list(orientation = "h", x = 0, y = -0.15),
        paper_bgcolor = "white", plot_bgcolor = "white",
        margin   = list(l = 60, r = 20, t = 50, b = 60),
        hoverlabel = list(bgcolor = "white")
      )
  })
  
  output$dl_comp_chart <- downloadHandler(
    filename = function() paste0("comparacao_", input$comp_ind, ".png"),
    content  = function(file) {
      df   <- dados()
      mod1 <- input$comp_mod1; mod2 <- input$comp_mod2; ind <- input$comp_ind
      col1 <- paste0(ind, "_", mod1, "_pf"); col2 <- paste0(ind, "_", mod2, "_pf")
      nome1 <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod1)]]$nome
      nome2 <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod2)]]$nome
      cor1  <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod1)]]$cor
      cor2  <- modalidades[[which(sapply(modalidades, `[[`, "id") == mod2)]]$cor
      
      fmt_y <- switch(ind, "saldo" = "bilhoes", "conc" = "bilhoes", "percent")
      
      s1 <- if (col1 %in% names(df)) df %>% select(data, valor = all_of(col1)) %>% mutate(serie = nome1) else NULL
      s2 <- if (col2 %in% names(df)) df %>% select(data, valor = all_of(col2)) %>% mutate(serie = nome2) else NULL
      combinado <- bind_rows(s1, s2)
      
      titulo_ind <- switch(ind, "saldo"="Saldo (R$ bi)", "conc"="Concessões (R$ bi)",
                           "juros"="Taxa de Juros (% a.a.)", "inad"="Inadimplência (%)")
      fmt_fn <- if (fmt_y == "bilhoes") label_number(suffix=" bi") else label_percent(scale=1)
      
      p <- ggplot(combinado, aes(x = data, y = valor, color = serie)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = setNames(c(cor1, cor2), c(nome1, nome2))) +
        scale_y_continuous(labels = fmt_fn) +
        scale_x_date(date_labels = "%b/%y", date_breaks = "1 year") +
        labs(title = titulo_ind, color = NULL, x = NULL, y = NULL) +
        tema_executivo +
        theme(legend.position = "bottom")
      ggsave(file, p, width = 10, height = 5, dpi = 180, bg = "white")
    }
  )
}

# ==============================================================================
# 8. EXECUTAR
# ==============================================================================
shinyApp(ui = ui, server = server)
