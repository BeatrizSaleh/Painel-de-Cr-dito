# Monitor de Crédito – Dashboard Shiny
## Painel Executivo de Monitoramento Mensal | Banco Central do Brasil

---

## 📦 Dependências

Instale os pacotes necessários antes de rodar:

```r
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "tidyr",
  "lubridate",
  "scales",
  "plotly",
  "ggtext",
  "patchwork"
))
```

---

## ▶️ Como rodar

```r
# Opção 1 – diretamente no R
shiny::runApp("caminho/para/pasta/credit_dashboard")

# Opção 2 – com arquivo aberto no RStudio
# Abra app.R e clique em "Run App"
```

---

## 📁 Estrutura do Projeto

```
credit_dashboard/
│
├── app.R          ← Código principal do dashboard
└── README.md      ← Este arquivo
```

> Opcionalmente, crie a pasta `data/` para armazenar arquivos `.rds` ou `.csv` com os dados reais.

---

## 🔧 Como conectar dados reais (SGS/BCB)

### Opção 1 – API do SGS em tempo real

Instale o pacote `rbcb` e substitua a chamada à função `gerar_dados_simulados()`:

```r
install.packages("rbcb")
library(rbcb)

carregar_dados_bcb <- function(data_ini = "2015-01-01", data_fim = Sys.Date()) {
  ids <- c(
    inadimplencia_pf       = 21082,
    inadimplencia_pj       = 21083,
    endividamento_familias = 29037,
    comprometimento_renda  = 29038
    # adicione os demais IDs aqui...
  )

  dados <- rbcb::get_series(ids, start_date = data_ini, end_date = data_fim)
  # o rbcb retorna uma lista de data.frames; unifique por data
  Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), dados)
}
```

### Opção 2 – Arquivo local (.rds ou .csv)

```r
# Substitua a linha:
dados_brutos <- gerar_dados_simulados()

# Por:
dados_brutos <- readRDS("data/dados_credito.rds")
# ou
dados_brutos <- read.csv("data/dados_credito.csv")
dados_brutos$data <- as.Date(dados_brutos$data)
```

---

## 🗂️ Mapeamento das Colunas

| Coluna no data.frame         | Significado                          | Unidade    |
|-----------------------------|--------------------------------------|------------|
| `inadimplencia_pf`          | Inadimplência PF – Rec. Livres       | %          |
| `inadimplencia_pj`          | Inadimplência PJ – Rec. Livres       | %          |
| `endividamento_familias`    | Endividamento / Renda Anual          | %          |
| `comprometimento_renda`     | Comprometimento c/ Serviço da Dívida | %          |
| `saldo_{modalidade}_pf`     | Saldo da carteira                    | R$ bilhões |
| `conc_{modalidade}_pf`      | Concessões mensais                   | R$ bilhões |
| `juros_{modalidade}_pf`     | Taxa média de juros                  | % a.a.     |
| `inad_{modalidade}_pf`      | Inadimplência > 90 dias              | %          |

**Modalidades disponíveis** (sufixo `{modalidade}`):
`imob` | `veiculos` | `consig_priv` | `consig_serv` | `consig_inss` | `cartao` | `naoconsig` | `cheque`

---

## 🎨 Personalização Visual

Para alterar cores, edite o objeto `cores` no início do `app.R`:

```r
cores <- list(
  primaria   = "#1a5276",   # Azul escuro (cabeçalhos)
  secundaria = "#2874a6",   # Azul médio
  destaque   = "#e74c3c",   # Vermelho (inadimplência)
  ...
)
```

Para alterar a cor de cada modalidade, edite o campo `cor` na lista `modalidades`.

---

## 📥 Downloads

| Botão                  | O que baixa                        |
|-----------------------|------------------------------------|
| `⬇ PNG` (Visão Geral) | Gráfico individual (8×4.5 pol.)    |
| `⬇` (cada quadrante)  | Gráfico individual da modalidade   |
| `⬇ Bloco (PNG)`       | Os 4 gráficos da modalidade (2×2)  |
| `⬇ PNG` (Comparações) | Gráfico de comparação              |

---

## 📅 Atualização dos Dados

Para atualizar a base de dados:
1. Baixe as séries do SGS (https://www3.bcb.gov.br/sgspub)
2. Salve como `.rds` com o mesmo esquema de colunas descrito acima
3. Atualize o caminho em `dados_brutos <- readRDS(...)`

---

*Desenvolvido para monitoramento executivo mensal de crédito.*
