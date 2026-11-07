# ufc
Trabalho Final CE302

Análise de dados e aplicativo Shiny sobre o Ultimate Fighting Championship (UFC),
desenvolvidos como trabalho final da disciplina de Programação em R.

## Integrantes do grupo

| Nome | GitHub |
|---|---|
| Lucas | [@usuario1](https://github.com/usuario1) |
| Felipe | [@usuario2](https://github.com/usuario2) |
| Miguel Antunes de Lima | [@miguellima1188](https://github.com/miguellima1188)

## Banco de dados

**UFC Athletes and Fight Data**, publicado no TidyTuesday em 07 de julho de 2026.

Conjunto original disponível em:
https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-07-07

O banco é composto por cinco arquivos:

| Arquivo | Conteúdo | Registros |
|---|---|---|
| `ufc_athletes.csv` | Perfil dos atletas, atributos físicos e estatísticas de carreira | 3.140 |
| `ufc_fights.csv` | Histórico de lutas, eventos, métodos de vitória e árbitros | 8.736 |
| `ufc_rankings_dataset.csv` | Rankings divisionais e pound-for-pound ao longo do tempo | 534 datas |
| `ufcstats_data.csv` | Estatísticas consolidadas de striking e grappling | 4.497 |
| `ultimate_ufc_dataset.csv` | Lutas com odds de apostas e variáveis de diferença entre cantos | cerca de 6.000 |

## Objetivo do projeto

O trabalho se divide em duas entregas complementares. A primeira é um relatório
analítico em formato de artigo científico, escrito em Quarto, com análise estatística
do banco escolhido. A segunda é um aplicativo Shiny com dez abas, que apresenta os
principais resultados da análise de forma visual e interativa.

A pergunta que orienta o projeto é como atributos físicos, histórico de desempenho e
estilo de luta se associam ao resultado de uma luta no UFC. O recorte é descritivo e
associativo, sem pretensão causal.

## Estrutura do repositório

```
ufc/
├── data/
│   ├── raw/                # arquivos originais baixados do TidyTuesday
│   └── processed/          # dados tratados, salvos em .rds
├── R/
│   ├── 01_download.R       # download dos CSVs
│   ├── 02_cleaning.R       # limpeza e criação de variáveis
│   ├── 03_analysis.R       # funções de análise
│   └── 04_plots.R          # funções de gráficos
├── relatorio/
│   ├── relatorio.qmd       # documento principal
│   ├── referencias.bib     # bibliografia em BibTeX
│   └── relatorio.html      # versão renderizada
└── app/
    ├── app.R               # aplicativo Shiny
    ├── mod_*.R             # módulos, um por aba
    └── www/                # CSS e imagens
```

As funções em `R/` são carregadas tanto pelo relatório quanto pelo aplicativo. Isso garante que os números e gráficos exibidos no Shiny sejam os mesmos do texto.

## Aplicativo Shiny

Link do aplicativo publicado: *(a preencher após o deploy no shinyapps.io)*

As dez abas cobrem panorama geral, evolução temporal, métodos de vitória, perfil físico, correlações, vantagem física entre cantos, trajetória nos rankings, odds e previsão, estilos de luta e um explorador livre de variáveis. Cada aba traz ao menos um gráfico, uma tabela e seletores próprios.

## Vídeo de apresentação

Link do vídeo no YouTube: *(a preencher)*

## Como reproduzir

Requisitos: R 4.4 ou superior e Quarto 1.5 ou superior.

Pacotes necessários:

```r
install.packages(c(
  "tidyverse", "janitor", "tidymodels", "gt", "bslib", "shiny",
  "shinyWidgets", "plotly", "DT", "scales", "patchwork", "here"
))
```

Para baixar e tratar os dados:

```r
source("R/01_download.R")
source("R/02_cleaning.R")
```

Para renderizar o relatório:

```bash
quarto render relatorio/relatorio.qmd
```

Para executar o aplicativo localmente:

```r
shiny::runApp("app")
```

## Referências

- R for Data Science. (2026). *TidyTuesday: UFC Athletes and Fight Data*.