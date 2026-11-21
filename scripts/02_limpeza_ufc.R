library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

# 1. CARREGAMENTO DOS DADOS BRUTOS

ufc_athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_athletes.csv')
ufc_fights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_fights.csv')
ufc_rankings_dataset <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_rankings_dataset.csv')
ufcstats_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufcstats_data.csv')
ultimate_ufc_dataset <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ultimate_ufc_dataset.csv')

# 2. FUNÇÕES AUXILIARES

tempo_para_seg <- function(x) {
  mins <- as.numeric(str_extract(x, "\\d+(?=M)"))
  segs <- as.numeric(str_extract(x, "\\d+(?=S)"))
  mins[is.na(mins)] <- 0
  segs[is.na(segs)] <- 0
  ifelse(is.na(x), NA_real_, mins * 60 + segs)
}

limpar_texto <- function(x) {
  str_squish(as.character(x))
}

# 3. BASE PRINCIPAL DE LUTAS (SEM APOSTAS E SEM RANKINGS ESPARSOS)

ufc_lutas_completo <- ultimate_ufc_dataset %>%
  select(
    -matches("^[rb]_(womens_|heavyweight|light_heavyweight|middleweight|welterweight|lightweight|featherweight|bantamweight|flyweight).*_rank$"),
    -matches("(_odds|_ev)$")
  ) %>%
  mutate(
    date = as.Date(date),
    r_fighter = limpar_texto(r_fighter),
    b_fighter = limpar_texto(b_fighter),
    weight_class = limpar_texto(weight_class),
    finish_details = str_to_title(limpar_texto(finish_details)),
    r_weight_kg = round(r_weight_lbs * 0.453592, 1),
    b_weight_kg = round(b_weight_lbs * 0.453592, 1)
  )

# Mapa auxiliar de gênero por lutador
mapa_genero <- bind_rows(
  ufc_lutas_completo %>% select(name = r_fighter, gender),
  ufc_lutas_completo %>% select(name = b_fighter, gender)
) %>%
  filter(!is.na(name), !is.na(gender)) %>%
  distinct(name, .keep_all = TRUE)

# Auxiliar do ufcstats (stance, dob e nc)
ufcstats_aux <- ufcstats_data %>%
  mutate(
    name = limpar_texto(name),
    dob = mdy(dob),
    stance = na_if(limpar_texto(stance), "")
  ) %>%
  select(name, stance, dob, nc) %>%
  distinct(name, .keep_all = TRUE)

# 4. ABA 2 & 5: ATLETAS (MASC/FEM + CARTEL) E ESTATÍSTICAS/RECORDES

atletas_limpo <- ufc_athletes %>%
  select(-profile_name, -profile_nickname) %>%
  mutate(
    name = limpar_texto(name),
    nickname = na_if(limpar_texto(nickname), ""),
    weight_class = limpar_texto(weight_class),
    octagon_debut = as.Date(octagon_debut),
    avg_fight_time_secs = tempo_para_seg(average_fight_time),
    height_cm = round(height * 2.54, 1),
    reach_cm = round(reach * 2.54, 1),
    leg_reach_cm = round(leg_reach * 2.54, 1),
    weight_kg = round(weight * 0.453592, 1),
    total_fights = wins + losses + draws,
    win_rate = ifelse(total_fights > 0, round(wins / total_fights, 3), NA_real_),
    total_finishes = coalesce(ko_tko_win, 0L) + coalesce(sub_wins, 0L)
  ) %>%
  left_join(mapa_genero, by = "name") %>%
  mutate(
    gender = case_when(
      !is.na(gender) ~ gender,
      str_detect(weight_class, regex("Women", ignore_case = TRUE)) ~ "FEMALE",
      !is.na(weight_class) ~ "MALE",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(ufcstats_aux, by = "name")

# 5. ABA 3 & 4: RANKINGS POR PESO E EVOLUÇÃO HISTÓRICA

rankings_limpo <- ufc_rankings_dataset %>%
  rename(weight_class = weightclass) %>%
  mutate(
    date = as.Date(date),
    weight_class = limpar_texto(weight_class),
    fighter = limpar_texto(fighter)
  ) %>%
  filter(!is.na(fighter), fighter != "") %>%
  arrange(desc(date), weight_class, rank)

rankings_atual <- rankings_limpo %>%
  filter(date == max(date, na.rm = TRUE))

ranking_longevidade <- rankings_limpo %>%
  group_by(fighter, weight_class) %>%
  summarise(
    semanas_ranqueado = n(),
    semanas_campeao_ou_top1 = sum(rank <= 1, na.rm = TRUE),
    semanas_top5 = sum(rank <= 5, na.rm = TRUE),
    melhor_posicao = min(rank, na.rm = TRUE),
    primeira_aparicao = min(date, na.rm = TRUE),
    ultima_aparicao = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(semanas_campeao_ou_top1), desc(semanas_ranqueado))

# 6. ABA 6: COMPARADOR DE LUTADORES

comparador_atletas <- atletas_limpo %>%
  select(
    name, nickname, gender, weight_class, status, stance, age,
    height_cm, reach_cm, leg_reach_cm, weight_kg,
    wins, losses, draws, nc, total_fights, win_rate,
    ko_tko_win, sub_wins, dec_wins,
    ko_tko_percent, sub_percent, dec_percent,
    sig_str_landed, sig_str_absorbed, sig_str_defense,
    takedown_avg, takedown_defense, submission_avg, knockdown_avg,
    standing_pct, clinch_percent, ground_percent, avg_fight_time_secs
  )

# 7. ABA 7: COMPARAÇÃO DE CATEGORIAS

cat_lutas <- ufc_lutas_completo %>%
  filter(!is.na(weight_class)) %>%
  group_by(weight_class, gender) %>%
  summarise(
    total_lutas = n(),
    pct_ko = round(mean(finish == "KO/TKO", na.rm = TRUE), 3),
    pct_sub = round(mean(finish == "SUB", na.rm = TRUE), 3),
    pct_dec = round(mean(str_detect(finish, "DEC"), na.rm = TRUE), 3),
    tempo_medio_seg = round(mean(total_fight_time_secs, na.rm = TRUE), 0),
    round_medio = round(mean(finish_round, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat_fisico <- atletas_limpo %>%
  filter(!is.na(weight_class)) %>%
  group_by(weight_class) %>%
  summarise(
    total_atletas = n(),
    altura_media_cm = round(mean(height_cm, na.rm = TRUE), 1),
    envergadura_media_cm = round(mean(reach_cm, na.rm = TRUE), 1),
    idade_media = round(mean(age, na.rm = TRUE), 1),
    golpes_sig_min = round(mean(sig_str_landed, na.rm = TRUE), 2),
    quedas_media = round(mean(takedown_avg, na.rm = TRUE), 2),
    .groups = "drop"
  )

comparacao_categorias <- cat_lutas %>%
  left_join(cat_fisico, by = "weight_class") %>%
  arrange(desc(total_lutas))

# 8. ABA 8: RAIO-X DAS FINALIZAÇÕES E GOLPES

lutas_metodos <- ufc_lutas_completo %>%
  filter(!is.na(finish)) %>%
  mutate(
    ano = year(date),
    finish_details_limpo = case_when(
      is.na(finish_details) | finish_details == "" ~ "Não especificado / Decisão",
      str_detect(finish_details, regex("^Punch", ignore_case = TRUE)) ~ "Punches",
      str_detect(finish_details, regex("^Kick", ignore_case = TRUE)) ~ "Kicks",
      str_detect(finish_details, regex("^Elbow", ignore_case = TRUE)) ~ "Elbows",
      str_detect(finish_details, regex("^Knee", ignore_case = TRUE)) ~ "Knees",
      TRUE ~ finish_details
    )
  )

raio_x_finalizacoes <- lutas_metodos %>%
  filter(finish_details_limpo != "Não especificado / Decisão") %>%
  count(gender, weight_class, finish, finish_details_limpo, name = "total_lutas", sort = TRUE)

metodos_por_ano <- lutas_metodos %>%
  count(ano, gender, finish, name = "total_lutas") %>%
  group_by(ano, gender) %>%
  mutate(pct = round(total_lutas / sum(total_lutas), 3)) %>%
  ungroup()

# 9. ABA 9: DISPUTAS DE CINTURÃO

lutas_cinturao <- ufc_lutas_completo %>%
  filter(title_bout == TRUE) %>%
  mutate(
    vencedor_nome = case_when(
      winner == "Red"  ~ r_fighter,
      winner == "Blue" ~ b_fighter,
      TRUE ~ "Empate / NC"
    )
  ) %>%
  select(
    date, weight_class, gender, r_fighter, b_fighter, vencedor_nome,
    finish, finish_details, finish_round, finish_round_time, location, country
  ) %>%
  arrange(desc(date))

disputadores_cinturao <- bind_rows(
  lutas_cinturao %>%
    transmute(fighter = r_fighter, venceu = (vencedor_nome == r_fighter), weight_class, date),
  lutas_cinturao %>%
    transmute(fighter = b_fighter, venceu = (vencedor_nome == b_fighter), weight_class, date)
) %>%
  group_by(fighter) %>%
  summarise(
    disputas_titulo = n(),
    vitorias_titulo = sum(venceu, na.rm = TRUE),
    derrotas_titulo = disputas_titulo - vitorias_titulo,
    taxa_vitoria_titulo = round(vitorias_titulo / disputas_titulo, 3),
    categorias = paste(unique(weight_class), collapse = " / "),
    primeira_disputa = min(date, na.rm = TRUE),
    ultima_disputa = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(disputas_titulo), desc(vitorias_titulo))

# 10. ABA 10: LOCAIS COM MAIS EVENTOS

lutas_eventos_limpo <- ufc_fights %>%
  mutate(
    date = as.Date(date),
    weight_class = str_remove(limpar_texto(weight_class), " Bout$"),
    f1_name = limpar_texto(f1_name),
    f2_name = limpar_texto(f2_name),
    referee = limpar_texto(referee),
    fight_time_secs = tempo_para_seg(time)
  )

eventos_unicos <- lutas_eventos_limpo %>%
  distinct(event_name, date, location) %>%
  mutate(
    pais = str_trim(str_extract(location, "[^,]+$")),
    cidade = str_trim(str_extract(location, "^[^,]+"))
  )

locais_mais_eventos <- eventos_unicos %>%
  count(location, cidade, pais, name = "total_eventos", sort = TRUE)

paises_mais_eventos <- eventos_unicos %>%
  count(pais, name = "total_eventos", sort = TRUE)

# 11. ABA 1: VISÃO GERAL (HOME / OVERVIEW)

kpis_gerais <- tibble(
  total_lutas = nrow(ufc_lutas_completo),
  total_eventos = nrow(eventos_unicos),
  total_atletas = nrow(atletas_limpo),
  total_paises = n_distinct(eventos_unicos$pais, na.rm = TRUE),
  total_lutas_titulo = sum(ufc_lutas_completo$title_bout == TRUE, na.rm = TRUE),
  pct_ko_geral = round(mean(ufc_lutas_completo$finish == "KO/TKO", na.rm = TRUE), 3),
  pct_sub_geral = round(mean(ufc_lutas_completo$finish == "SUB", na.rm = TRUE), 3),
  pct_dec_geral = round(mean(str_detect(ufc_lutas_completo$finish, "DEC"), na.rm = TRUE), 3),
  data_inicio = min(ufc_lutas_completo$date, na.rm = TRUE),
  data_fim = max(ufc_lutas_completo$date, na.rm = TRUE)
)

eventos_por_ano <- eventos_unicos %>%
  mutate(ano = year(date)) %>%
  count(ano, name = "total_eventos")

evolucao_anual <- ufc_lutas_completo %>%
  mutate(ano = year(date)) %>%
  group_by(ano) %>%
  summarise(
    total_lutas = n(),
    lutas_femininas = sum(gender == "FEMALE", na.rm = TRUE),
    lutas_masculinas = sum(gender == "MALE", na.rm = TRUE),
    pct_ko = round(mean(finish == "KO/TKO", na.rm = TRUE), 3),
    pct_sub = round(mean(finish == "SUB", na.rm = TRUE), 3),
    pct_dec = round(mean(str_detect(finish, "DEC"), na.rm = TRUE), 3),
    tempo_medio_seg = round(mean(total_fight_time_secs, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  left_join(eventos_por_ano, by = "ano") %>%
  arrange(ano)

dir.create("dados_limpos", showWarnings = FALSE)

# 12. EXPORTAÇÃO DE TODOS OS ARQUIVOS .RDS

objetos <- list(
  kpis_gerais = kpis_gerais,
  evolucao_anual = evolucao_anual,
  atletas_limpo = atletas_limpo,
  rankings_limpo = rankings_limpo,
  rankings_atual = rankings_atual,
  ranking_longevidade = ranking_longevidade,
  comparador_atletas = comparador_atletas,
  comparacao_categorias = comparacao_categorias,
  raio_x_finalizacoes = raio_x_finalizacoes,
  metodos_por_ano = metodos_por_ano,
  lutas_cinturao = lutas_cinturao,
  disputadores_cinturao = disputadores_cinturao,
  locais_mais_eventos = locais_mais_eventos,
  paises_mais_eventos = paises_mais_eventos,
  ufc_lutas_completo = ufc_lutas_completo,
  lutas_eventos_limpo = lutas_eventos_limpo
)

dir.create("dados_limpos", showWarnings = FALSE, recursive = TRUE)

iwalk(objetos, ~ saveRDS(.x, file.path("dados_limpos", paste0(.y, ".rds"))))

message("Limpeza concluída! ", length(objetos), " arquivos salvos em 'dados_limpos/'.")

getwd()
list.files("dados_limpos")
