## Executa as bibliotecas =====

purrr::map(c('dplyr', 'stringr', 'tidyr', 'sf', 'ggplot2', 'readr'), library, character.only = TRUE)

## -----

## Lê a base de dados de votação 2022 e eleitorado =====

"data\\votacao_secao_2022_BR.csv" %>% 
  readr::read_csv2(locale = locale(encoding = 'Latin1')) %>% 
  filter(NR_TURNO == 2) %>% 
  select(ANO_ELEICAO, NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL, QT_VOTOS) %>%
  select(-c(ANO_ELEICAO, NR_TURNO)) %>% 
  group_by(NM_VOTAVEL, NM_MUNICIPIO, CD_MUNICIPIO, SG_UF) %>% 
  summarise(QT_VOTOS = sum(QT_VOTOS)) %>% 
  reshape2::dcast(formula = SG_UF + CD_MUNICIPIO + NM_MUNICIPIO ~ NM_VOTAVEL, value.var = 'QT_VOTOS') %>% 
  magrittr::set_colnames(c('uf', 'cd_mun', 'nm_mun', 'jair', 'lula', 'voto_branco', 'voto_nulo')) -> votacao2022

votacao2022 %>% mutate(jair_vence = ifelse(lula < jair, 1, 0),
                          lula_vence = ifelse(lula > jair, 1, 0),
                          empate = ifelse(lula == jair, -1, 0)) -> votacao2022

"data\\perfil_eleitorado_2022.csv" %>%
  readr::read_csv2(locale = locale(encoding = 'Latin1')) -> eleitorado2022

## -----

## Lê base de dados que unifica o código dos municípios // Coletado do site Base de Dados (https://basedosdados.org/) ======

munIBGE <- readr::read_csv('data\\bq-results-20231013-135949-1697205618301.csv')

## -----

## Lê os shapefiles ======

shp <- read_sf('data\\shapefile\\BR_Municipios_2022\\BR_Municipios_2022.shp')
shpUF <- read_sf('data\\shapefile\\BR_UF_2022\\BR_UF_2022.shp')

## -----

## Remodela base de dados de votações e une com as tabelas de unificação de municípios e shapefiles ======

votacao2022 <- votacao2022 %>% 
  left_join(munIBGE, by = c('cd_mun' = 'id_municipio_tse')) %>% 
  select(c(1:11, 31)) %>% 
  mutate(across(4:7, \(x) tidyr::replace_na(data = x, replace = 0))) %>% 
  mutate(total = lula + jair,
         lula_bolso = lula/total,
         bolso_lula = jair/total)

votacao2022 <- votacao2022 %>% mutate(maior_60 = if_else((lula_bolso <= .6 & lula_bolso >= .5) | (bolso_lula <= .6 & bolso_lula >= .5), TRUE, FALSE))

votacao2022 %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  left_join(shp, by = c('id_municipio' = 'CD_MUN')) -> dfMunMap

dfMunMap %>% 
  mutate(jair_vence = ifelse(lula < jair, 1, 0),
         lula_vence = ifelse(lula > jair, 1, 0),
         empate = ifelse(lula == jair, -1, 0)) -> dfMunMap

dfMunMap %>% mutate(voto_branco_percentage = voto_branco/(jair + lula + voto_branco + voto_nulo),
                    voto_nulo_percentage = voto_nulo/(jair + lula + voto_branco + voto_nulo),
                    branco_nulo_percentage = voto_branco_percentage + voto_nulo_percentage) -> dfMunMap

votacao2022 %>% 
  filter(uf != 'ZZ') %>%
  mutate(cd_mun = cd_mun %>% as.character()) %>% 
  group_by(uf, cd_mun, nm_mun) %>% 
  summarise(jair = sum(jair), lula = sum(lula)) %>% 
  left_join(
    munIBGE %>% mutate(id_municipio_tse = id_municipio_tse %>% as.character(),
                       id_municipio = id_municipio %>% as.character()), by = c('cd_mun' = 'id_municipio_tse')
  ) %>% 
  left_join(
    shp %>% mutate(CD_MUN = CD_MUN %>% as.character()), by = c('id_municipio' = 'CD_MUN')
  ) %>% 
  mutate(lula_venceu = if_else(lula > jair, 1, -1)) -> citiesMap

votacao2022 %>% 
  filter(uf != 'ZZ') %>%
  mutate(cd_mun = cd_mun %>% as.character()) %>% 
  group_by(uf) %>% 
  summarise(jair = sum(jair), lula = sum(lula)) %>% 
  left_join(
    shpUF, by = c('uf' = 'SIGLA_UF')
  ) %>% 
  mutate(lula_venceu = if_else(lula > jair, 1, -1)) -> stateMap

## ----

## Dados gerais da votação ======

votacao2022$lula %>% sum(na.rm = TRUE) ## Número de votos do Lula
votacao2022$lula %>% sum(na.rm = TRUE)/sum(votacao2022$lula %>% sum(na.rm = TRUE), votacao2022$jair %>% sum(na.rm = TRUE)) ## Porcentagem de votos do Lula

votacao2022$jair %>% sum(na.rm = TRUE) ## Número de votos do Bolsonaro
votacao2022$jair %>% sum(na.rm = TRUE)/sum(votacao2022$lula %>% sum(na.rm = TRUE), votacao2022$jair %>% sum(na.rm = TRUE)) ## Porcentagem de votos do Bolsonaro

votacao2022 %>% filter(maior_60 == TRUE) %>% nrow() / votacao2022 %>% nrow()

eleitorado2022 %>% 
  group_by(SG_UF) %>% 
  summarise(qt_eleitores = sum(QT_ELEITORES_PERFIL)) %>% 
  arrange(desc(qt_eleitores)) ## Quantidade de eleitores por UF em 2022

votacao2022 %>% 
  filter(maior_60 == TRUE) %>%
  select(uf) %>% 
  table() %>%
  as.data.frame() %>%
  left_join(
    munIBGE %>% group_by(sigla_uf) %>% count() %>% as.data.frame(), by = c('uf' = 'sigla_uf')
    ) %>% mutate(percentage = Freq/n) %>% 
  arrange(desc(percentage)) ## Diferença menor do que 10% entre os candidatos

## -----

## Plotagem dos gráficos ======

## Gráfico de votos do Lula
ggplot() +
  geom_sf(data = dfMunMap %>% st_as_sf(), aes(fill = lula_vence), colour = alpha(colour = '#d0d0d0', 0.001), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  scale_fill_gradientn(colours = c('#e81316', '#c21013', '#7d0507')) +
  labs(
    fill = 'Votos (%)'
  )

## Gráfico de votos do Bolsonaro
ggplot() +
  geom_sf(data = dfMunMap %>% st_as_sf(), aes(fill = jair_vence), colour = alpha(colour = '#d0d0d0', 0.02), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = 'none') +
  scale_fill_gradientn(colours = c('#1856cb', '#114097', '#081f4a')) +
  labs(
    fill = 'Votos (%)'
  )

## Votação em São Paulo
ggplot() +
  geom_sf(data = citiesMap %>% filter(uf == 'SP') %>% st_as_sf(), aes(fill = lula_venceu), colour = alpha(colour = '#d0d0d0', 0.01), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low = '#071e49', high = '#7c0406') +
  labs(
    fill = 'Venceu?'
  )

## Votação no Rio de Janeiro
ggplot() +
  geom_sf(data = citiesMap %>% filter(uf == 'RJ') %>% st_as_sf(), aes(fill = lula_venceu), colour = alpha(colour = '#d0d0d0', 0.01), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low = '#071e49', high = '#7c0406') +
  labs(
    fill = 'Venceu?'
  )

## Votação em Minas Gerais
ggplot() +
  geom_sf(data = citiesMap %>% filter(uf == 'MG') %>% st_as_sf(), aes(fill = lula_venceu), colour = alpha(colour = '#d0d0d0', 0.01), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low = '#071e49', high = '#7c0406')

## Votação dos candidatos no país (ganhou ou perdeu?)
ggplot() +
  geom_sf(data = stateMap %>% st_as_sf(), aes(fill = lula_venceu), colour = alpha(colour = '#d0d0d0', 0.1), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low = '#071e49', high = '#7c0406')


ggplot() +
  geom_sf(data = dfMunMap %>% st_as_sf(), aes(fill = branco_nulo_percentage), colour = alpha(colour = '#d0d0d0', 0.01), linewidth = 0) + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = 'none') +
  scale_fill_gradient(low = '#dcdcdc', high = '#5c5c5c')

## -----