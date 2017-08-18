library(funModeling)
library(dplyr)
load('resultados.rda')

group_by(mesas, estado) %>%
  count()

votos = votos %>% 
  mutate(
    diputados_nacionales=as.numeric(diputados_nacionales),
    diputados_provinciales=as.numeric(diputados_provinciales),
    senadores_nacionales=as.numeric(senadores_nacionales),
    senadores_provinciales=as.numeric(senadores_provinciales),
    concejales=as.numeric(concejales)
  )

filter(mesas, estado != 'Grabada') %>% count()

totalVotosByMesa = filter(votos, estado == 'Grabada' & as.numeric(mesa) < 9000) %>%
  select(-estado, -lista, -partido) %>%
  group_by(provincia, seccion, circuito, mesa, mesa_id) %>%
  summarize_all(funs(sum=sum)) %>%
  ungroup()

lowcount = filter(totalVotosByMesa, diputados_nacionales_sum < 20)

summedByLista = filter(votos, estado == 'Grabada') %>%
  anti_join(lowcount, by='mesa_id') %>%
  select(-estado, -lista) %>%
  group_by(provincia, seccion, circuito, partido, mesa, mesa_id) %>%
  summarize_all(funs(sum)) %>%
  ungroup() %>%
  inner_join(totalVotosByMesa, by=c('provincia', 'seccion', 'circuito', 'mesa', 'mesa_id')) %>%
  mutate(
    diputados_nacionales=ifelse(diputados_nacionales_sum==0, 0, diputados_nacionales/diputados_nacionales_sum),
    diputados_provinciales=ifelse(diputados_provinciales_sum==0, 0, diputados_provinciales/diputados_provinciales_sum),
    senadores_nacionales=ifelse(senadores_nacionales_sum==0, 0, senadores_nacionales/senadores_nacionales_sum),
    senadores_provinciales=ifelse(senadores_provinciales_sum==0, 0, senadores_provinciales/senadores_provinciales_sum),
    concejales=ifelse(concejales_sum==0, 0, concejales/concejales_sum)
  ) %>%
  select(-ends_with('_sum'))

stats = summedByLista %>%
  select(-mesa, -mesa_id) %>%
  group_by(provincia, seccion, circuito, partido) %>%
  summarize_all(funs(
    mean(., na.rm=T),
    median(., na.rm=T),
    sd(., na.rm=T),
    qmin=quantile(., probs=0.005, na.rm=T),
    qmax=quantile(., probs=0.995, na.rm=T),
    hout_btm=hampel_outlier(., 2)[['bottom_threshold']],
    hout_top=hampel_outlier(., 2)[['top_threshold']]
    )) %>%
  ungroup()

joinedStats = inner_join(summedByLista, stats, by=c('provincia', 'seccion', 'circuito', 'partido'))


## SD outlier detection
outliers_diputados_nacionales = joinedStats %>%
  filter(!is.na(diputados_nacionales)
         & (diputados_nacionales < diputados_nacionales_mean-diputados_nacionales_sd*3 |
              diputados_nacionales > diputados_nacionales_mean+diputados_nacionales_sd*3)
  ) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  mutate(razon=ifelse(diputados_nacionales_mean-diputados_nacionales_sd*3, 'Menos de lo esperado', 'Mas de lo esperado')) %>%
  select(provincia, seccion, circuito, partido, mesa, razon, starts_with('diputados_nacionales'))

## Hampel outlier detection
outliers_diputados_nacionales_hampel = joinedStats %>%
  filter(!is.na(diputados_nacionales)
         & (diputados_nacionales+0.05 < diputados_nacionales_hout_btm |
              diputados_nacionales-0.05 > diputados_nacionales_hout_top)
  ) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  mutate(razon=ifelse(diputados_nacionales < diputados_nacionales_hout_btm, 'Menos de lo esperado', 'Mas de lo esperado')) %>%
  select(provincia, seccion, circuito, partido, mesa, razon, starts_with('diputados_nacionales'))

sum_outliers_dn = group_by(outliers_diputados_nacionales, provincia, seccion, partido, razon) %>%
  count() %>%
  arrange(desc(n))

resumen_diputadosnacionales = outliers_diputados_nacionales %>%
  group_by(provincia, partido, razon) %>%
  count() %>%
  filter(n>10) %>%
  arrange(razon, provincia, desc(n))

provinciasMasAnomalias = group_by(resumen_diputadosnacionales, provincia) %>% dplyr::summarize(n=sum(n)) %>% arrange(desc(n)) %>% head(6)


paragrafico = ungroup(resumen_diputadosnacionales) %>% mutate(
                     partido = case_when(
                       grepl('CAMBIEMOS', partido) | grepl('VAMOS JUNTOS', partido) ~ 'CAMBIEMOS',
                       grepl('UNIDAD CIUDADANA', partido) ~ 'UNIDAD CIUDADANA',
                       grepl('JUSTICIALISTA', partido) ~ 'FRENTE JUSTICIALISTA',
                       grepl('FRENTE PARA LA VICTORIA', partido) | grepl('FRENTE DE LA VICTORIA', partido) ~ 'FRENTE PARA LA VICTORIA',
                       grepl('1PAIS', partido) ~ '1PAIS',
                       grepl('1 PAIS', partido) ~ '1PAIS',
                       grepl('FRENTE DE IZQUIERDA', partido) ~ 'FIT',
                       grepl('UNION POR CORDOBA', partido) ~ 'UNION X CORDOBA',
                       grepl('IZQUIERDA AL FRENTE', partido) ~ 'IZQ. AL FRENTE',
                       TRUE ~ 'OTROS'
                     ),
                     provincia=ifelse(provincia %in% provinciasMasAnomalias$provincia, provincia, 'Otras')
) %>% group_by(partido, provincia, razon) %>% dplyr::summarize(n=sum(n)) %>% arrange(desc(n))

totalGrafico = group_by(paragrafico, partido) %>% dplyr::summarize(n=sum(n))
newlevels=levels(reorder(totalGrafico$partido, -totalGrafico$n))
paragrafico$partido = factor(paragrafico$partido, levels=newlevels)

totalGrafico = group_by(paragrafico, provincia) %>% dplyr::summarize(n=sum(n))
newlevels=levels(reorder(totalGrafico$provincia, -totalGrafico$n))
paragrafico$provincia = factor(paragrafico$provincia, levels=newlevels)


ggplot(paragrafico, aes(partido, n)) +
  geom_col(aes(fill=provincia)) +
  coord_flip() +
 facet_wrap(~razon)

paragrafico2 = ungroup(resumen_diputadosnacionales) %>% mutate(
  partido2 = case_when(
    grepl('CAMBIEMOS', partido) | grepl('VAMOS JUNTOS', partido) ~ 'CAMBIEMOS',
    grepl('UNIDAD CIUDADANA', partido) ~ 'UNIDAD CIUDADANA',
    grepl('JUSTICIALISTA', partido) ~ 'FRENTE JUSTICIALISTA',
    grepl('FRENTE PARA LA VICTORIA', partido) | grepl('FRENTE DE LA VICTORIA', partido) ~ 'FRENTE PARA LA VICTORIA',
    grepl('1PAIS', partido) ~ '1PAIS',
    grepl('1 PAIS', partido) ~ '1PAIS',
    grepl('FRENTE DE IZQUIERDA', partido) ~ 'FIT',
    grepl('UNION POR CORDOBA', partido) ~ 'UNION X CORDOBA',
    grepl('IZQUIERDA AL FRENTE', partido) ~ 'IZQ. AL FRENTE',
    TRUE ~ 'OTROS'
  )
) %>% filter(partido2=='OTROS' & razon == 'Mas de lo esperado')

totalGrafico = group_by(paragrafico2, partido) %>% dplyr::summarize(n=sum(n))
newlevels=levels(reorder(totalGrafico$partido, -totalGrafico$n))
paragrafico2$partido = factor(paragrafico2$partido, levels=newlevels)

ggplot(paragrafico2, aes(partido, n)) +
  geom_col(aes(fill=provincia)) +
  coord_flip()
