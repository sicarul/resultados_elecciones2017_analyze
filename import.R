library(dplyr)
library(jsonlite)
library(tidyjson)

inputJsonLines = readLines("resultados.json")
out <- lapply(inputJsonLines, fromJSON, simplifyDataFrame=F)

convertToMesas = function(row){
  new = data.frame(
    provincia=row$provincia_name,
    seccion=row$seccion_name,
    circuito=row$circuito,
    mesa=row$mesa,
    estado=row$estado,
    impugnados=row$especiales$impugnados,
    nulos_concejales=ifelse(!is.null(row$especiales$nulo$concejales), row$especiales$nulo$concejales, NA),
    nulos_senadores_nacionales=ifelse(!is.null(row$especiales$nulo$senadores_nacionales), row$especiales$nulo$senadores_nacionales, NA),
    nulos_senadores_provinciales=ifelse(!is.null(row$especiales$nulo$senadores_provinciales), row$especiales$nulo$senadores_provinciales, NA),
    nulos_diputados_nacionales=ifelse(!is.null(row$especiales$nulo$diputados_nacionales), row$especiales$nulo$diputados_nacionales, NA),
    nulos_diputados_provinciales=ifelse(!is.null(row$especiales$nulo$diputados_provinciales), row$especiales$nulo$diputados_provinciales, NA),
    
    blanco_concejales=ifelse(!is.null(row$especiales$blanco$concejales), row$especiales$blanco$concejales, NA),
    blanco_senadores_nacionales=ifelse(!is.null(row$especiales$blanco$senadores_nacionales), row$especiales$blanco$senadores_nacionales, NA),
    blanco_senadores_provinciales=ifelse(!is.null(row$especiales$blanco$senadores_provinciales), row$especiales$blanco$senadores_provinciales, NA),
    blanco_diputados_nacionales=ifelse(!is.null(row$especiales$blanco$diputados_nacionales), row$especiales$blanco$diputados_nacionales, NA),
    blanco_diputados_provinciales=ifelse(!is.null(row$especiales$blanco$diputados_provinciales), row$especiales$blanco$diputados_provinciales, NA),
    
    recurridos_concejales=ifelse(!is.null(row$especiales$recurridos$concejales), row$especiales$recurridos$concejales, NA),
    recurridos_senadores_nacionales=ifelse(!is.null(row$especiales$recurridos$senadores_nacionales), row$especiales$recurridos$senadores_nacionales, NA),
    recurridos_senadores_provinciales=ifelse(!is.null(row$especiales$recurridos$senadores_provinciales), row$especiales$recurridos$senadores_provinciales, NA),
    recurridos_diputados_nacionales=ifelse(!is.null(row$especiales$recurridos$diputados_nacionales), row$especiales$recurridos$diputados_nacionales, NA),
    recurridos_diputados_provinciales=ifelse(!is.null(row$especiales$recurridos$diputados_provinciales), row$especiales$recurridos$diputados_provinciales, NA),
    stringsAsFactors = F
  )
  
  return(new)
}

mesas = do.call(rbind, lapply(out, convertToMesas)) %>% arrange(provincia, seccion, circuito, mesa)
mesas$mesa_id = rownames(mesas)
mapping = select(mesas, provincia, seccion, circuito, mesa, mesa_id)

jsonLines = tbl_json(data.frame(id=seq(1:length(out))), out)

votos = jsonLines %>%
  spread_values(
    provincia=jstring('provincia_name'),
    seccion=jstring('seccion_name'),
    circuito=jstring('circuito'),
    mesa=jstring('mesa'),
    estado=jstring('estado')
    ) %>%
  enter_object('votos') %>%
  gather_array %>%
  spread_values(
    partido=jstring('partido'),
    lista=jstring('lista'),
    diputados_nacionales=jstring('diputados_nacionales'),
    diputados_provinciales=jstring('diputados_provinciales'),
    senadores_nacionales=jstring('senadores_nacionales'),
    senadores_provinciales=jstring('senadores_provinciales'),
    concejales=jstring('concejales')
    ) %>%
  select(-id, -array.index) %>%
  left_join(mapping, by=c('provincia', 'seccion', 'circuito', 'mesa'))


save(mesas, votos, file='resultados.rda')
