# Cargar el paquete necesario
library(tidyr)
library(dplyr)  # Para manipulación de datos
library(rio)

##### tituos repetidos ####
# cargamos bases de datos  
tabla <-  rio::import("wca.csv") # busqueda saul 
tabla1 <- rio::import("regeneracion_501_1000.txt")
tabla2 <- rio::import("regeneracion_1001_1500.txt")
tabla3 <- rio::import("regeneracion_1501_1820.txt")

wos_tabla <- rbind(tabla1, tabla2, tabla3)

write.table(wos_tabla, "wos_table.txt", row.names = FALSE, quote = FALSE, sep = "\t")

# este paso es necesario porque wca esta en csv y mo em formato txt UTF-8
tabla$Title <- iconv(tabla$Title, from = "latin1", to = "UTF-8", sub = "byte")

# convertimos todo a minisculas
wos_tabla$TI <- tolower(wos_tabla$TI)
tabla$Title <- tolower(tabla$Title)

# los titulos se usaran como filtro, ahora todos entan en minusculas,
# esto es necesario ya que R toma como cosas diferentes las minusculas y mayusculas
head(tabla$Title)
head(wos_tabla$TI)

#  vemos si no hay titulos repetidos dentro de a base de datos
# esto si se tiene que hacer por titulo porque hay doi faltantes
reps <- tabla %>% 
  count(Title, sort = T) %>% 
  filter (n>1)

rep_wos <- wos_tabla %>% 
  count(TI, sort = T) %>% 
  filter (n>1)

# vamos a filtrar por titulo y por el doi
repetidos <- tabla[tabla$Title %in% wos_tabla$TI,]
dim(repetidos) # 386 repetidos 
write.table(repetidos, "repetidos.txt")

# vamos a filtrar por el doi
repetidos_doi <- tabla[tabla$DOI %in% wos_tabla$DI,]
dim(repetidos_doi) # 705 repetidos

# existen filas sin doy, ahora veamos los titulos, esos no estan vacios
doi_titulos <- repetidos_doi[repetidos_doi$Title %in% repetidos$Title,]
dim(doi_titulos) # 375 titulos repetidos, usar el doi sobrestima los repetidos y no es util para comparar repetidos entre bases de datos

write.table(repetidos, "repetidos.txt")
#
##### sinonimos ####
# Separar las palabras clave en múltiples filas

colnames(tabla)[colnames(tabla) == "Author Keywords"] <- "Author_Keywords"

tabla_separado <- tabla %>%
  separate_rows(Author_Keywords, sep = ";")

tabla_separado$Author_Keywords <- iconv(tabla_separado$Author_Keywords, from = "latin1", to = "UTF-8", sub = "")
tabla_separado$Author_Keywords <- tolower(tabla_separado$Author_Keywords)



nodeseado <- c("meta-analisys", "review", "mangrove", "socio-econimis", "industry", "politics", "economi",
               "policy", "Economic analysis", "Economic aspects", "Stakeholder", "Aerial forager", " Financial performance",
               " Transaction costs", " Clonal diversity", "clonal",  "Forest products", " Rights-based approach", " Socio-ecological analysis",
               " economic analysis", " Community participation", " Forest Policy", " Smallholder farmers", " Urban landscape", "Urban", "urban" )

si_deseado <- c("germination", " Functional diversity", "seed", " Seed germination")

t_no <- tabla_separado[tabla_separado$Author.Keywords %in% nodeseado,]
