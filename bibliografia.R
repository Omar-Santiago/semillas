# Cargar el paquete necesario
library(tidyr)
library(dplyr)  # Para manipulación de datos
library(rio)
library(stringdist)
library(usethis)
library(devtools)
library(clustringr)
library(stringr)

#
##### tituos repetidos ####
# cargamos bases de datos  
tabla <-  rio::import("wca.txt") # busqueda saul 
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


# eliminamos duplicados en la misma base de datos
#despues checamos que no existan duplicados 
# esto si se tiene que hacer por titulo porque hay doi faltantes

tabla <- tabla %>%
  distinct(Title, .keep_all = TRUE) 
reps <- tabla %>% 
  count(Title, sort = T) %>% 
  filter (n>1)

wos_tabla <- wos_tabla %>%
  distinct(TI, .keep_all = TRUE) 
rep_wos <- wos_tabla %>% 
  count(TI, sort = T) %>% 
  filter (n>1)

# vamos a filtrar por titulo y por el doi
repetidos <- tabla[tabla$Title %in% wos_tabla$TI,]
dim(repetidos) # 386 repetidos 

# vamos a filtrar por el doi
repetidos_doi <- tabla[tabla$DOI %in% wos_tabla$DI,]
dim(repetidos_doi) # 705 repetidos

# existen filas sin doy, ahora veamos los titulos, esos no estan vacios
doi_titulos <- repetidos_doi[repetidos_doi$Title %in% repetidos$Title,]
dim(doi_titulos) # 375 titulos repetidos, usar el doi sobrestima los repetidos y no es util para comparar repetidos entre bases de datos

write.table(repetidos, "repetidos.txt")
#

# ahora vamos a unir las dos bases de datos pero sin los titulos repetidos
# usaremos el formato de scopus 
scopus_unir <- tabla[c(5,6,15,19,20,21)]
scopus_dos <- scopus_unir[!scopus_unir$Title %in% repetidos$Title,]

wos_unir <- wos_tabla[c(9,47,57,22,20,21)]
colnames(wos_unir) <- colnames(scopus_dos)

bases_datos <- rbind(scopus_dos, wos_unir)
write.table(bases_datos, "bases_datos.txt")
#
##### sinonimos ####

tabla <-  rio::import("bases_datos.txt") # busqueda saul 

# Separar las palabras clave en múltiples filas
colnames(tabla)[colnames(tabla) == "Author Keywords"] <- "Author_Keywords"

# ordenamos por orden alfabetico los titulos
tabla_ordenada <- tabla %>%
  arrange(Title)

# separamos las palabras clave del autor
tabla_separado <- tabla_ordenada %>%
  separate_rows(Author_Keywords, sep = ";")

# Eliminar espacios en blanco antes y después de cada palabra
tabla_separado$Author_Keywords <- str_trim(tabla_separado$Author_Keywords)

# Eliminar caracteres no alfanuméricos o extraños
tabla_separado$Author_Keywords <- gsub("[^[:alnum:][:space:][:punct:]]", "", tabla_separado$Author_Keywords)

# Convertir a minúsculas
tabla_separado$Author_Keywords <- tolower(tabla_separado$Author_Keywords)

# Verificar los resultados
head(tabla_separado$Author_Keywords)

# la frecuencia mayor a 13 es lo que seleccionamos en el campo de minimo numero de ocurrencias 
frec_14 <- tabla_separado %>% count(tabla_separado$Author_Keywords) %>% filter(n > 13)

distance_matrix <- stringdistmatrix(frec_14$`tabla_separado$Author_Keywords`, useNames=TRUE, method = "dl")

# Perform hierarchical clustering
hierarchical_clusters <- hclust(as.dist(distance_matrix))

# Cut dendrogram to obtain clusters
num_clusters <- 20
cluster_labels <- cutree(hierarchical_clusters, k = num_clusters)

# Asignar las palabras a sus respectivos clústeres
clustered_words <- data.frame(
  Word = frec_14$`tabla_separado$Author_Keywords`,  # Las palabras clave
  Cluster = cluster_labels  # Clúster asignado a cada palabra
)

# Ver los resultados
head(clustered_words)

# Visualize dendrogram
# apartir de esto podemos contruir nuestro theasure 
plot(hierarchical_clusters, labels = frec_14$`tabla_separado$Author_Keywords`)

# ahora que tenemos el dendograma podemos ver las palabras mas similares,
# aca es impotante checar y decidir que palabras son las que queremos juntar dentro de una palabra
# este proceso es igual a generar al theasure
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("boreal forest")] <- "temperate forest"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("tropical forests")] <- "tropical forest"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("clearcutting", "salvage logging", "selective logging", "thinning", "shelterwood")] <- "logging"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("afforestation")] <- "reforestation"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("seedlings", "saplings", "seeding")] <- "seedling"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("wildfire", "fire severity", "forest fire")] <- "fire"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("forestry")] <- "silviculture"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("biodiversity")] <- "diversity"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("secoundary succession", "forest succession")] <- "succession"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("microsatellite markers")] <- "microsatellites"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("sustainable forest management")] <- "sustainable management"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("stand structure")] <- "forest structure"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("disturbances")] <- "disturbance"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("prescribed burning")] <- "prescribed fire"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("biodiversity conservation")] <- "conservation"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("tree regeneration", "regeneration")] <- "natural regeneration"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("oak regeneration")] <- "oak"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("white spruce")] <- "picea glauca"

# ahora viene un paso crucial

# ahora bajaremos la frecuencia de las palabras para ser analizadas, las bajaremos hasta 12
frec_11 <- tabla_separado %>% count(tabla_separado$Author_Keywords) %>% filter(n > 11)
distance_matrix <- stringdistmatrix(frec_11$`tabla_separado$Author_Keywords`, useNames=TRUE, method = "dl")
hierarchical_clusters <- hclust(as.dist(distance_matrix))

# podemos hacer un procesi iterativo ente cambiar las palabras en el theasure y volver a correr el analis de clusterizacion para 
# agregar mas palabras, despues volver a visualizar el dendograma y finalmente cambiar el nombre de las palabras similares
plot(hierarchical_clusters, labels = frec_11$`tabla_separado$Author_Keywords`)

# Volver a juntar las palabras en una sola fila por cada grupo
tabla_reunida <- tabla_separado %>%
  group_by(V1) %>%  # Supongamos que tienes una columna ID que identifica cada grupo original
  summarise(Author_Keywords = str_c(Author_Keywords, collapse = "; "))  # Juntar las palabras con un delimitador

# ahora vamos a ordenar tabla_ordenada por V1
tabla_ordenada <- tabla_ordenada %>%
  arrange(tabla_ordenada$V1)

# agregamos las palabras clave a una base de datos que queremos exportar
tabla_ordenada$Author_Keywords <- tabla_reunida$Author_Keywords
colnames(tabla_ordenada)[colnames(tabla_ordenada) == "Author_Keywords"] <- "Author Keywords"

# esta base de datos solo se guardar hasta que estemos satisfechos con los resultados del dendograma y el theasure
write.csv(tabla_ordenada, "bases_datos_limpio1.csv")

##### articulos que no nos interesan ####

# podemos revisar toda la base de datos, pero en verdad solo algunos articulos apoartan al analisis, 
# por lo que seria bueno ver que articulos aportan y dentro de esos cuales seria necesario eliminar

# separamos aquellos registros donde si esten las palabras clave del theasure
art_si <-  tabla_separado[tabla_separado$Author_Keywords %in% frec_11$`tabla_separado$Author_Keywords`,]

# unimos las palabras clave
art_si_reunida <- art_si %>%
  group_by(V1) %>%  # Supongamos que tienes una columna ID que identifica cada grupo original
  summarise(Author_Keywords = str_c(Author_Keywords, collapse = "; "))  # Juntar las palabras con un delimitador

#separamos de tabla_ordenada los registrso que si estan en art_si
art_cont <- tabla_ordenada[tabla_ordenada$V1 %in% art_si_reunida$V1,]
colnames(art_cont)[colnames(art_cont) == "Author_Keywords"] <- "Author Keywords"

# Filtrar las filas donde la columna 'columna_vacia' está vacía (cadena vacía)
art_cont_vacio <- art_cont %>% 
  filter(art_cont$`Author Keywords` == "")

# obtenemos una base de datos donde solo haya registros con palabras clave del autor
art_cont_bueno <- art_cont[!art_cont$V1 %in% art_cont_vacio$V1,]

# guardamos y vemos si nos da la misma señal 
write.csv(art_cont_bueno, "art_cont.csv")

#

##### extra ####

frec_13 <- tabla_separado %>% count(tabla_separado$Author_Keywords) %>% filter(n == 13)
#

as.dist(distance_matrix)
ds <- as.matrix(distance_matrix)

nueve <- as.data.frame(rownames(ds))

nueve$grupo10 <- apply(ds, 1, function(x) {
  # Encontrar el valor más cercano a 1 para cada fila (evitar valores 0)
  min_val <- which.min(abs(x - 1))  # Indice del valor más cercano a 1
  # Verifica si el valor más cercano es cero
  if (x[min_val] == 0) {
    return(NA)  # Si el valor más cercano es 0 (coincidencia exacta de la misma palabra), lo consideramos como NA
  } else {
    return(colnames(ds)[min_val])  # Devuelve el nombre de la columna correspondiente
  }
})

# Ahora, vamos a agregar columnas con los valores ordenados de menor a mayor:
nueve$sorted_values <- apply(ds, 1, function(x) {
  sorted_indices <- order(x)  # Obtener los índices ordenados de menor a mayor
  return(paste(colnames(ds)[sorted_indices], collapse = ", "))  # Crear una cadena con los nombres de las columnas ordenadas
})



# Verificar los primeros resultados para depuración
head(nueve)


ET_Cl <- data.frame(
  Keyword = frec_14$`tabla_separado$Author_Keywords`,
  Column = colnames(ds)[apply(ds, 1, function(x) {
    x[x == 0] <- NA  # Reemplazar los ceros por NA
    which.min(abs(x - 1))  # Encontrar el valor más cercano a 1
  })]
)

# Perform hierarchical clustering
hierarchical_clusters <- hclust(as.dist(distance_matrix))

# Cut dendrogram to obtain clusters
num_clusters <- 10
cluster_labels <- cutree(hierarchical_clusters, k = num_clusters)

# Visualize dendrogram
plot(hierarchical_clusters, labels = frec_14$`tabla_separado$Author_Keywords`)

nodeseado <- c("meta-analisys", "review", "mangrove", "socio-econimis", "industry", "politics", "economi",
               "policy", "Economic analysis", "Economic aspects", "Stakeholder", "Aerial forager", " Financial performance",
               " Transaction costs", " Clonal diversity", "clonal",  "Forest products", " Rights-based approach", " Socio-ecological analysis",
               " economic analysis", " Community participation", " Forest Policy", " Smallholder farmers", " Urban landscape", "Urban", "urban" )

si_deseado <- c("germination", " Functional diversity", "seed", " Seed germination")

t_no <- tabla_separado[tabla_separado$Author.Keywords %in% nodeseado,]

######### 
