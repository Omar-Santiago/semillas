######## paquetes ########
library(tidyr)
library(dplyr)  # Para manipulación de datos
library(rio)
library(stringdist)
library(stringr)
library(bibliometrix)

library(ggthemes)
library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(scatterpie)

library("ggspatial")
library("sf")
library("tmap")
library("rnaturalearth")
library("rnaturalearthdata")

rm(list = ls())

#
######### tituos repetidos ####
# cargamos la bases de datos de scopus 
tabla <-  rio::import("wca_crudo.csv")

# convertimos todo a minisculas
tabla$Title <- tolower(tabla$Title)
tabla$Link <- tolower(tabla$Link)


# los titulos se usaran como filtro, ahora todos entan en minusculas,
# esto es necesario ya que R toma como cosas diferentes las minusculas y mayusculas
head(tabla$Title)
head(tabla$Link)

# eliminamos duplicados en la misma base de datos
# despues checamos que no existan duplicados 
# esto si se tiene que hacer por titulo o por la columna de Link

tabla <- tabla %>%
  distinct(Title, .keep_all = TRUE) 
reps <- tabla %>% 
  count(Title, sort = T) %>% 
  filter (n>1)

# lo hacemos por doi 
# tabla <- tabla %>%
#  distinct(DOI, .keep_all = TRUE) 
reps <- tabla %>% 
  count(Link, sort = T) %>% 
  filter (n>1)

######### threasure ######

# subimos las bases con las palabras que conformaran el teashuere
# estas pañabras fueron obtenidad de una primer vista del sofware voswiev
# de forma manual se realizaron los cambios oara combinar palabras con significado o topografia similar
# en esta parte nos dedicaremos a cambiar las palabras a lo largo de la base de datos

# partiremos de la tabla filtrada
View(tabla)

# cargamos el threasure y lo preparamos 
theasure <- rio::import("keywords_10_coocurrencias.xlsx")
# Eliminar espacios en blanco antes y después de cada palabra
theasure$remplazar <- str_trim(theasure$remplazar)
# Eliminar caracteres no alfanuméricos o extraños
theasure$remplazar <- gsub("[^[:alnum:][:space:][:punct:]]", "", theasure$remplazar)
# Convertir a minúsculas
theasure$remplazar <- tolower(theasure$remplazar)


# preparamos la base de datos
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

# cambiamos lo nombres 
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("biodiversity conservation")] <- "conservation"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("disturbances")] <- "disturbance"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("european beech")] <- "fagus sylvatica"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("fire")] <- "forest fire"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("tree regeneration", "regeneration")] <- "natural regeneration"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("restoration")] <- "forest restoration"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("stand structure")] <- "forest structure"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("microsatellite markers")] <- "microsatellites"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("flowering")] <- "phenology"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("norway spruce")] <- "picea abies"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("plantation")] <- "planting"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("seeding")] <- "planting"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("prescribed fire")] <- "prescribed burning"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("oak regeneration")] <- "quercus sp."
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("quercus")] <- "quercus sp."
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("soil seed bank")] <- "seed bank"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("germination")] <- "seed germination"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("recruitment")] <- "seedling recruitment"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("seedling")] <- "seedlings"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("diversity")] <- "species diversity"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("forest succession")] <- "succession"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("management")] <- "sustainable management"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("sustainability")] <- "sustainable management"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("sustainable forest management")] <- "sustainable management"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("sustainability")] <- "sustainable management"
tabla_separado$Author_Keywords[tabla_separado$Author_Keywords  %in% c("logging")] <- "thinning"

# separamos aquellos registros donde si esten las palabras clave del theasure
art_si <-  tabla_separado[tabla_separado$Author_Keywords %in% theasure$remplazar,]

# unimos las palabras clave
art_si_reunida <- art_si %>%
  group_by(Title) %>%  # Supongamos que tienes una columna ID que identifica cada grupo original
  summarise(Author_Keywords = str_c(Author_Keywords, collapse = "; "))  # Juntar las palabras con un delimitador

# Filtrar las filas donde la columna 'columna_vacia' está vacía (cadena vacía)
art_si_vacio <- art_si_reunida %>% 
  filter(art_si_reunida$Author_Keywords == "")

# separamos de los registrso completos para despues guardarlo
# en teoria los articulos que no estan en este objeto son los que no tiene almenos una de las palabras clave y no aportan 
# a los analisis
art_fin <- tabla_ordenada[tabla_ordenada$Title %in% art_si_reunida$Title,]
colnames(art_fin)[colnames(art_fin) == "Author_Keywords"] <- "Author Keywords"

# guardamos 
write.csv(art_fin, "base_preeliminar.csv")
#
######### mapear por paises #########

# caragamos la base de datos en formato bib
file <- "base_bib_preliminar.bib"
M <- convert2df(file = file, dbsource = "scopus", format = "bibtex")
head(M$TI)

# caragamos la base de datos pre-limpia
bd <- read.csv("base_preeliminar.csv")
bd$Title <- toupper(bd$Title)
head(bd$Title)

# filtramos la base de daos M para tener los mismos documentos que en bd
M2 <- M[M$TI %in% bd$Title,]

# obtenemos informacion relevante, esto sale de bibli
results <- biblioAnalysis(M2, sep = ";")
options(width=100)

# esto nos genera diversos estadisticos, K = 30 especifica que solo se usen los 30
# registros mas frecuentes
S <- summary(object = results, k = 30, pause = FALSE)

# ahora separamos la informacion de los paises
country <- S[["MostProdCountries"]]

# anotamos las key api para poder usar google maps desde r, esta se obtiene a partir
# de la pagina de internet de google maps api
register_google(key = "AIzaSyC9jNUC6YkEOZ4iQD76igWhS_wqOmnQblU")

# Eliminar espacios en blanco antes y después de cada palabra
country$Country <- str_trim(country$Country)

# Creamos un vector con los paises
paises <- country$Country
paises

# Función para obtener coordenadas
obtener_coordenadas <- function(pais) {
  # Geocodifica el nombre de la capital del país
  direccion <- paste("capital of", pais)
  coordenadas <- geocode(direccion)
  return(coordenadas)
}

# Obtén las coordenadas para cada país
coordenadas_paises <- do.call(rbind, lapply(paises, obtener_coordenadas))
coor_paises <- cbind(country, coordenadas_paises)

# base de datos para hacer el grafico
coor_paises
colnames(coor_paises)[colnames(coor_paises) == "lon"] <- "long"
colnames(coor_paises)[colnames(coor_paises) == "Country"] <- "name"


# Asegúrate de que las columnas 'SCP' y 'MCP' sean numéricas
coor_paises$SCP <- as.numeric(coor_paises$SCP)
coor_paises$MCP <- as.numeric(coor_paises$MCP)
coor_paises$Articles <- as.numeric(coor_paises$Articles)

# Cambiar los valores en las celdas específicas, esto porque tenian una mala codificacion
coor_paises[9, 7] <- 52.8987  # Cambia el valor en la fila 2, columna 3
coor_paises[9, 8] <- 18.7136  # Cambia el valor en la fila 4, columna 2

# la funcion ne_countries nos permite jalar la informacion de los paises y sus metadatos
world <- ne_countries(scale = "medium", returnclass = "sf")

# corregir los nombres de los paises para que sean similares a world
coor_paises$name[coor_paises$name == "USA"] <- "United States of America"
coor_paises$name[coor_paises$name == "CANADA"] <- "Canada"
coor_paises$name[coor_paises$name == "SPAIN"] <- "Spain"
coor_paises$name[coor_paises$name == "BRAZIL"] <- "Brazil"
coor_paises$name[coor_paises$name == "GERMANY"] <- "Germany"
coor_paises$name[coor_paises$name == "CHINA"] <- "China"
coor_paises$name[coor_paises$name == "ITALY"] <- "Italy"
coor_paises$name[coor_paises$name == "SWEDEN"] <- "Sweden"
coor_paises$name[coor_paises$name == "POLAND"] <- "Poland"
coor_paises$name[coor_paises$name == "ARGENTINA"] <- "Argentina"
coor_paises$name[coor_paises$name == "UNITED KINGDOM"] <- "United Kingdom"
coor_paises$name[coor_paises$name == "MEXICO"] <- "Mexico"
coor_paises$name[coor_paises$name == "AUSTRALIA"] <- "Australia"
coor_paises$name[coor_paises$name == "CZECH REPUBLIC"] <- "Czech Republic"
coor_paises$name[coor_paises$name == "FRANCE"] <- "France"
coor_paises$name[coor_paises$name == "JAPAN"] <- "Japan"
coor_paises$name[coor_paises$name == "BELGIUM"] <- "Belgium"
coor_paises$name[coor_paises$name == "SWITZERLAND"] <- "Switzerland"
coor_paises$name[coor_paises$name == "INDIA"] <- "India"
coor_paises$name[coor_paises$name == "FINLAND"] <- "Finland"
coor_paises$name[coor_paises$name == "AUSTRIA"] <- "Austria"
coor_paises$name[coor_paises$name == "PORTUGAL"] <- "Portugal"
coor_paises$name[coor_paises$name == "INDONESIA"] <- "Indonesia"
coor_paises$name[coor_paises$name == "NETHERLANDS"] <- "Netherlands"
coor_paises$name[coor_paises$name == "NEW ZEALAND"] <- "New Zealand"
coor_paises$name[coor_paises$name == "ESTONIA"] <- "Estonia"
coor_paises$name[coor_paises$name == "DENMARK"] <- "Denmark"
coor_paises$name[coor_paises$name == "MALAYSIA"] <- "Malaysia"
coor_paises$name[coor_paises$name == "THAILAND"] <- "Thailand"
coor_paises$name[coor_paises$name == "BOLIVIA"] <- "Bolivia"

# ahora realizamos un empate entre los paises del mapa mundi y los de nuestra base de datos
world_data <- left_join(world, coor_paises, by = "name")

# graficamos 
# cambiar escala de color amarillo = menos
ggplot(data = world_data) +
  geom_sf(aes(fill = Articles)) + #agrega geometría de objetos
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", na.value = "white")+
  #ggtitle("Mapa Mundial")+
  #xlab("Longitud") + ylab("Latitud") +
  labs(fill = "Articulos")+
  theme_void()

ggplot(data = world_data) +
  geom_sf(aes(fill = MCP)) + #agrega geometría de objetos
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", na.value = "white")+
  #ggtitle("Mapa Mundial")+
  #xlab("Longitud") + ylab("Latitud") +
  labs(fill = "Articulos")+
  theme_void()


world <- map_data('world')
p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()
p + geom_scatterpie(aes(x=long, y=lat, group=Country, r=Articles/5),
                    data=coor_paises, cols=c("SCP", "MCP"), color=NA, alpha=.8)+
  geom_scatterpie_legend(coor_paises$Articles/5, x=-160, y=-55)


#
######### dendograma temas ######
# realizamos pruebas iniciales

# para hacer el dendograma de los topicos usamos las palabras de los autores.
# Este analisis usa K-means, asi que le especificamos que pueden existi hasta 10 
# grupos (k.max) pero le decimos que escoja 6 grupos (clust) porque es el numero
# de grupos que obtuvimos en vosview



# en la siguiente configuracion se uso un corte de co-ocurrencias mayor a 10, 
# por lo que se usaron 90 palabras. Los dos primeros componentes explica
# 12.44% y 9.85%
CS <- conceptualStructure(M2, field="DE", method="MCA", minDegree=10, clust= 6,
                          k.max = 10, stemming=FALSE, labelsize=10, documents=30)


# en la siguiente configuracion se uso un corte de co-ocurrencias mayor a 10. 
# Los dos primeros componentes explican 12.44% y 9.85%. Pero ahora subiremos hasta 
# 4 los clusters seleccionados, esto para tener una vision de los extremos y saber 
# que grupos se mantienen y cuales no
CS1 <- conceptualStructure(M2, field="DE", method="MCA", minDegree=10, clust= 4,
                           k.max = 10, stemming=FALSE, labelsize=10, documents=10) 

# ahora mantenemos los mismos parametros pero usamos 8 clusters
CS2 <- conceptualStructure(M2, field="DE", method="MCA", minDegree=10, clust= 8,
                           k.max = 10, stemming=FALSE, labelsize=10, documents=10)

# ahora ya que realizamos las pruaba anteriones es necesario ver como se
# desempeñan los otros metodos, en este caso el metodo de analisis de correspondencia 
# se desempeña peor que el analisi de escalamiento metrico multidimencional (MCA) 
# ya que los primeros componentes explican el 2.32% y 2.26%
CS3 <- conceptualStructure(M2, field="DE", method="CA", minDegree=10, clust= 6,
                           k.max = 10, stemming=FALSE, labelsize=10, documents=10)


CS4 <- conceptualStructure(M2, field="DE", method="MDS", minDegree=10, clust= 6,
                           k.max = 10, stemming=FALSE, labelsize=10, documents=10)
#
####### Acoplamiento bibliografico #####

# caragamos la base de datos en formato bib
file <- "base_bib_preliminar.bib"
M <- convert2df(file = file, dbsource = "scopus", format = "bibtex")
head(M$TI)

file1 <- "base_preeliminar.csv"
M2 <- convert2df(file = file1, dbsource = "scopus", format = "csv")
head(M2$TI)

# To see what the sep is
M2$CR[1]

# To obtain the most frequent cited manuscripts:
CR <- citations(M2, field = "article", sep = ";")
cbind(CR$Cited[1:10])

# To obtain the most frequent cited first authors:
CR <- citations(M2, field = "author", sep = ";")
cbind(CR$Cited[1:10])

~# 
NetMatrix <- biblioNetwork(M2, analysis = "co-citation", network = "references")
netstat <- networkStat(NetMatrix)

net <- networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, 
Title = "Authors' Coupling", type = "fruchterman", size=5, size.cex=T,
remove.multiple=TRUE, labelsize=0.8, label.n=10, label.cex=F)


data(scientometrics, package = "bibliometrix")
M <- scientometrics
M2 <- M
NetMatrix <- biblioNetwork(M2, analysis = "coupling", network = "authors", sep = ";")

#

