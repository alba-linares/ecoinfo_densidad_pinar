getwd()
library(here)
library(sf)
library(dplyr)

#Importamos el archivo csv de GBIF con sus atributos geográficos
presencias            <-read.delim(file=here("0002064-240130105604617.csv"), quote = '')
  #read.delim2 es para las comas como separador de decimales y los puntos como separador de miles

#Convertimos la tabla importada a un objeto geográfico tipo sf
presencias_geo        <- st_as_sf(presencias,coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) #WGS84
#Reproyectamos la capa creada al sistema de coordenadas EPSG:23030 (UTM)
presencias_geo_23030  <- st_transform(presencias_geo, crs = 23030)
#Creamos una malla de 250 m con la extensión y sistema de coordenadas de la capa de presencias
grid_250m             <- st_make_grid(presencias_geo_23030, cellsize = c(250, 250))
#Transformamos la malla obtenida (tipo sfc) a tipo espacial sf
grid_250m_sf          <-st_sf(geometry = grid_250m)
#Añadimos un campo llamado id_250 a la malla. Le incluimos valores secuenciales desde 1
grid_250m_sf$id_250   <- seq(1, 55125, by = 1) #55125 es el n de recuadros en la malla (se puede ver en Environment)
#Asignamos a cada punto de presencia el código del cuadrado de la malla en el que está. Unión espacial.
presencias_x_grid     <- st_join(presencias_geo_23030, left = F, grid_250m_sf) 
##########left = significa ¿quieres guardar las observaciones aunque no se solapen espacialmente? T: sí, F: no.

# 10. Extraemos la tabla de atributos de la capa de puntos creada y borramos todos los campos menos los dos que nos interesan. 
bio <- as.data.frame(presencias_x_grid)
bio <- bio[c("id_250","scientificName")]

# 11. Calcular el número de individuos por especie y por cuadrícula (num_ind_sp_cuad)
T_num_ind_sp_cuad <- bio %>%    #create a new data table of the aggregated data
  group_by(id_250, scientificName) %>%    #group the calculations by id_250 + scientificName
  summarize(`num_ind_sp_cuad` = length(scientificName)) 
#     length del vector, es decir, el nº de veces que sale en cada cuadrícula
#     %>% significa que "de esta dataframe, me haces las siguientes operaciones:". Se le llaman pipes.

T_num_ind_sp_cuad

# 12. Calcular el número total de individuos por cuadrícula.
T_num_ind_cuad <- bio %>%    #create a new data table of the aggregated data
  group_by(id_250) %>%    #group the calculations by id_250 
  summarize(`num_ind_cuad` = n())

# 13. Fusionar las tablas anteriores para calcular Pi
T_num_ind_sp_cuad_mas_num_ind_cuad <-merge(T_num_ind_sp_cuad,T_num_ind_cuad)
#     merge() no hace falta decirle el campo común por el que se une, porque solo hay uno coincidente

# 14. Calcular pi por especie y por cuadrícula.
T_num_ind_sp_cuad_mas_num_ind_cuad$pi <- T_num_ind_sp_cuad_mas_num_ind_cuad$num_ind_sp_cuad/T_num_ind_sp_cuad_mas_num_ind_cuad$num_ind_cuad
#     se divide la columna de spp por la columna de la cantidad de spp de cada cuadrícula

# 15. Calcular el log2 pi por especie y por cuadrícula
T_num_ind_sp_cuad_mas_num_ind_cuad$lnpi_pi <- log2(T_num_ind_sp_cuad_mas_num_ind_cuad$pi)*T_num_ind_sp_cuad_mas_num_ind_cuad$pi
#     log(proporción)*proporción

# 16. Calcular H por cuadrícula
T_Shannon <- T_num_ind_sp_cuad_mas_num_ind_cuad %>%    #create a new data table of the aggregated data
  group_by(id_250) %>%    #group the calculations by id_250 
  summarize(`H` = sum(lnpi_pi)*(-1))

# 17. Fusionar la tabla que tiene el índice de Shannon con la malla de cuadrículas.
grid_250m_sf<-merge(x = grid_250m_sf, y = T_Shannon, by.x = "id_250", by.y = "id_250")

# 18. Exportamos la capa de la malla obtenida a un fichero de formas para visualizarlo en QGIS.
st_write(grid_250m_sf, "Shannon_250_sierra_nevada.shp", append=FALSE) 
#     Si es append=TRUE, si se corre varias veces, crea capas del mismo nombre
