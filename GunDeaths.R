#EVALUACIÓN CEMEX


#Directorio de trabajo
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))
getwd()

#Carga de datos
datos<-read.csv("GunDeaths.csv")

#Evaluación rápida de todos los datos
names(datos)
str(datos)
summary(datos)
table(complete.cases(datos))
prop.table(table(complete.cases(datos)))*100
table(datos$ageGroup)

#Revisión de las variables age y ageGroup
prop.table(table(datos$ageGroup))*100
table(datos$age, datos$ageGroup)
tapply(datos$age, datos$ageGroup, summary)
#ageGroups: 0; 1(0-12); 2(13-17); 3(18-95)

#Revisión de las variable gender
datos$gender <- ifelse(datos$gender=="", NA, datos$gender)
table(datos$gender, exclude = NULL)
prop.table(table(datos$gender, exclude = NULL))*100
datos$gender <- as.factor(as.character(datos$gender))

#Revisión de las variable state
estados<-table(datos$state)
estados
estados[order(estados)]  
levels(as.factor(datos$state))
rm(estados)
datos$state<-toupper(datos$state)
datos$state<-as.factor(datos$state)
levels(datos$state)
str(datos$state)


#Mapa 01
library(tidyverse)
library(urbnmapr)

#Prepara archivo de mapa
spatial_data <- left_join(statedata,
                          get_urbn_map(map = "states", sf = TRUE),
                          by = "state_name")
names(spatial_data)
to_remove <- c("state_fips.x", "state_fips.y","horate","medhhincome")
spatial_data <- spatial_data[ , !(names(spatial_data) %in% to_remove)]
rm(to_remove)


#Crea tabla con el número total de casos por estado
library(dplyr)
states_tbl <- datos %>% group_by(state) %>% 
  summarise(Total=n())

#Agrega el total de casos al archivo del mapa
table(states_tbl$state)
sum(states_tbl$Total)
names(states_tbl)
names(spatial_data)
colnames(states_tbl)[colnames(states_tbl) == "state"] ="state_abbv"
spatial_data <- dplyr::left_join(spatial_data, states_tbl, by = "state_abbv")
rm(states_tbl)

#Agrega informacion poblacional del censo de 2019
library("covidcast")
datos_poblacion <- state_census
names(datos_poblacion)
colnames(datos_poblacion)[colnames(datos_poblacion) == "ABBR"] ="state_abbv"
spatial_data <- dplyr::left_join(spatial_data, datos_poblacion, by = "state_abbv")
rm(datos_poblacion)

#POPESTIMATE2019: Estimate of the state's resident population in 2019
#POPEST18PLUS2019: Estimate of the state's resident population in 2019 that is over 18 years old
#PCNT_POPEST18PLUS: Estimate of the percent of a state's resident population in 2019 that is over 18.

#Calculo del numero de muertes por cada 100,000 habitantes
spatial_data$porcentaje <-(spatial_data$Total/(spatial_data$POPESTIMATE2019/10000))*100
summary(spatial_data$porcentaje)

#Mapa numero total de muertes por arma por estado
ggplot(data = spatial_data, 
       aes(geometry = geometry)) + 
  geom_sf(mapping = aes(fill = porcentaje), color = "#bdbebd") +
  coord_sf(datum = NA)  +
  ggtitle("Muertes por arma de fuego por cada 100,000 hab.") +
  labs(fill = "Tasa") + theme(legend.position="bottom") +
  scale_fill_gradient(low = 'seashell1', 
                      high = 'coral3')


#Mapa 02
library(tidyverse)
library(sf)
library(mapview)

datosTX <- datos  %>% 
  filter(state == "TX")

mapview(datosTX,
        zcol = "gender",
        xcol = "lng", 
        ycol = "lat", 
        crs = 4269, 
        grid = FALSE,
        col.regions = c("blue", "orange"))

#----FIN----
