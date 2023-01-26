#Importación de datos----
#primero fijamos el directorio donde se ubican los archivos
setwd("C:/Users/DELL/Documents/GitHub/R_basics_QLAb/BD/MUNICIPAL DISTRITAL 2018/")
getwd()
#install.packages("ggpol")

#importamos en df cada uno de los archivos
library(readxl)
library(ggplot2)
library(ggpol)
#library(rio)
library(dplyr)
library(tidyverse)
library(magrittr)

candidatos <- read_xlsx("ERM2018_Candidatos_Distrital.xlsx")
padron <- read_xlsx("ERM2018_Padron_Distrital.xlsx")
resultados <- read_xlsx("ERM2018_Resultados_Distrital.xlsx")
autoridades <- read_xlsx("ERM2018_Autoridades_Distrital.xlsx")

names(candidatos)
#este df muestra los candidatos por distrios y partido politico

names(padron)
#este df muestra la proporcion de electores por distrito, edad y sexo

names(resultados)
# df que muestra los resultados por distrito y partido politico

names(autoridades)
# df que muestra los alcades elegidos y sus regidores por distrito

# comentarios----
##electores (padron)----
#distribucion de electores por sexo, por edad (4 escenarios)
#distribucion de electores por macroregion (costa, sierra, selva -- norte, centro y sur)

##candidatos----
#distribucion de candidatos por organizacion politica, sexo (opc agregar cargo, nativo)
#distribucion de candidatos por cargo y sexo
#distribucion de candidatos por macroregion (costa, sierra, selva -- norte, centro y sur)
#analizar el campo 'N°' del df
#distribucion de candidatos por tipo de organizacion politica (unique(candidatos$`Tipo Organización Política`))

##autoridades----
#distribucion de autoridades elegidas por organizacion politica, sexo (opc agregar cargo, nativo)
#distribucion de candidatos por cargo y sexo
#distribucion de candidatos por macroregion (costa, sierra, selva -- norte, centro y sur)
#distribucion de candidatos por tipo de organizacion politica
#qué tanto se pasó el umbral minimo para ganar la eleccion (30%)? diferenciar por sexo
#alcades y regidores pertenecen a la misma organización politica? analizar efecto arrastre

##resultados----
#distribucion de participacion por region
#comparar %votos vs %votos organizacion politica (df autoridades)

#para edades indicar solamente la proporcion de jovenes respecto al total

candidatos[,c("Region", "Provincia", "Distrito", "Organización Política", "Tipo Organización Política", "Cargo", "Sexo", "Joven")] <- lapply(candidatos[,c("Region", "Provincia", "Distrito", "Organización Política", "Tipo Organización Política", "Cargo", "Sexo", "Joven")], as.numeric)
summary(candidatos)

#Primero comprobamos que clase es la variable joven para poder completar la categoria faltante, como para la variable nativo. 
class(autoridades$Joven)

autoridades <- autoridades |> 
  dplyr::mutate(Joven = ifelse(is.na(Joven), "No Joven", Joven)) |> 
  dplyr::mutate(Nativo = ifelse(is.na(Nativo), "No Nativo", Nativo))



autoridades <- as.data.frame(autoridades)

### Gráfico de parlamento de las ganadores a regidores municipales distritales por juventud y sexo ----

#Creamos un cuadro resumen para ver la cantidad de REGIDORES DISTRITALES electos, filtrando solo este grupo usando filter().
#Luego, agrupamos estas observaciones por si son jovenes o no y su sexo con la funcion group_by()
#Por ultimo, resumimos estos datos contando cuantas observaciones existen, lo reducimos a centenas y redondeamos.

aut_sum <- autoridades |> 
  filter(autoridades$`Cargo electo`== "REGIDOR DISTRITAL") |> 
  dplyr::group_by(Joven, Sexo) |> 
  dplyr::summarise(regidores=round(n()/100))

#para elaborar el cuadro, debemos agregar la columna de colors con el color correspondiente que queramos usando mutate.
#Sin embargo, este esta determinado por la combinacion entre Joven y Sexo que se haga, por ello usamos case_when.
aut_sum <-aut_sum |> 
  mutate(colors = case_when(Joven == 'Joven' & Sexo == 'Femenino' ~ 'lightpink',
                            Joven == 'Joven' & Sexo == 'Masculino' ~ 'lightblue',
                            Joven == 'No Joven' & Sexo == 'Femenino' ~ 'red',
                            Joven == 'No Joven' & Sexo == 'Masculino' ~ 'blue'
                            ))
#Por ultimo, usamos una extension de ggplot, geom_parluament, para representar a los regidores distritales por juventud y sexo.
ggplot(aut_sum) + 
  geom_parliament(aes(seats = regidores, fill = Joven), color = "black") + 
  scale_fill_manual(values = aut_sum$colors, labels = aut_sum$Joven) +
  coord_fixed() + 
  theme_void()+
  labs(title = "Regidores distritales por juventud y sexo",
       subtitle="Por si es joven y sexo (en centenas)")

### Gráfico de parlamento de las ganadores a alcaldia municipal distrital jovenes y no jovenes por sexo ----


aut_sum2 <- autoridades |> 
  filter(autoridades$`Cargo electo`== "ALCALDE DISTRITAL") |> 
  dplyr::group_by(Joven, Sexo) |> 
  dplyr::summarise(regidores=round(n()))

aut_sum2 <-aut_sum2 |> 
  mutate(colors = case_when(Joven == 'Joven' & Sexo == 'Femenino' ~ 'lightpink',
                            Joven == 'Joven' & Sexo == 'Masculino' ~ 'lightblue',
                            Joven == 'No Joven' & Sexo == 'Femenino' ~ 'red',
                            Joven == 'No Joven' & Sexo == 'Masculino' ~ 'blue'
  ))


ggplot(aut_sum2) + 
  geom_parliament(aes(seats = regidores, fill = Joven), color = "black") + 
  scale_fill_manual(values = aut_sum$colors, labels = aut_sum$Joven) +
  coord_fixed() + 
  theme_void()+
  labs(title = "Alcaldes distritales jovenes y no jovenes por sexo",
       subtitle="Por si es joven y sexo (en centenas)")