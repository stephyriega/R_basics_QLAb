#Importación de datos----

#install.packages("ggpol")

library(readxl)
library(ggplot2)
library(ggpol)
library(rio)
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggmosaic)


#primero fijamos el directorio donde se ubican los archivos
setwd(paste0(getwd(), "/BD/MUNICIPAL DISTRITAL 2018"))

#importamos en df cada uno de los archivos
candidatos <- read_xlsx("ERM2018_Candidatos_Distrital.xlsx")
padron <- read_xlsx("ERM2018_Padron_Distrital.xlsx")
resultados <- read_xlsx("ERM2018_Resultados_Distrital.xlsx")
autoridades <- read_xlsx("ERM2018_Autoridades_Distrital.xlsx")




#Genera cuadro que indica cantidad de candidatos por sexo
candidatos_sexo <- candidatos |> 
  filter(candidatos$`Cargo`== "ALCALDE DISTRITAL") |> 
  dplyr::group_by(Sexo) |> 
  dplyr::summarise(candidatos=sum(n()))


#Genera cuadro que indica cantidad de candidatos por distrito y sexo 
candidatos_sexo_distrito <- candidatos |> 
  filter(candidatos$`Cargo`== "ALCALDE DISTRITAL") |> 
  dplyr::group_by(Distrito, Sexo) |> 
  dplyr::summarise(candidatos_distrital1=sum(n()))

#Genera cuadro que indica cantidad de candidatos por distrito y sexo.
#La diferencia con el anterior cuadro es que estos distritos tienen candidatos
#femeninos y masculinos. 
#En el anterior, hay distritos que solo presentan o femeninos o masculinos
candidatos_2sexo_distrito <- candidatos %>% 
  filter(candidatos$`Cargo`== "ALCALDE DISTRITAL") %>% 
  group_by(Distrito) %>% 
  filter(n_distinct(Sexo)== 2) %>% 
  group_by(Distrito, Sexo) %>% 
  summarise(candidatos_distrital2=sum(n()))


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

  
### Grafico de indicador de preferencia por sexo y distrito ----
candidatos %>%
  group_by(Sexo, Cargo, Distrito) %>%
  summarize(count = n()) %>%  
  # Crea el gráfico mediante ggplot 
  ggplot(aes(x = Distrito, y = count, fill = Sexo)) +
  # Agrega barra con tamaño proporcional a la cantidad de candidatos
  geom_bar(stat = "identity", position = "fill") + 
  facet_wrap(~ Cargo ) + #divide el gráfico por cargo
  #Para colocar la leyenda en la parte inferior del gráfico y para no colocar los nombres de Distro en eje x
  theme(legend.position = "bottom", axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  # Coloca etiquetas y título. En este gráfico no se considera los nombres del eje x debido a su extensión 
  ylab("Indicador de preferencia") +
  ggtitle("Distribución de Candidatos por distrito") 



### Grafico de indicador de preferencia por sexo y distrito ----
unique(candidatos$Region)
candidatos %>%
  group_by(Region, Sexo, Cargo, Distrito) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Region, y = count, fill = Sexo)) +
  # Agrega barra con tamaño proporcional a la cantidad de candidatos
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Cargo ) + #divide el gráfico por cargo
  # Para colocar la leyenda en la parte inferior del gráfico
  theme(legend.position = "bottom") + 
  # Coloca etiquetas y título 
  xlab("Region") +
  ylab("Indicador de preferencia") +
  ggtitle("Distribución de Candidatos por departamento")+
  # Ayuda a convertir los nombres horizontales en verticales, para que no aparezcan superpuestos
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Distribución de candidatos por Macrorregión (Norte, Centro y Sur)
candidatos <- as.data.frame(candidatos)

candidatos_2 <- candidatos |>
  mutate(Joven = ifelse(is.na(Joven), "No Joven", Joven)) |> 
  mutate(Nativo = ifelse(is.na(Nativo), "No Nativo", Nativo)) |> 
  mutate(macrorregion = if_else(Region %in% c("AMAZONAS" , "CAJAMARCA","LA LIBERTAD", "LAMBAYEQUE", "LORETO", "PIURA", "SAN MARTIN", "TUMBES"), "Norte",
                                if_else(Region %in% c("LIMA","ANCASH", "CALLAO", "HUANCAVELICA", "HUANUCO", "JUNIN","MADRE DE DIOS", "PASCO", "UCAYALI"), "Centro",
                                        if_else(Region %in% c("AREQUIPA","APURIMAC", "AYACUCHO", "CUSCO","ICA","MOQUEGUA", "PUNO", "TACNA"), "Sur", "NA"))))

candidatos_2 |> 
  group_by(macrorregion, Sexo) |> 
  ggplot()+
  geom_mosaic(aes(x = product(macrorregion), fill=Sexo)) +
  ggtitle("Distribución de candidatos por macrorregión y sexo")


#Distribución de autoridades elegidas por macrorregion
autoridades_2 |> 
  group_by(Macrorregion, Sexo) |> 
  ggplot()+
  geom_mosaic(aes(x = product(Macrorregion), fill=Sexo)) +
  ggtitle("Distribución de autoridades electas por macrorregión y sexo")
  




