#Importación de datos----
#primero fijamos el directorio donde se ubican los archivos
setwd("C:/Users/Cristian/Documents/GitHub/R_basics_QLAb/BD/MUNICIPAL DISTRITAL 2018/")
getwd()

#importamos en df cada uno de los archivos
library(readxl)
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





