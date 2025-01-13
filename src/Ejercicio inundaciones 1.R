#ejercicio de inundacione

sacmex <- read.csv("C:/Users/strep/Desktop/Clase de modelación/CONTRERAS.csv", 
                   header = TRUE, 
                   fileEncoding = "UTF-8", 
                   check.names = FALSE)
sacmex$tipo <- ifelse(sacmex$DICTAMEN == "ATARJEA OBSTRUIDA", "Obstruction",
                      ifelse(sacmex$DICTAMEN == "COLADERA OBSTRUIDA", "Obstruction",
                             ifelse(sacmex$DICTAMEN == "FALTA DE INFRAESTRUCTURA", "Sewage Overflow",
                                    ifelse(sacmex$DICTAMEN == "INSUFICIENCIA DE ATARJEA Y COLECTOR", "Sewage Overflow",
                                           ifelse(sacmex$DICTAMEN == "INSUFICIENCIA DE GRIETA", "Sewage Overflow",
                                                  ifelse(sacmex$DICTAMEN == "RUPTURA DE TUBO DE AGUA POTABLE", "Breaking off",
                                                         ifelse(sacmex$DICTAMEN == "INEXISTENTE AL MOMENTO DE LA INSPECCION [NO SE APRECIAN DIMENSIONES]", "Lacking Info",
                                                                ifelse(sacmex$DICTAMEN == "INEXISTENTE AL MOMENTO DE LA INSPECCION", "Lacking Info",
                                                                       ifelse(sacmex$DICTAMEN == "NO SE OPERO CARCAMO DE BOMBEO", "Pump Failure",
                                                                              ifelse(sacmex$DICTAMEN == "HUNDIMIENTO DE CARPETA ASFALTICA", "Sinking",
                                                                                     "otros")))))))))))

frecuencia_tipo <- table(sacmex$tipo)

#Subgrupo 1: Años 2007, 2008, 2009
sucgrupo1 <- subset(sacmex, Año %in% c(2007, 2008, 2009))

#Subgrupo 2: Años 2010, 2011, 2012, 2013, 2014
sucgrupo2 <- subset(sacmex, Año %in% c(2010, 2011, 2012, 2013, 2014))

frecuencia_subgrupo1 <- sucgrupo1 %>%
  count(Año, tipo, name = "Frecuencia")

frecuencia_subgrupo2 <- sucgrupo2 %>%
  count(Año, tipo, name = "Frecuencia")


boxplot(Frecuencia ~ tipo, data = frecuencia_subgrupo1, 
        main = "Frecuencia por Tipo (2010-2014)")

boxplot(Frecuencia ~ Año, data = frecuencia_subgrupo1, 
        main = "Frecuencia por Tipo (2010-2014)")

boxplot(Frecuencia ~ tipo, data = frecuencia_subgrupo2, 
       main = "Frecuencia por Tipo (2010-2014)")

boxplot(Frecuencia ~ Año, data = frecuencia_subgrupo2, 
        main = "Frecuencia por Tipo (2010-2014)")

#opción 2 anual total 
frecuencia_agrupada <- sacmex %>%
  count(Año, tipo, name = "Frecuencia")

boxplot(Frecuencia ~ Año, data = frecuencia_agrupada, 
        main = "Frecuencia por Año")

boxplot(Frecuencia ~ tipo, data = frecuencia_agrupada, 
        main = "Frecuencia por Año")

