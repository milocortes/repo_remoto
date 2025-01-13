sacmex <- read.csv("C:/Users/strep/Desktop/Clase de modelación/CONTRERAS.csv", 
                   header = TRUE, 
                   fileEncoding = "UTF-8", 
                   check.names = FALSE)
sacmex$tipo <- ifelse(sacmex$DICTAMEN %in% c("ATARJEA OBSTRUIDA", "COLADERA OBSTRUIDA"), "Obstruction",
                      ifelse(sacmex$DICTAMEN %in% c("FALTA DE INFRAESTRUCTURA", "INSUFICIENCIA DE ATARJEA Y COLECTOR", "INSUFICIENCIA DE GRIETA"), "Sewage Overflow",
                             ifelse(sacmex$DICTAMEN == "RUPTURA DE TUBO DE AGUA POTABLE", "Breaking off",
                                    ifelse(sacmex$DICTAMEN %in% c("INEXISTENTE AL MOMENTO DE LA INSPECCION [NO SE APRECIAN DIMENSIONES]", "INEXISTENTE AL MOMENTO DE LA INSPECCION"), "Lacking Info",
                                           ifelse(sacmex$DICTAMEN == "NO SE OPERO CARCAMO DE BOMBEO", "Pump Failure",
                                                  ifelse(sacmex$DICTAMEN == "HUNDIMIENTO DE CARPETA ASFALTICA", "Sinking",
                                                         "otros")))))))

frecuencia_tipo <- table(sacmex$tipo)

sacmex$subgrupo <- ifelse(sacmex$Año %in% c(2007, 2008, 2009), "Subgrupo 1",
                          ifelse(sacmex$Año %in% c(2010, 2011, 2012, 2013, 2014), "Subgrupo 2", "Otros"))

sacmex_frecuencia <- sacmex %>%
  count(subgrupo, Año, tipo, name = "Frecuencia")

boxplot(Frecuencia ~ tipo, data = sacmex_frecuencia %>% filter(subgrupo == "Subgrupo 1"),
        main = "Frecuencia por Tipo (Subgrupo 1)")

boxplot(Frecuencia ~ Año, data = sacmex_frecuencia %>% filter(subgrupo == "Subgrupo 1"),
        main = "Frecuencia por Año (Subgrupo 1)")

boxplot(Frecuencia ~ tipo, data = sacmex_frecuencia %>% filter(subgrupo == "Subgrupo 2"),
        main = "Frecuencia por Tipo (Subgrupo 2)")

boxplot(Frecuencia ~ Año, data = sacmex_frecuencia %>% filter(subgrupo == "Subgrupo 2"),
        main = "Frecuencia por Año (Subgrupo 2)")
