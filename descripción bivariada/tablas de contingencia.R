# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}

# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)

# Generar tablas de contingencia
t1<-table(centro$tabaco,centro$sexo); t1
windows(10,10)
barplot(t1, legend.text = rownames(t1))
mosaicplot(t1, color = T)
mosaicplot(t1, col = 4:2)

# Categorización de variables cuantitativas

centro$pas_cat<- cut(centro$pas, breaks = c(-Inf, 120, 140, Inf), labels = c("normal", "elevada", "alta"))
centro$pas_cat
t2<-table(centro$pas_cat,centro$sexo); t2

## Ingresar una tabla de contingencia ------
t4<-table(centro$estcivil,centro$sexo);t3

# Crear los vectores con los datos de frecuencia
est_civl <- c("Casado/pareja", "Separado", "Soltero", "Viudo")
sexo1 <- c("femenino", "masculino")

# Crear la matriz de frecuencias
t5 <- matrix(c(98,11,8,63,77,0,3,5), nrow = length(est_civl), ncol = length(sexo1),
                      dimnames = list(est_civl, sexo1))
t5

## Medidas de resumen estadístico -----

# Relación entre sexo y fumador
prop.table(t1) # Proporción respecto al total
prop.table(t1,2) # Proporción respecto al sexo
chisq.test(t1)

# Calcular las frecuencias esperadas bajo la hipótesis nula
chisq.test(t1)$expected
# Si las frecuencias esperadas son menor a 5 es necesario
# fusionar categorías, como se muestra enseguida
t1
centro$tabaco2<-ifelse((centro$tabaco=="ex fumador 10 años+")|(centro$tabaco=="ex fumador 9 años-"),"ex fumador", as.character(centro$tabaco))
t1a=with(centro,table(tabaco2,sexo))
t1a
chisq.test(t1a) # No se resalta error alguno en la prueba





