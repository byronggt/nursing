# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(rempsyc)){install.packages("rempsyc")}

# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)

# Prueba de hipótesis acerca de la media de una muestra
t.test(pesoc,mu=500, alternative = "t", conf.level = 0.95)

# Prueba de hipótesis acerca de dos medias 
t.test(a,b, alternative = "t", var.equal=T, conf.level = 0.95)


# prueba de hipótesis acerca de la varianza de dos muestras







