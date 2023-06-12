# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(rempsyc)){install.packages("rempsyc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}

# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)

# Gráfico de puntos
windows(10,10)
ggplot(centro, aes(pas)) +
  geom_dotplot(aes(fill=sexo)) +
  labs(x = "Presión arterial sistólica", y = "")

# Prueba de hipótesis acerca de la media de una muestra
boxplot(centro$pas
        , col="orange"
        , ylab="Presión arterial sistólica")
t.test(centro$pas,pas=140, alternative = "t", conf.level = 0.95)

# Prueba de hipótesis acerca de dos medias 
mujeres<-subset(centro, sexo=="femenino")
hombres<-subset(centro, sexo=="masculino")

boxplot(centro$pas~centro$sexo, col="orange",
        xlab="Sexo",
        ylab="Presión arterial sistólica")

# Se asume que las varianzas son iguales
# Usar el comando `ggbetweenstats()` para visualizar los datos
centro1<-centro %>% 
          select(sexo,pas)
centro1
attach(centro1)

ggbetweenstats(centro1, x = sexo, y = pas, 
               plot.type = "box",
               type="parametric", var.equal = T)

detach(centro1)

t.test(mujeres$pas,hombres$pas, alternative = "t", var.equal=T, conf.level = 0.95)


# prueba de hipótesis acerca de la varianza de dos muestras
var.test(hombres$pas,mujeres$pas)






