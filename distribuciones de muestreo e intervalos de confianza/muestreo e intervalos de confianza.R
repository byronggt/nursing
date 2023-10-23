# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(samplingbook)){install.packages("samplingbook")}
if(!require(confintr)){install.packages("confintr")}
if(!require(car)){install.packages("car")}
if(!require(EnvStats)){install.packages("EnvStats")}
if(!require(janitor)){install.packages("janitor")}


# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)

#===============Muestreo simple aleatorio=======================================
colnames(centro)
# Tomar 30 datos iniciales para calcular tamaño de muestra
# de acuerdo a la presión arterial sistólica (pas)
muestra <- sample(1:nrow(centro), size = 30, replace=FALSE)
muestra
data_msa <- centro[muestra, ]
data_msa
# Calcular el valor de "e" 
precision=qnorm(0.025, lower.tail = F)*(sd(data_msa$pas)/sqrt(30));precision

# Calcular el tamaño de muestra para un e y sd
# Aquí se ha seleccionado un e= 5
n_muestra <- sample.size.mean(e = 5, sd(data_msa$pas), N = Inf, level = 0.95)
n_muestra
# completar muestra considerando los 30 iniciales
data_restante <- centro[-muestra, ] # A la tabla original se resta 30
muestra_complemento <- sample(1:nrow(data_restante),size=(n_muestra$n-30), replace=FALSE)
muestra_definitiva<-rbind(centro[muestra_complemento, ], data_msa)

ic_mean<- MeanCI(x=muestra_definitiva$pas, trim = 0, conf.level = 0.95, na.rm = FALSE);ic_mean

## Intervalos de confianza (IC)

# Intervalo de confianza para la media de 
# presión arterial en hombres
mujeres<-subset(centro, sexo=="femenino")
hombres<-subset(centro, sexo=="masculino")
shapiro.test(hombres$pas)
qqPlot(hombres$pas, pch=19, las=1, main="QQplot",
       xlab="Cuantiles teóricos", ylab="Cuantiles muestrales")

ic_pas_h<-ci_mean(hombres$pas); ic_pas_h

# Intervalo de confianza para la diferencia de medias 
# entre medias de pas para hombres y mujeres

ci_mean_diff(mujeres$pas, mujeres$pas)

# Intervalo de confianza para la proporción de mujeres
# con pas > 140

mujeres1<-subset(mujeres, select = c(pas))
mujeres2<-na.omit(mujeres1)
mujeres2
mujeres2 %>%
  mutate(masde140=ifelse(pas>140,1,0)) %>%
  tabyl(masde140) %>%
  adorn_totals("row")

# Calcular el IC para la proporción de pas>140

BinomCI(93, 180,conf.level=0.95)

## Intervalo de confianza para la varianza de pas para mujeres
ci_var(mujeres$pas)

## Intervalo de confianza para la diferencia
## de varianzas de pas para hombres y mujeres
var.test(x=mujeres$pas, y=hombres$pas,conf.level=0.95)$conf.int

#===============Muestreo simple aleatorio con simulación=======================================

# Simulación para calcular tamaño de muestra
# al considerar diferentes valores de precisión y niveles de confianza
# 1000 simulaciones para cada combinación mediante bucles

data_muestreo <- NULL
for (conf in seq(0.80, 0.95, by = 0.05)){
  for(epas in seq(1, 6, by = 0.50)){
    for(simulacion in 1:1000){
      muestra_temp <- centro
      muestra <- sample(1:nrow(muestra_temp), size = 30, replace=FALSE)
      data_msa<- muestra_temp[muestra, ]
      n_muestra <- sample.size.mean(e = epas, sd(data_msa$pas), N = Inf, level = conf)
      data_temp <- data.frame("confiabilidad" = conf,
                              "precision" = epas,
                              "simulacion" = simulacion,
                              "pacientes_muestra" = n_muestra$n)
      data_muestreo <- rbind(data_muestreo, data_temp)
      
      
      
    }
  }
}
head(data_muestreo)
tail(data_muestreo)

data_muestreo_resumen_final <-
  data_muestreo %>% 
  group_by(confiabilidad, precision)%>%
  summarise(sd_muestras = sd(pacientes_muestra),
            pacientes_muestra = mean(pacientes_muestra),
            simulaciones = length(confiabilidad))

data_muestreo_resumen_final %>% 
  print(max=1000)


# Seleccionar "n" de acuerdo a la experiencia o efectos prácticos
# la mejor combinación de precisión y nivel de confianza
      
      