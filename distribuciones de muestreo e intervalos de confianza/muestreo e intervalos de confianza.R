# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(samplingbook)){install.packages("samplingbook")}

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

data_muestreo_resumen_final <-
  data_muestreo %>% 
  group_by(confiabilidad, precision)%>%
  summarise(sd_muestras = sd(pacientes_muestra),
            pacientes_muestra = mean(pacientes_muestra),
            simulaciones = length(confiabilidad))

data_muestreo_resumen_final

# Seleccionar "n" de acuerdo a la experiencia o efectos prácticos
# la mejor combinación de precisión y nivel de confianza
      
      