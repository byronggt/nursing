# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(performance)){install.packages("performance")}
if(!require(car)){install.packages("car")}
if(!require(jtools)){install.packages("jtools")}

# Abrir la tabla de datos "creatininab" -----
creatin<-read_excel("creatininab.xlsx")
head(creatin)

# Gráfico tridimensional
# sin elipsoide
scatter3d(creatinina~estatura+peso
          , data=creatin
          , surface=FALSE
          , residuals=TRUE
          , bg="white"
          , axis.scales=TRUE
          , grid=TRUE)

# con elipsoide
scatter3d(creatinina~estatura+peso
          , data=creatin
          , surface=FALSE
          , residuals=TRUE
          , bg="white"
          , axis.scales=TRUE
          , grid=TRUE
          , ellipsoid=TRUE)

# Modelo de regresión y residuos
attach(creatin)
mod1<-lm(creatinina~estatura+peso)
summary(mod1)

# Revisión de los supuestos
windows(10,10)
check_model(mod1)

check_normality(mod1)

# Modelo simple sin estatura
mod2<-lm(creatinina~peso)
summary(mod2)

# Revisión de los supuestos
windows(10,10)
check_model(mod2)

check_normality(mod2)

# Adición de predichos y residuos a la tabla original
creatin$pred_creat<-mod2$fitted.values
creatin$res_creat<-mod2$residuals

# Graficar el modelo ajustado
effect_plot(mod2, pred = peso, interval = TRUE, plot.points = TRUE, x.lab="Peso (Kg)", y.lab="Creatinina (mg/día)")
