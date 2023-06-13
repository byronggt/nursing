# Dr. Byron González
# http://www.byrong.tk 

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
if(!require(performance)){install.packages("performance")}
if(!require(jtools)){install.packages("jtools")}
if(!require(readxl)){install.packages("readxl")}

# Abrir la tabla de datos "centrosalud1" -----
centro1<-read_excel("centrosalud1.xlsx")
head(centro1)

str(centro1)
pairs(centro1)

# Matriz de correlaciones
names(centro1)
cor(centro1[,c("edad","peso","talla","imc","pas","pad")], use="complete")
rcorr.adjust(centro1[,c("edad","peso","talla","imc","pas","pad")], type="pearson", use="complete")

# Correlograma de tipo círculo
corrplot(cor(centro1), method="circle")

# Correlograma de tipo elipse
corrplot(cor(centro1), method="ellipse")

# Correlograma de tipo número
corrplot(cor(centro1), method="number")

# Correlograma de tipo matriz superior
corrplot(cor(centro1), type="upper")

# Correlograma de tipo matriz inferior
corrplot(cor(centro1), type="lower")

# Correlograma de tipo mapa de calor
c1<-cor(centro1)
ggcorrplot(c1)
ggcorrplot(c1, type="lower", lab=T)

# Análisis de regresión lineal simple para edad vs pas
attach(centro1)
plot(edad,pas
     , xlab = "edad del paciente"
     , ylab = "presión arterial sistólica")
model<-lm(pas~edad)
summary(model)

# Revisión de los supuestos de la regresión
windows(10,10)
check_model(model)
check_normality(model)

# Análisis de regresión lineal simple para talla y peso
plot(talla,peso)
model1<-lm(peso~talla)
centro1$pred_peso<-model1$fitted.values
centro1$res_peso<-model1$residuals
summary(model1)

# Revisión de los supuestos de la regresión
windows(10,10)
check_model(model1)
check_normality(model1)

# Gráfico de ajuste
effect_plot(model1, pred = talla, interval = TRUE, plot.points = TRUE, x.lab="Talla", y.lab="Peso")


