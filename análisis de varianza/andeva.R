# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(performance)){install.packages("performance")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}
if(!require(agricolae)){install.packages("agricolae")}

# Abrir la tabla de datos "centrosalud" -----
infarto<-read_excel("infarto.xlsx")
head(infarto)
tail(infarto)

# Gráfico comparativo
boxplot(infarto$total~infarto$grupo
        , xlab = "Grupo"
        , ylab = "Autoeficacias"
        , col = "yellow")
attach(infarto)
model<-aov(total~grupo, data=infarto); model
summary(model)
infarto$predichos<-model$fitted.values
infarto$residuos<-model$residuals
head(infarto)

model1<-lm(total~grupo, data = infarto)
summary(model1)

# Verificación de los supuestos del Anova

boxplot(model$residuals~infarto$grupo, col="orange")

# Hipótesis para la homogeneidad de varianzas de los residuos
# Ho: Las varianzas son homogéneas 
# Ha: Las varianzas son heterogéneas

bartlett.test(total~grupo) # las varianza de los residuos son homogéneas

windows(10,10)
check_model(model1) # Ver el gráfico de residuos estandarizados vs predichos
check_normality(model1)


# Comparación gráfica
ggbetweenstats(infarto, x=grupo, y=total, 
               var.equal = T, plot.type = "box", type = "parametric")
pr.medias <-LSD.test(model, "grupo",console=TRUE)

