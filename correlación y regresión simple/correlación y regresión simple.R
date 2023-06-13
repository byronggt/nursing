# Dr. Byron González
# http://www.byrong.tk 

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
if(!require(readxl)){install.packages("readxl")}

# Abrir la tabla de datos "centrosalud1" -----
centro1<-read_excel("centrosalud1.xlsx")
head(centro1)

str(centro1)
pairs(centro1)

# Matriz de correlaciones
names(centro1)
cor(centro1[,c("edad","peso","talla","imc","pas","pad","fc")], use="complete")
rcorr.adjust(centro1[,c("edad","peso","talla","imc","pas","pad","fc")], type="pearson", use="complete")

# Correlograma de tipo círculo
corrplot(cor(centro1), method="circle")

# Correlograma de tipo elipse
corrplot(cor(salinidad), method="ellipse")

# Correlograma de tipo número
corrplot(cor(salinidad), method="number")

# Correlograma de tipo matriz superior
corrplot(cor(salinidad), type="upper")

# Correlograma de tipo matriz inferior
corrplot(cor(salinidad), type="lower")

# Correlograma de tipo mapa de calor
sal<-cor(salinidad)
ggcorrplot(sal)
ggcorrplot(sal, type="lower", lab=T)