# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}

# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)
attach(centro)

# Histograma de frecuencias absolutas
h1<- graph.freq(edad, col="yellow", frequency =1, xlab="Edad (años)", ylab="Número de pacientes", main="frecuencia absoluta")

# Histograma con polígono de frecuencias

h2<- graph.freq(edad, frequency =2 , main="Polígono de frecuencia")
polygon.freq(h2, col="blue", lwd=2, frequency =2)

# Gráfico de densidad

h3<- graph.freq(edad, col="brown", frequency =3 , main="Gráfico de densidad")

# Gráfico de densidad con curva normal

h4<- graph.freq(edad, col="blue", frequency =3 , main="Densidad normal", density=4)
normal.freq(h4, col="red", lty=4,lwd=2, frequency=3)
dens1<-density(edad)
plot(dens1)

h5<- graph.freq(edad, frequency=1, axes=T)

# En caso se requiera definir el número de clases
# , nclass=7, main="frecuencia con 7 clases")

# Ojiva de Galton

h6<-ogive.freq(h5,axes=T,type="b", xlab="Edad (años)", ylab="Fracción de pacientes", main="Ojiva de Galton", col="red")
round(table.freq(h5),3)

# Estadísticas descriptivas para datos agrupados
stat.freq(h5)

# Obtener la tabla de frecuencias para la edad

tablaf<-table.freq(h5)
tablaf %>% 
  flextable() %>% 
  autofit () %>% 
  save_as_docx(path = "tabla de frecuencias.docx")