# Dr. Byron González
# http://www.byrong.tk

# Limpiar todos los objetos en memoria
rm(list=ls()) 

if(!require(Hmisc)){install.packages("Hmisc")}
# Ver el listado de tablas de datos disponibles
getHdata()
getHdata(diabetes)

# Ver las primeras 6 filas de la tabla de datos
head(diabetes)

# Ver las últimas 6 filas de la tabla de datos
tail(diabetes)
# Visitar para la descripción de las variables:
# https://hbiostat.org/data/repo/cdiabetes

str(diabetes)
attach(diabetes)
names(diabetes)


# Calcular el número de grupos

n<-length(stab.glu); n
k=ceiling(1+log(n)); k
# Se emplearán 7 grupos

# Definir la amplitud de los intervalos

A = ceiling(diff(range(stab.glu))/7); A # na.rm se emplea cuando hay valores perdidos

# La amplitud será de 49 libras

# Obtener la tabla de frecuencias

# Obtener la tabla de frecuencia con una función
# Ver la ayuda disponible en:
# https://aprender-uib.github.io/AprendeR1/chap-hist.html#c%C3%B3mo-agrupar-datos
# donde x= tabla de datos


Tabla_frec_agrup=function(x,k,A,p){
  L=min(x)-p/2+A*(0:k)
  x_int=cut(x, breaks=L, right=FALSE)
  intervalos=levels(x_int)
  marcas=(L[1]+L[2])/2+A*(0:(k-1))
  f.abs=as.vector(table(x_int))
  f.rel=f.abs/length(x)
  f.abs.cum=cumsum(f.abs)
  f.rel.cum=cumsum(f.rel)
  tabla_x=data.frame(intervalos, marcas, f.abs, f.abs.cum, f.rel, f.rel.cum)
  tabla_x
}
Tabla_frec_agrup(stab.glu,7,49,1)

# Histograma
hist(stab.glu, breaks = "Sturges",
     freq=F,
     main="Distribución de glucosa estabilizada",
     xlab="Glucosa estabilizada",
     ylab="Número de pacientes",
     col="yellow")
curve(dnorm(x, mean(stab.glu), sd(stab.glu)), col="cyan4", lty=4, lwd=2,
      add=T)
