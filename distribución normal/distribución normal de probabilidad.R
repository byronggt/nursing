# Dr. Byron González
# http://www.byrong.tk 


if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(readxl)){install.packages("readxl")}
if(!require(agricolae)){install.packages("agricolae")}

# Entre los diabéticos, a la glucemia en ayunas puede suponerse
# una distribución normal con media de 106 mg/100 mL 
# y desviación estándar de 8 mg/100 mL

# 1. ¿Qué porcentaje de diabéticos tendrá niveles 
# entre 90 y 120 mg/100 mL? 

P120<- pnorm(c(120), mean(106), sd=8, lower.tail=T)
P90<-pnorm(90,106,8)
P90a120<-P120-P90; P90a120

# 2. Gráfica de la distribución de probabilidad -----

.x<-seq(82, 130, length.out=1000)
windows(10,10)
plotDistr(.x, dnorm(.x,106,8), cdf=F, 
          xlab="Glucemia en ayunas",
          ylab="Densidad",
          main="Distribución normal",
          regions=list(c(90,120)), col="red",
          legend.pos = "topleft", cex=0.1          
)
# 3. Calcular P(106 ≤ X ≤ 110)
P110<- pnorm(c(110), mean(106), sd=8, lower.tail=T)
P106<-pnorm(106,106,8)
P106a110<-P110-P106; P106a110


# 4. Gráfica de la distribución de probabilidad
.x1<-seq(82, 130, length.out=1000)
plot.new()
dev.new(10,10)
plotDistr(.x1, dnorm(.x1,106,8), cdf=F, 
          xlab="Glucemia en ayunas",
          ylab="Densidad",
          main="Distribución normal",
          regions=list(c(106,110)), col="red",
          legend.pos = "topleft", cex=0.1          
)

# Segundo procedimiento para graficar la distribución de probabilidad
# Crear la siguiente función: normal_area
# mean: media de la variable normal
# sd: desviación típica de la variable normal
# lb: límite inferior del área
# ub: límite superior del área
# acolor: color del área
# ...: argumentos adicionales para ser pasados a la función lines

normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
}

# Área entre 106 a 110
normal_area(mean = 106, sd = 8, lb = 106, ub = 110, lwd = 2, acolor = "red")
text(82, 0.25, "106 a 110")

## Verificar el ajuste de la distribución normal de probabilidad
## para pas en mujeres

centro<-read_excel("centrosalud.xlsx")
head(centro)
centro1<-subset(centro, sexo=="femenino") 
head(centro1)  

# Gráfico de cuantil-cuantil
qqnorm(centro1$pas)
qqline(centro1$pas)
qqPlot(centro1$pas, xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")

h1<- graph.freq(centro1$pas, col="blue", frequency =3 , main="Densidad normal", density=4)
normal.freq(h1, col="red", lty=4,lwd=2, frequency=3)
dens1<-density(centro1$pas)
plot(dens1)

# Prueba de Shapiro & Wilk
shapiro.test(centro1$pas)

## Verificar el ajuste de la distribución normal de probabilidad
## para pas en hombres

centro2<-subset(centro, sexo=="masculino") 
head(centro2)  

# Gráfico de cuantil-cuantil
qqnorm(centro2$pas)
qqline(centro2$pas)
qqPlot(centro2$pas, xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")

h2<- graph.freq(centro2$pas, col="blue", frequency =3 , main="Densidad normal", density=4)
normal.freq(h2, col="red", lty=4,lwd=2, frequency=3)
dens2<-density(centro2$pas)
plot(dens2)

# Prueba de Shapiro & Wilk
shapiro.test(centro2$pas)

