# Dr. Byron González
# http://www.byrong.tk 

#==================== Distribución Binomial ====================

# En una población 10% es daltónico, en una muestra aleatoria 
# de 25 personas
# Calcular las siguientes probabilidades

# 1. Valores de la distribución binomial para n=25 p=0.1 q=0.9
tabla.p=data.frame(Probability=dbinom(0:25, size=25, prob=0.10))
rownames(tabla.p) <- 0:25 
print(tabla.p)

# 2. Calcular probabilidades puntuales. Probabilidad de encontrar
# ningún daltónico
# Usar dbinom para valores puntuales de probabilidad
P0<-dbinom(0,25,0.10); P0

# 2. ¿Cuál es la probabilidad de encontrar 5 o menos daltónicos?
# P(X=0)
# Usar pbinom para probabilidades acumuladas
# Si es cola izquierda es desde el punto fijado hacia abajo
# Si es cola derecha debe ser un valor abajo del punto de interés
# P(X<=5)
P5omenos<-pbinom(c(5), size=25, prob=0.10, lower.tail=TRUE); P5omenos


# 3. ¿Cuál es la probabilidad de encontrar 6 personas daltónicas o más?
# Si es cola derecha debe ser un valor abajo del punto de interés
# En este caso, como es 6 el interés, debe solicitarse para 5 y cola derecha
P6omas<-pbinom(5,25,0.10, lower.tail = F); P6omas

# 4. ¿Cuál es la probabilidad de encontrar entre 6 y 9 personas daltónicas?

P6y9<-pbinom(9,25,0.1, lower.tail = T)-pbinom(5,25,0.1, lower.tail = T); P6y9
p6y9a<-sum(dbinom(6:9,25,0.1)); p6y9a

# 5. ¿Cuál es la probabilidad de encontrar entre 2 y 4 personas daltónicas?
p2o3o4<-sum(dbinom(2:4,25,0.1)); p2o3o4 


# 5. Gráfica de la distribución
ensayos<-0:25
plot(ensayos, dbinom(ensayos,size=25, prob=0.10), type = "h", xlab = "Número de éxitos", ylab="Probabilidad")


## ============= Distribución de Poisson ====================

# dpois para valores puntuales
# ppois para valores acumulados
# qpois para cuantiles

# Si el número promedio de accidentes graves por año en una
# fábrica grande (donde el número de empleados es constante) 
# es de cinco, calcule la probabilidad de que en el año en curso haya:

# Tabla de distribución de probabilidades

ppois(c(20), lambda=5, lower.tail = T)
tabla.ps=data.frame(Probability=dpois(0:20, lambda = 5))
rownames(tabla.ps) <- 0:20 
print(tabla.ps)

# Gráfica de la distribución
x <- 0:20
lambda <- 5
plot(dpois(x, lambda), type = "h", lwd = 2,
     main = "Gráfica de la distribución de probabilidad",
     ylab = "P(X = x)", xlab = "Número de accidentes")

# Calcular la probabilidad de cero accidentes
P0<-dpois(0,5); P0

# Calcular la probabilidad de exactamente siete accidentes
P7<-dpois(7,5); P7

# Calcular la probabilidad de ocurrencia de 10 o más accidentes
P10mas<-ppois(c(9), lambda = 5, lower.tail = F); P10mas

# Calcular la probabilidad de ocurrencia de 5 accidentes o menos
P5menos<-ppois(5,5, lower.tail = T); P5menos

# Calcular la probabilidad de que ocurran entre 3 a 7 accidentes
P3a7<-sum(dpois(3:7,5)); P3a7

# Gráfica del segmento de probabilidad que corresponde de 3 a 7

# Antes agregar la siguiente función
# lambda: media
# lb: límite inferior de la suma
# ub: límite superior de la suma
# col: color
# lwd: ancho de línea

pois_sum <- function(lambda, lb, ub, col = 4, lwd = 1, ...) {
  x <- 0:(lambda + lambda * 2)
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  plot(dpois(x, lambda = lambda), type = "h", lwd = lwd, ...)
  
  if(lb == min(x) & ub == max(x)) {
    color <- col
  } else {
    color <- rep(1, length(x))
    color[(lb + 1):ub ] <- col
  }
  
  lines(dpois(x, lambda = lambda), type = "h",
        col =  color, lwd = lwd, ...)
}

# Gráfico de probabilidad cuando hay 3 a 7 accidentes
pois_sum(lambda = 5, lb = 2, ub = 7, lwd = 2,
         ylab = "P(X = x)", xlab = "Número de granos quebrados")


