# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(gtsummary)){install.packages("gtsummary")}
if(!require(DataExplorer)){install.packages("DataExplorer")}
if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(carData)){install.packages("carData")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(explore)){install.packages("explore")}
if(!require(dplyr)){install.packages("dplyr")}

# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)
introduce(centro)

# Resumen gráfico porcentual de variables y datos perdidos
plot_intro(centro)

# Resumen porcentual de datos perdidos
plot_missing(centro)

# Histograma de variables cuantitativas
plot_histogram(centro)

# Diagramas de caja para variables cuantitativas
data_box1<-centro[c("sexo","edad")]
plot_boxplot(data_box1, by="sexo")

# Resumen para edad 

numSummary(centro[,c("edad"), drop=FALSE]
           , groups=centro$tabaco
           , statistics=c("mean", "sd", "IQR", "skewness"))

# Resumen simple de variables

c1<-centro %>% 
  select(tabaco, pad) %>% 
  group_by(tabaco) %>% 
  arrange(desc(pad), .by_group = T) %>% 
  slice(1)
c1


c2<-centro %>% 
  select(tabaco, pad) %>% 
  group_by(tabaco) %>% 
  arrange(desc(pad), .by_group = T)
c2  


c3<-centro %>% 
  filter(laboro=="jubilado")
c3

# Calcular nuevamente el IMC como imc1
c4<-centro %>% 
  mutate(centro, imc1=peso/(talla/100)^2) %>% 
  select(edad:imc, imc1)
c4

# Seleccionar los estudiantes con imc1 superiores a 25 para laboro
c5<-centro %>% 
  mutate(centro, imc1=peso/(talla/100)^2) %>% 
  select(laboro, imc1) %>% 
  filter(imc1>25)
c5

# Calcular el imc promedio por laboro y superior a 20
c6<-centro %>% 
  group_by(laboro) %>% 
  summarise(conteo=n(),imc=mean(imc)) %>% 
  filter(conteo>1,imc>20)
c6

c6 %>% 
  flextable() %>% 
  autofit %>% 
  save_as_docx(path = "imc_20.docx")

