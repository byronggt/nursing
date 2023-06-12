# Dr. Byron González
# http://www.byrong.tk 

if(!require(readxl)){install.packages("readxl")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(rempsyc)){install.packages("rempsyc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}

# Abrir la tabla de datos "centrosalud" -----
centro<-read_excel("centrosalud.xlsx")
head(centro)