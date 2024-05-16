#Autor: Michelle Clempner
#Calculo 2DCT

library(pacman)

library(dplyr)

p_load("readr", #Llamar bases de datos
       "ggplot2",#Graficar
       "dplyr" ) #facilita el manejo de datos

datos<-read_csv(file="https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/examen2")
head(datos)

#Extracción de genes control (referencia)
Controles<- datos%>%
  filter(Condicion=="Control")

head(Controles)

#Sacar promedios

promedio_controles <- Controles %>%
  summarise(Mean_C1= mean (Cx1),
            Mean_C2= mean (Cx2), 
            Mean_C3= mean (Cx3), 
            Mean_T1= mean (T1), 
            Mean_T2= mean (T2), 
            Mean_T3= mean (T3)) %>%
  mutate(Gen="Promedio_controles") %>% #Generar columna "gen promedio controles"
  select(7,1,2,3,4,5,6)
promedio_controles

#Extraer los  genes de la tabla "datos"

genes<-datos %>%
  filter(Condicion=="Target")  %>%
  select(-2)
head(genes)

#Sacar 2^^-deltaCT

DCT<- genes%>%
  mutate(DCT_C1=2^-(Cx1-promedio_controles$Mean_C1),
         DCT_C2=2^-(Cx2-promedio_controles$Mean_C2),
 DCT_C3=2^-(Cx3-promedio_controles$Mean_C3),
  DCT_T1=2^-(T1-promedio_controles$Mean_T1), 
   DCT_T2=2^-(T2-promedio_controles$Mean_T2),
    DCT_T3=2^-(T3-promedio_controles$Mean_T3)) %>% 
      select(-2,-3,-4,-5,-6,-7)
  DCT
  
  promedio_genes<-DCT %>%
    mutate(Mean_DCT_Cx=(DCT_C1+DCT_C2+DCT_C3)/3,
          Mean_DCT_Tx=(DCT_T1+DCT_C2+DCT_T3)/3)
promedio_genes  

