
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

poblacion <- read_excel("Cat_Poblacion.xlsx", range = "A2:C21", col_names = c("Edad","Hombre","Mujer"))


#Con la función mutate recalculamos una nueva variable a partir de las ya existentes
#Creamos una nueva columna con el porcentaje de cada edad segun su sexo

p <- poblacion %>%
  mutate(H=Hombre/sum(Hombre)*100,M=Mujer/sum(Mujer)*100) %>%
  select(Edad,H,M)

lab_edad <- c(paste(seq(from = 0,to = 90,by = 5),seq(from = 4,to = 94,by = 5),sep=" - "),
              "más de 95")
p$Edad <- factor(p$Edad,labels=lab_edad)


#Con la función gather unimos múltiples columnas por un valor clave, en este caso segun la variable sexo

pop <- p %>% gather(sexo,p_edad,-1)
ggplot(pop,aes(x=Edad,fill=sexo,
               
               y=ifelse(sexo=="H",-p_edad,p_edad)))+
  
  geom_bar(stat="identity") +
  scale_y_continuous(limits=max(pop$p_edad)*c(-1,1),labels=abs)+
  labs(y="Porcentaje de H/M por Edades Quinquenales")+
  ggtitle("Piramide Poblacional de Catalunya 2019")+
  coord_flip()+
  theme_classic()

