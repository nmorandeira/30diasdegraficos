## Acumulado de casos confirmados de COVID-19 en CABA y en Villas/Barrios vulnerables de CABA ####

## Fuente de los datos ####
# Total villas/barrios vulnerables: Boletín Epidemiológico CABA y/o La Garganta Poderosa (información en Twitter)
# Total CABA: Repositorio de Sistemas Mapache

## Lectura y preparación de datos ####
#lectura
library(readr)
covid19_villasCABA <- read_csv("covid19_villasCABA.csv")
covid19_villasCABA$fecha <- as.Date( ISOdate(covid19_villasCABA$anio, covid19_villasCABA$mes, covid19_villasCABA$dia) )
covid19_villasCABA$CABA_cum_sinvillas <- covid19_villasCABA$CABA_cum  - covid19_villasCABA$VillasCABA_cum


#tidy
library(tidyverse)
colnames(covid19_villasCABA)
covid19_villasCABA <- gather(covid19_villasCABA,Zona,CasosAcumulados,c(CABA_cum_sinvillas, VillasCABA_cum))
head(covid19_villasCABA)

#quitar días sin datos
covid19_villasCABA=na.omit(covid19_villasCABA) 

## Gráfico ####
library(ggplot2)
library(gridExtra)
library(wesanderson)

pal <- wes_palette("GrandBudapest1", n = 2)

p1 <- ggplot(data = covid19_villasCABA, aes(x = fecha, y = CasosAcumulados, fill = Zona)) +
  geom_col(show.legend = F) +
  geom_text(aes(label=CasosAcumulados), size = 3, position =  "stack")+
  scale_fill_manual(values = pal) +
  xlab("Fecha") +
  ylab("Casos acumulados de Covid-19") +
  scale_x_date(date_labels = "%d/%m/%Y", breaks = "day") +
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.caption = element_text(hjust = 0, vjust=-1, face = "italic"), plot.title = element_text(face = "bold"), legend.position="bottom", legend.title = element_blank()) +
  labs(title="Covid-19 en CABA y situación de barrios vulnerables", subtitle = "Total de casos confirmados acumulados") 

p1

p2<- ggplot(data = covid19_villasCABA, aes(x = fecha, y = CasosAcumulados/CABA_cum *100, fill = Zona)) +
   geom_col(show.legend = T) +
  geom_text(aes(label= round( CasosAcumulados/CABA_cum *100, 1)), position = "stack", size=3) +
  scale_fill_manual(values= pal, breaks=c("VillasCABA_cum","CABA_cum_sinvillas"), labels = c("Villas", "Otros barrios"), name="Residencia:  ") +
  xlab("Fecha") +
  ylab("Porcentaje sobre el total de casos") +
  scale_x_date(date_labels = "%d/%m/%Y", breaks = "day") +
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.caption = element_text(hjust = 0, vjust=-1, face = "italic"), plot.title = element_text(face = "bold"), legend.position="bottom") +
  labs(subtitle = "Porcentaje sobre el total de casos confirmados (%)", caption = "Bases de datos: La Garganta Poderosa + Boletín Epidemiológico CABA + Sistemas Mapache") 

p2

p <- grid.arrange(p1,p2)
p

ggsave("salida.png", plot = p, width = 8, height = 7, dpi = 300)
