### Intereses de búsqueda en un año electoral - Argentina 2019 #########
# Fuente: Google Trends
# Búsquedas: Alberto + Alberto Fernández, Macri + Mauricio Macri, dolar

# Carga de datos y tidy
library(readr)
googletrends <- read_csv("data/googletrends.csv", 
                         trim_ws = FALSE, skip = 2)

library(tidyverse)
library(janitor)

googletrends = clean_names(googletrends)
colnames(googletrends)


busquedas <-
googletrends %>%
  mutate(Alberto1 = str_replace(alberto_fernandez_argentina, "<1", "0")) %>%
  mutate(Alberto2 = str_replace(alberto_argentina, "<1", "0")) %>%
  mutate(Macri1 = str_replace(macri_argentina, "<1", "0")) %>%
  mutate(Macri2 = str_replace(mauricio_macri_argentina, "<1", "0")) %>%
  mutate(Dolar = str_replace(dolar_argentina, "<1", "0"))

busquedas <-
  busquedas %>%
  mutate(Alberto = as.numeric(Alberto1) + as.numeric(Alberto2)) %>%
  mutate(Macri = as.numeric(Macri1) + as.numeric(Macri2)) %>%
  mutate(Dolar = as.numeric(Dolar)) %>%
  select(week, Alberto, Macri, Dolar)

colnames(busquedas)


busquedas <-
  busquedas %>%
  pivot_longer(cols = c(Alberto, Macri, Dolar), names_to = "Busqueda")

busquedas$Busqueda <- factor(busquedas$Busqueda, levels = c("Dolar", "Alberto", "Macri"))
levels(busquedas$Busqueda)

glimpse(busquedas)

#agrupar semanas en meses
busquedas <- busquedas %>% 
  mutate(mes = format(week, "%m"))

glimpse(busquedas)

  
# Gráfico de Florence Nightingale
# script basado en código de AnguloB https://github.com/AnguloB/datosdemiercoles/blob/master/00_30diasDeGraficos/30_nightingale/30_nightingale.R

p <- ggplot(data = busquedas, aes(x=mes, y = value, fill = Busqueda))+
  geom_col()+coord_polar()+
  #scale_fill_manual(values= palette30)+
  theme_bw() +
  labs(x="", y="", fill="", 
       title= "Un año electoral en Argentina - 2019", 
       subtitle = "Interés relativo de términos de búsqueda en Google", caption = 
          "Términos en Google Trends: Alberto + 'Alberto Fernández', Macri + 'Mauricio Macri', dolar") +
  scale_fill_manual(values = c("#009999", "#cc0000", "yellow"), labels=c("Dólar", "Alberto Fernández", "Mauricio Macri"), name="Búsquedas") +
  scale_x_discrete(labels=c("Enero","Febrero","Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))

p

# Guardar gráfico
ggsave(filename = "salida.png",p,width = unit(13,"cm") ,height = unit(6.5,"cm")) # medidas tw
ggsave(filename = "salida.png",p,width = unit(9,"cm") ,height = unit(7,"cm"), dpi = 150) # medidas más cuadradas  
