## Descriptores de muestras - Mirando histogramas ####
#Skewness and Kurtosis #
#Asimetría y Curtosis

# para una distribución normal, se espera skewness cercano a 0 y kurtosis cercano a 3
#Pearson’s measure of kurtosis 
library(moments)

# está muy desprolijo este código. hice un loop para calcular kurtosis y asimetría para cada una de las muestras


library(ggplot2)
# datos tiene una columna de Skewness y otra columna de Kurtosis para N = 77 filas, cada una correspondiente a una muestra. Además en mi caso hay otra columna con una variable del tipo factor ("Species") y otras columnas que caracterizan a las 77 muestras.

p <- ggplot(data = datos,  aes(x =  Skewness, y = Kurtosis)) + 
  geom_bin2d(binwidth=0.1) +
  #facet_grid(. ~ Species) +
  scale_fill_gradient(low = "steelblue",high = "red") +
  geom_hline(yintercept = 3, linetype= "dashed", size = 0.25) +
  geom_vline(xintercept = 0, linetype= "dashed", size = 0.25) +
  theme_bw() +
  labs(x = "Asimetría (Skewness)", y = "Kurtosis", title = "Descriptores de un conjunto de muestras", caption = "Librerías: moments y ggplot2")

p

ggsave("salida.png", plot = p, width = 7, height = 7, dpi = 300)
