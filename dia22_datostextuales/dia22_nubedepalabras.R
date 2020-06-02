#Nube de palabras de la canción "Me gritaron Negra" de Victoria Santa Cruz
#Link al tema: https://www.youtube.com/watch?v=cHr8DTNRZdg

## librerías necesarias
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#preparación de datos
library(readr)
letra <- readLines(file.choose())  #se abre ventana para elegir el archivo de texto
str(letra)

# convertir en cuerpo
docs <- Corpus(VectorSource(letra))

# reemplazo de caracteres especiales 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# convertir a minusculas
#docs <- tm_map(docs, content_transformer(tolower))

#convertir a mayusculas
docs <- tm_map(docs, content_transformer(toupper))

# quitar numeros
docs <- tm_map(docs, removeNumbers)
# quitar palabras comunes en inglés o español "stopwords" 
docs <- tm_map(docs, removeWords, stopwords("spanish"))

# quitar palabras especificas
#docs <- tm_map(docs, removeWords, c("palabra1", "palabra2", "palabra3")) 

# quitar puntuación
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, toSpace, "¿")
docs <- tm_map(docs, toSpace, "¡")
docs <- tm_map(docs, stripWhitespace)

# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs,  control = list(tolower = FALSE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100) #primeras 100 apariciones

set.seed(1235)
set.seed(1)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=F, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#grafico mejorardo - pero hay mucha desproporción en las escalas
library(wordcloud2)
a<- wordcloud2(d,color='random-light', backgroundColor="black", minSize = 2, size= 1)
a
b<- wordcloud2(d,color='random-light', backgroundColor="black", minSize = 1, size= 5)







