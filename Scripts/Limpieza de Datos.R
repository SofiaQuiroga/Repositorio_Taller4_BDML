### Taller 4
## Clean the workspace
rm(list=ls())

## Cargar paquetes
require("pacman")
p_load(tidyverse, janitor, tm, stringi, tidytext, stopwords, wordcloud2, udpipe,
       ggcorrplot) 

## Cargar datos 
test <- read.csv(url("https://github.com/SofiaQuiroga/Repositorio_Taller4_BDML/blob/main/Data/test.csv?raw=true"),encoding = "UTF-8")
train <- read.csv(url("https://github.com/SofiaQuiroga/Repositorio_Taller4_BDML/blob/main/Data/train.csv?raw=true"),encoding = "UTF-8")

## Limpiar variable de texto "tweets"
# Todo en minuscula
test$text <- tolower(test$text)
train$text <- tolower(train$text)

# Eliminamos tildes
test$text <- stri_trans_general(str = test$text, id = "Latin-ASCII")
train$text <- stri_trans_general(str = train$text, id = "Latin-ASCII")

# Eliminamos caracteres especiales. Remplaza todo lo que no es alfanumericos por un espacio
test$text <- str_replace_all(test$text, "[^[:alnum:]]", " ")
train$text <- str_replace_all(train$text, "[^[:alnum:]]", " ")

# Eliminar números
test$text <- removeNumbers(test$text)
train$text <- removeNumbers(train$text)

# Eliminar puntuación 
test$text <- removePunctuation(test$text)
train$text <- removePunctuation(train$text)

# Eliminamos espacios extras
test$text <- gsub("\\s+", " ", str_trim(test$text))
train$text <- gsub("\\s+", " ", str_trim(train$text))

## Tokenizar
words_test <- test %>%
  unnest_tokens(output = "word", input = "text")

words_train <- train %>%
  unnest_tokens(output = "word", input = "text")

## Eliminar stopwords
sw <- c() # vector de todos los stopwords 
for (s in c("snowball", "stopwords-iso", "nltk")) {
  temp <- get_stopwords("spanish", source = s)$word
  sw <- c(sw, temp)
}

# Unir en el vector sw los stopwords de las diferentes fuentes
sw <- unique(sw) # borra las repeticiones
sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII")) # quitar tildes a stopwords
sw <- data.frame(word = sw) # guardar como objeto data frame

# Quitar stopwords 
words_test <- words_test %>%
  anti_join(sw, by = "word")

words_train <- words_train %>%
  anti_join(sw, by = "word")

# Nube de palabras con las 100 palabras más repetidas en la base de entrenamiento
n_words <- words_test %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(100)

wordcloud2(data = n_words)

# Nube de palabras con las 100 palabras más repetidas en la base de prueba
n_words2 <- words_train %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(100)

wordcloud2(data = n_words2)

## Buscar Missing values 
test[is.na(test), ]
train[is.na(train), ]

### Visualizar numero de tweets por personaje de interés
table(train$name)

# Cantidad de tweets por persona de interes
n_tweets = train %>% # analisis descriptivo por político
  group_by(name) %>%
  summarise(n = n()) %>%
  ungroup()

n_tweets

barplot(height = n_tweets$n, names = n_tweets$name, col = "#69b3a2", 
        xlab = "Personaje político", ylab =  "Número de tweets", ylim = c(0, 4000))
