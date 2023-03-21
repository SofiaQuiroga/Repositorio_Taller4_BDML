#train
model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
palabras_unicas <- words_train %>%
  distinct(word)
udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
udpipe_results <- as_tibble(udpipe_results)
udpipe_results <- udpipe_results %>% 
  select(token, lemma) %>%
  rename("word" = "token")
words_train <- words_train %>%
  left_join(udpipe_results, by = "word", multiple = "all")
words_train[is.na(words_train$lemma), "lemma"] <- words_train[is.na(words_train$lemma), "word"]
#test
palabras_unicas2 <- words_test %>%
  distinct(word)
udpipe_results2 <- udpipe_annotate(model, x = palabras_unicas2$word)
udpipe_results2 <- as_tibble(udpipe_results2)
udpipe_results2 <- udpipe_results2 %>% 
  select(token, lemma) %>%
  rename("word" = "token")
words_test <- words_test %>%
  left_join(udpipe_results, by = "word", multiple = "all")
words_test[is.na(words_test$lemma), "lemma"] <- words_test[is.na(words_test$lemma), "word"]

#eliminamos las palabras que menos aparecen
palabras_eliminar <- words_train %>%
  count(lemma) %>%
  filter(n < 5)

words_train <- words_train %>%
  anti_join(palabras_eliminar, by = "lemma") 


#Volvemos a nuestro formato original. Comentario por tweets
train_clean <- words_train %>%
  group_by(id,name) %>% 
  summarise(text = str_c(lemma, collapse = " ")) %>%
  ungroup()

test_clean <- words_test %>%
  group_by(id) %>% 
  summarise(text = str_c(lemma, collapse = " ")) %>%
  ungroup()

#### creamos la matriz TF-IDF
bigramas_train <- train_clean %>%
  unnest_tokens(bigramas, text, token = "ngrams", n = 2)

tdm_train <- bigramas_train %>%
  count(id, bigramas, sort = TRUE) %>%
  cast_dtm(id, bigramas, n)
tfidf_train <- tdm_train %>%
  weightTfIdf()

tfidf_train <- as.matrix(tfidf_train)
tfidf_train<- data.frame(tfidf_train)
  


bigramas_test <- test_clean %>%
  unnest_tokens(bigramas, text, token = "ngrams", n = 2)

tdm_test <- bigramas_test %>%
  count(id, bigramas, sort = TRUE) %>%
  cast_dtm(id, bigramas, n)
tfidf_test <- tdm_test %>%
  weightTfIdf()

tfidf_test <- as.matrix(tfidf_test)
tfidf_test<- data.frame(tfidf_test)

 

##

#dismunuimos las columnnas
columnas_seleccionadas <- colSums(tfidf_train) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(500) %>%
  rownames()

train_tf_idf_reducido <- tfidf_train %>%
  select(all_of(columnas_seleccionadas))

columnas_seleccionadas2 <- colSums(tfidf_test) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(500) %>%
  rownames()

test_tf_idf_reducido <- tfidf_test %>%
  select(all_of(columnas_seleccionadas2))
test_tf_idf_reducido2<-as.matrix(test_tf_idf_reducido)

dim(test_tf_idf_reducido2)
#
train_Y <- factor(train_clean$name)
train_Y <- model.matrix(~train_Y+0)
head(train_Y)
dim(train_Y)
class(train_Y)

train_X <- as.matrix(train_tf_idf_reducido)
dim(train_X)
##
############ Armamos el modelo
rm(model2)

model <- keras_model_sequential() 
model2 <- keras_model_sequential() 

model2 %>% 
  layer_dense(units = 500, activation = 'relu',input_shape = c(500)) %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 300, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.01) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model2)

model2 %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('CategoricalAccuracy')
)
summary(model2)

model2 %>% fit(
  train_X, train_Y, 
  epochs = 15, 
  batch_size = 100,
  validation_split = 0.2
)

#Predecimos 

y_hat <- model2  %>% predict(test_tf_idf_reducido2) 
predict<- data.frame(test_clean$id,y_hat)
y_hat<-data.frame(y_hat)

max_col <- max.col(y_hat, ties.method = "last")
col_names <- names(y_hat)[max_col]
predict$name <- col_names
predict$name <- factor(predict$name, levels = c("X1", "X2", "X3"), labels = c("Lopez", "Petro", "Uribe"))
predict<- predict[,-c(2,3,4)]
names(predict)[1] <- "id"
###################### Correacción al tweet de solo emojis
test2 <- read.csv(url("https://github.com/SofiaQuiroga/Repositorio_Taller4_BDML/blob/main/Data/test.csv?raw=true"),encoding = "UTF-8")
observacion_faltante <- setdiff(test2$id,predict$id)
observacion_faltante
extra<- data.frame(id= "cb9ac947c675464803342fc9", name = "Lopez")
predict <- rbind(predict, extra)
#######################################
write.csv(predict, 'R_Neuronal7.csv',row.names=FALSE) 
