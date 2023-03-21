#Tensorflow y Keras
rm(list=ls())
install.packages("tensorflow")
library(reticulate)
path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)

library(tensorflow)
install_tensorflow(envname = "r-reticulate")

install.packages("keras")
library(keras)
install_keras(envname = "r-reticulate")

#TF IDF REDUCIDO 

columnas_seleccionadas <- colSums(tf_idf_train) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(2000) %>%
  rownames()

train_tf_idf_reducido <- tf_idf_train %>%
  select(all_of(columnas_seleccionadas))

columnas_seleccionadas2 <- colSums(tf_idf_test) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(2000) %>%
  rownames()

test_tf_idf_reducido <- tf_idf_test %>%
  select(all_of(columnas_seleccionadas2))
test_tf_idf_reducido2<-as.matrix(test_tf_idf_reducido)

dim(test_tf_idf_reducido2)

#MODELOS
Ytrain <- factor(train_clean$name)
Ytrain<- model.matrix(~Ytrain+0)
head(Ytrain)
dim(Ytrain)
class(Ytrain)

Xtrain <- as.matrix(train_tf_idf_reducido)
dim(Xtrain)

#Modelo de prueba

model <- keras_model_sequential() 
model1 <- keras_model_sequential() 

model1 %>% 
  layer_dense(units = 100, activation = 'relu',input_shape = c(2000)) %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 70, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model1)

model1 %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('CategoricalAccuracy')
)
summary(model1)

model1 %>% fit(
  Xtrain, Ytrain, 
  epochs = 5, 
  batch_size = 2^4,
  validation_split = 0.2
)


#Predicciones

yhat1 <- model1  %>% predict(test_tf_idf_reducido2) 

prediccion<- data.frame(test_clean$id,yhat1)
yhat1<-data.frame(yhat1)

max_col <- max.col(yhat1, ties.method = "last")
col_names <- names(yhat1)[max_col]
prediccion$name <- col_names
prediccion$name <- factor(prediccion$name, levels = c("X1", "X2", "X3"), labels = c("Lopez", "Petro", "Uribe"))
prediccion<- prediccion[,-c(2,3,4)]
names(prediccion)[1] <- "id"

#Corrección al tweet de solo emojis
test2 <- read.csv(url("https://github.com/SofiaQuiroga/Repositorio_Taller4_BDML/blob/main/Data/test.csv?raw=true"),encoding = "UTF-8")
observacion_faltante <- setdiff(test2$id,prediccion$id)
observacion_faltante
extra<- data.frame(id= "cb9ac947c675464803342fc9", name = "Lopez")
prediccion <- rbind(prediccion, extra)

write.csv(prediccion, 'Modelo_RedNeuronal1.csv',row.names=FALSE) 

#Modelo de prueba
model <- keras_model_sequential() 
model3 <- keras_model_sequential() 

model3 %>% 
  layer_dense(units = 100, activation = 'relu',input_shape = c(2000)) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model3)

model3 %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('CategoricalAccuracy')
)


model3 %>% fit(
  Xtrain, Ytrain, 
  epochs = 4, 
  batch_size = 2^4,
  validation_split = 0.3
)
#Categorical Accuracy no cambia

#Modelo de prueba
model <- keras_model_sequential() 
model4 <- keras_model_sequential() 

model4 %>% 
  layer_dense(units = 200, activation = 'relu',input_shape = c(2000)) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model4)

model4 %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('CategoricalAccuracy')
)


model4 %>% fit(
  Xtrain, Ytrain, 
  epochs = 5, 
  batch_size = 2^5,
  validation_split = 0.3
)

#Predicciones

yhat2 <- model4  %>% predict(test_tf_idf_reducido2) 

prediccion2<- data.frame(test_clean$id,yhat2)
yhat2<-data.frame(yhat2)

max_col <- max.col(yhat2, ties.method = "last")
col_names <- names(yhat2)[max_col]
prediccion2$name <- col_names
prediccion2$name <- factor(prediccion2$name, levels = c("X1", "X2", "X3"), labels = c("Lopez", "Petro", "Uribe"))
prediccion2<- prediccion2[,-c(2,3,4)]
names(prediccion2)[1] <- "id"

#Corrección al tweet de solo emojis
test2 <- read.csv(url("https://github.com/SofiaQuiroga/Repositorio_Taller4_BDML/blob/main/Data/test.csv?raw=true"),encoding = "UTF-8")
observacion_faltante <- setdiff(test2$id,prediccion$id)
observacion_faltante
extra<- data.frame(id= "cb9ac947c675464803342fc9", name = "Lopez")
prediccion2 <- rbind(prediccion2, extra)

write.csv(prediccion2, 'Modelo_RedNeuronal2.csv',row.names=FALSE) 

