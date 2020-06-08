require(data.table)
require(ggplot2)
require(verification)
library(Hmisc)
require(keras)
require(tidyverse)
set.seed(3)
load("C:/Users/EricaCastilho/Dropbox/Recrutamentos/Github/MOOC Dataset/MoocData.RData")


dados_estudantes<-data.table(x)
rm(x)

colnames(dados_estudantes)


dados_estudantes$grade<-as.numeric(dados_estudantes$grade)
hist(dados_estudantes$grade)

dados_estudantes$certified<-factor(dados_estudantes$certified)

hist(dados_estudantes[certified==1,grade])
ggplot(dados_estudantes,aes(x=certified,y=grade))+geom_boxplot()


ggplot(dados_estudantes,aes(x=certified,y=log(nplay_video)))+geom_boxplot()

ggplot(dados_estudantes,aes(x=certified,y=log(nevents)))+geom_boxplot()


ggplot(dados_estudantes,aes(x=certified,y=(ndays_act)))+geom_boxplot()


## Modelo logistico

modelo<-glm(certified~YoB+gender+grade+nevents+ndays_act+ LoE_DI +
              nplay_video+nchapters+nforum_posts,family = binomial(),data=dados_estudantes)


summary(modelo)

# index train

train<-sample(c(0,1),nrow(dados_estudantes),prob=c(0.2,0.8),replace=T)

prop.table((table(train)))


## FIltra dados


dados_filtrados<-dados_estudantes[,c('certified','YoB','gender','grade','nevents','ndays_act','LoE_DI', 
                                    'nplay_video','nchapters','nforum_posts','final_cc_cname_DI')]



## Imputacao

library(Hmisc)

apply(dados_filtrados,2,function(x) sum(is.na(x)))
colunas_faltantes<-colnames(dados_filtrados)[apply(dados_filtrados,2,function(x) sum(is.na(x)))>0]

# nao categoricas media
dados_filtrados$grade <- with(dados_filtrados, impute(grade, mean))
dados_filtrados$nevents <- with(dados_filtrados, impute(nevents, mean))
dados_filtrados$ndays_act <- with(dados_filtrados, impute(ndays_act, mean))
dados_filtrados$nplay_video <- with(dados_filtrados, impute(nplay_video, mean))
dados_filtrados$nchapters <- with(dados_filtrados, impute(nchapters, mean))


# Categoricas
dados_filtrados$YoB <- with(dados_filtrados, impute(YoB))
dados_filtrados$gender <- with(dados_filtrados, impute(gender))
dados_filtrados$LoE_DI <- with(dados_filtrados, impute(LoE_DI))


dados_filtrados$YoB<-factor(dados_filtrados$YoB)
dados_filtrados$gender<-factor(dados_filtrados$gender)
dados_filtrados$LoE_DI <- factor(dados_filtrados$LoE_DI)
dados_filtrados$final_cc_cname_DI<-factor(dados_filtrados$final_cc_cname_DI)



apply(dados_filtrados,2,function(x) sum(is.na(x)))

#save.image("Resultados_MoocHarvard.Rdata")


## Modelo Logistico
full.logit<-glm(data =dados_filtrados[train==1,],certified~.,family = binomial(link=logit))
summary(full.logit)

full.logit_predict<-predict(full.logit,type = "response")

## Taxa de acerto na base de teste
dados_test<-dados_filtrados[train==0]
dados_test$predito_logistic_prob<-predict(full.logit,dados_test[,-c('certified')],type = "response")
dados_test$predito_logisct<-ifelse(dados_test$predito_logistic_prob>0.5,1,0)


conf_matrix<-table(dados_test$predito_logisct,dados_test$certified)
precisao_logistica<-sum(diag(conf_matrix))/sum(conf_matrix)


## CUrva ROC
library(ROCR)
pred <- prediction(full.logit_predict,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


## Arvore de regressão

require(rpart)
require(rpart.plot)
full.rpart<-rpart(data = dados_filtrados[train==1],certified~.,method = 'class')
rpart.plot(full.rpart)

printcp(full.rpart)

rpart.prediction<-predict(full.rpart,type = 'prob')[,2]

pred <- prediction(rpart.prediction,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


## Taxa de acerto na base de teste

dados_test$predito_tree<-predict(full.rpart, newdata = dados_test[,-c('certified')], type = 'class')

matriz_confusao<-table(dados_test$predito_tree,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
precisao_tree<-sum(diag(matriz_confusao))/sum(matriz_confusao)

## Random forest
require(randomForest)


full.randomForest <- randomForest(certified ~ ., dados_filtrados[train==1], importance=TRUE,
                        proximity=TRUE)

full.randomForest<-randomForest(data = dados_filtrados[train==1,-c('YoB','final_cc_cname_DI')],certified~.,ntree=100)
#save(full.randomForest,file='Modelo_Random_forest.Rdata')

plot(full.randomForest)

dados_test$predito_randomforest<-predict(full.randomForest, newdata = dados_test[,-c('certified','YoB','final_cc_cname_DI')])

dados_test$predito_randomforest<-predict(full.randomForest, newdata = dados_test[,row.names(matrix_imp)])



varImpPlot(full.randomForest)


matrix_imp<-importance(full.randomForest)
row.names(matrix_imp)



dados_test[,row.names(matrix_imp),with=F]


dados_test$predito_randomforest<-predict(full.randomForest, newdata = dados_test[,row.names(matrix_imp),with=F])


matriz_confusao<-table(dados_test$predito_randomforest,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
precisao_randomForest<-sum(diag(matriz_confusao))/sum(matriz_confusao)


##   GBM
require(gbm)

model<-gbm(data=dados_filtrados[train==1],as.character(certified)~.,distribution = "bernoulli",n.trees = 10,
interaction.depth = 2,cv.folds = 2)

gbm.model.prediction<-predict(model,newdata = dados_filtrados[train==1,-c('certified')],n.trees = 10,type = 'response')

pred <- prediction(gbm.model.prediction,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

dados_test$predito_gbm_prob<-predict(model,newdata = dados_test[,-c('certified')],n.trees = 10,type = 'response')

dados_test$predito_gbm<-ifelse(dados_test$predito_gbm_prob>=0.5,1,0)


matriz_confusao<-table(dados_test$predito_gbm,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
precisao_gbm<-sum(diag(matriz_confusao))/sum(matriz_confusao)

## XG Boost
library(xgboost)
require(Matrix)
train_mat<-sparse.model.matrix(data = dados_filtrados[train==1,],certified~.-1)
head(train_mat)
test_mat<-sparse.model.matrix(data = dados_filtrados[train==0,],certified~.-1)
head(test_mat)
train_label<-as.numeric(dados_filtrados[train==1,]$certified)-1
test_label<-as.numeric(dados_filtrados[train==0]$certified)-1

# We need to conver data to DMatrix form
train_dMatrix<-xgb.DMatrix(data = as.matrix(train_mat),label=train_label)
test_dMatrix<-xgb.DMatrix(data = as.matrix(test_mat),label=test_label)

## Modeling
params <- list("objective" = "reg:logistic",
               "eval_metric" = "auc")
watchlist <- list(train = train_dMatrix, test = test_dMatrix)

# eXtreme Gradient Boosting Model
xgb_model <- xgb.train(params = params,
                       data = train_dMatrix,
                       nrounds = 2000,
                       watchlist = watchlist,
                       eta = 0.02,
                       max.depth = 4,
                       gamma = 0,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 222)

tunning<-as.data.frame(xgb_model$evaluation_log)
ggplot(data = NULL,aes(x = tunning$iter,y = tunning$train_auc,col='train'))+geom_line()+
  geom_line(aes(y = tunning$test_auc,col='test'))


imp <- xgb.importance(colnames(train_dMatrix), model = xgb_model)
print(imp)
xgb.plot.importance(imp)


# Curva roc

xgb.model.prediction<-predict(xgb_model,newdata = train_dMatrix)

pred <- prediction(xgb.model.prediction,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

dados_test$predito_xgb_prob<-predict(xgb_model,newdata = test_dMatrix)

dados_test$predito_xgb<-ifelse(dados_test$predito_xgb_prob>=0.5,1,0)


matriz_confusao<-table(dados_test$predito_xgb,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
precisao_xgb<-sum(diag(matriz_confusao))/sum(matriz_confusao)


## COmparando as precisoes
precisao_modelos<-data.frame(rbind(precisao_logistica,precisao_tree,
                                   precisao_randomForest,precisao_gbm,
                                   precisao_xgb))


colnames(precisao_modelos)<-'precisao'

precisao_modelos$modelo<-rownames(precisao_modelos)
ggplot(precisao_modelos,aes(modelo,precisao))+geom_col()

## Keras

final.training <- dados_filtrados[train==1,]
final.test <- dados_filtrados[train==0,]


X_train <- final.training %>% 
  select(-c(certified,YoB,gender,LoE_DI,final_cc_cname_DI)) %>% 
  scale()

y_train <- to_categorical(final.training$certified)



X_test <- final.test %>% 
  select(-c(certified,YoB,gender,LoE_DI,final_cc_cname_DI)) %>% 
  scale()

y_test <- to_categorical(final.test$certified)


model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  X_train, y_train, 
  epochs = 10, 
  batch_size = 5,
  validation_split = 0.3
)
summary(model)
resultados_keras<-model %>% evaluate(X_test, y_test)
