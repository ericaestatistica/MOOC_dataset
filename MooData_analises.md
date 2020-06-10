---
title: "MOOC Dataset"
output: 
  html_document:
    keep_md: true

---

# Intruduction

The dropout rate for distance learning courses is very high and the success rate for students is low. Success here will be defined when the student receives the course certificate. Variables such as sex, education level, number of interactions with the platform and country of origin will be considered.



# Data

The data used were made available by the University of Havard and refer to a set of student results from the year 2013. The database can be accessed at the following address https://dataverse.harvard.edu/dataverse/mxhx

# Metodologies

Logistic Regression

Desicion Tree

Random Forest

Gradient Boosting

XGboost

Deep Learning (sequential model)


We will first load the necessary packages.


```r
require(data.table)
require(ggplot2)
require(verification)
require(Hmisc)
require(keras)
require(tidyverse)
require(ROCR)
require(rpart)
require(rpart.plot)
require(randomForest)
require(gbm)
require(xgboost)
require(Matrix)
```

The data set was made available in Rdata format and can be loaded using the following command.
To facilitate data manipulation, the table will be converted to data.table format.


```r
load("C:/Users/EricaCastilho/Dropbox/Recrutamentos/Github/MOOC Dataset/MoocData.RData")
dados_estudantes<-data.table(x)
rm(x)

# Fiz seed
set.seed(3)

# Convert grades in numeric
dados_estudantes$grade<-as.numeric(dados_estudantes$grade)

# Convert cerfified in factor
dados_estudantes$certified<-factor(dados_estudantes$certified)
```

Now we are going to filter in the database only those variables that are possible features of our model: year of birth, sex, grade in the course, number of events in the course, number of active days, degree of education of the student, number of times he attended videos, number of chapters visited, number of forum posts and student country.



```r
dados_filtrados<-dados_estudantes[,c('certified','YoB','gender','grade','nevents','ndays_act','LoE_DI','nplay_video','nchapters','nforum_posts','final_cc_cname_DI')]
```

The database presents a large number of missing data. Therefore, we will use an imputation method to solve this problem. The Hmisc package will be used here. For the numerical variables we will use the mean as an imputation function and for the categorical variables the package used the Fisher’s optimum scoring method.


```r
#Quantitative variables
dados_filtrados$grade <- with(dados_filtrados, impute(grade, mean))
dados_filtrados$nevents <- with(dados_filtrados, impute(nevents, mean))
dados_filtrados$ndays_act <- with(dados_filtrados, impute(ndays_act, mean))
dados_filtrados$nplay_video <- with(dados_filtrados, impute(nplay_video, mean))
dados_filtrados$nchapters <- with(dados_filtrados, impute(nchapters, mean))


# Quallitative variables
dados_filtrados$YoB <- with(dados_filtrados, impute(YoB))
dados_filtrados$gender <- with(dados_filtrados, impute(gender))
dados_filtrados$LoE_DI <- with(dados_filtrados, impute(LoE_DI))

# Transform qualitative into factors
dados_filtrados$YoB<-factor(dados_filtrados$YoB)
dados_filtrados$gender<-factor(dados_filtrados$gender)
dados_filtrados$LoE_DI <- factor(dados_filtrados$LoE_DI)
dados_filtrados$final_cc_cname_DI<-factor(dados_filtrados$final_cc_cname_DI)
```

In order to fit and evaluate the models we will divide the base into a set of tests (20%) and training (80%).


```r
# index train

train<-sample(c(0,1),nrow(dados_estudantes),prob=c(0.2,0.8),replace=T)
```

# Logistic model


The first model we fit is the simplest one, the logistic model. We will then estimate the values for the test data , calculate the accuracy of the model and make its ROC curve



```r
## Logistic Model
full.logit<-glm(data =dados_filtrados[train==1,],certified~.,family = binomial(link=logit))


full.logit_predict<-predict(full.logit,type = "response")

## Predictions
dados_test<-dados_filtrados[train==0]
dados_test$predito_logistic_prob<-predict(full.logit,dados_test[,-c('certified')],type = "response")
dados_test$predito_logisct<-ifelse(dados_test$predito_logistic_prob>0.5,1,0)

## Accuracy
conf_matrix<-table(dados_test$predito_logisct,dados_test$certified)
precisao_logistica<-sum(diag(conf_matrix))/sum(conf_matrix)


## ROC curve
pred <- prediction(full.logit_predict,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
```

![](MooData_analises_files/figure-html/logistic-1.png)<!-- -->


The accuracy for this model turns out to be 99.8406%.

# Regression Tree

Now let's move on to decision tree based models. The first one will be the simplest, regression tree.


```r
# Fit the model
full.rpart<-rpart(data = dados_filtrados[train==1],certified~.,method = 'class')
#Plot the tree
rpart.plot(full.rpart)
```

![](MooData_analises_files/figure-html/regression_tree-1.png)<!-- -->

```r
printcp(full.rpart)
```

```
## 
## Classification tree:
## rpart(formula = certified ~ ., data = dados_filtrados[train == 
##     1], method = "class")
## 
## Variables actually used in tree construction:
## [1] grade     nchapters
## 
## Root node error: 5159/270456 = 0.019075
## 
## n= 270456 
## 
##         CP nsplit rel error   xerror      xstd
## 1 0.920333      0  1.000000 1.000000 0.0137891
## 2 0.011372      1  0.079667 0.079667 0.0039267
## 3 0.010000      4  0.045551 0.052336 0.0031835
```

```r
rpart.prediction<-predict(full.rpart,type = 'prob')[,2]

pred <- prediction(rpart.prediction,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
```

![](MooData_analises_files/figure-html/regression_tree-2.png)<!-- -->

```r
## Predictions

dados_test$predito_tree<-predict(full.rpart, newdata = dados_test[,-c('certified')], type = 'class')

matriz_confusao<-table(dados_test$predito_tree,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
```

```
## [1] 0.9991294
```

```r
precisao_tree<-sum(diag(matriz_confusao))/sum(matriz_confusao)
```

The accuracy for this model turns out to be 99.9129%.

# Random Forest


We will now adjust a model that an ensemble of the decision trees, Random Forest.
Two variables had to be discarded to fit this model, as it does not accept qualitative variables with a very high number of categories.



```r
# Fit the model
full.randomForest<-randomForest(data = dados_filtrados[train==1,-c('YoB','final_cc_cname_DI')],certified~.,ntree=100)

plot(full.randomForest)
```

![](MooData_analises_files/figure-html/random_forest-1.png)<!-- -->

```r
dados_test$predito_randomforest<-predict(full.randomForest, newdata = dados_test[,-c('certified','YoB','final_cc_cname_DI')])

# Plot importance of variables
varImpPlot(full.randomForest)
```

![](MooData_analises_files/figure-html/random_forest-2.png)<!-- -->

```r
matriz_confusao<-table(dados_test$predito_randomforest,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
```

```
## [1] 0.9994983
```

```r
precisao_randomForest<-sum(diag(matriz_confusao))/sum(matriz_confusao)
```

The accuracy for this model turns out to be 99.9498%.

# Gradient Boosting


Now let's fit the Gradient Boosting model. 
It also consists of an ensemble method, but seeks to reduce errors in the training base at each step of the algorithm.



```r
# Fit the model
model<-gbm(data=dados_filtrados[train==1],as.character(certified)~.,distribution = "bernoulli",n.trees = 10,
interaction.depth = 2,cv.folds = 2)

# Predictions
gbm.model.prediction<-predict(model,newdata = dados_filtrados[train==1,-c('certified')],n.trees = 10,type = 'response')

pred <- prediction(gbm.model.prediction,dados_filtrados[train==1,certified])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
```

![](MooData_analises_files/figure-html/gradient_boosting-1.png)<!-- -->

```r
dados_test$predito_gbm_prob<-predict(model,newdata = dados_test[,-c('certified')],n.trees = 10,type = 'response')

dados_test$predito_gbm<-ifelse(dados_test$predito_gbm_prob>=0.5,1,0)


matriz_confusao<-table(dados_test$predito_gbm,dados_test$certified)
precisao_gbm<-sum(diag(matriz_confusao))/sum(matriz_confusao)
```

The accuracy for this model turns out to be 99.8333%.

#XGboost Model


We will now adjust a more advanced model than Gradient Boosting, XGboost.
To adjust this model, the test and training matrices need to go through some pre-processing.


```r
# Pre-processing matrices
train_mat<-sparse.model.matrix(data = dados_filtrados[train==1,],certified~.-1)

test_mat<-sparse.model.matrix(data = dados_filtrados[train==0,],certified~.-1)

train_label<-as.numeric(dados_filtrados[train==1,]$certified)-1
test_label<-as.numeric(dados_filtrados[train==0]$certified)-1

# We need to convert data to DMatrix form
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
                       seed = 222,verbose=0)

# Prediction

dados_test$predito_xgb_prob<-predict(xgb_model,newdata = test_dMatrix)

dados_test$predito_xgb<-ifelse(dados_test$predito_xgb_prob>=0.5,1,0)


matriz_confusao<-table(dados_test$predito_xgb,dados_test$certified)
sum(diag(matriz_confusao))/sum(matriz_confusao)
```

```
## [1] 0.9995721
```

```r
precisao_xgb<-sum(diag(matriz_confusao))/sum(matriz_confusao)
```

The accuracy for this model turns out to be 99.9572%.

# Deep Learning


Finally, we will use a deep neural network. This model will be adjusted using the Keras package.


```r
# Pre-processing data
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
```

```
## Model: "sequential"
## ________________________________________________________________________________
## Layer (type)                        Output Shape                    Param #     
## ================================================================================
## dense (Dense)                       (None, 256)                     1792        
## ________________________________________________________________________________
## dropout (Dropout)                   (None, 256)                     0           
## ________________________________________________________________________________
## dense_1 (Dense)                     (None, 128)                     32896       
## ________________________________________________________________________________
## dropout_1 (Dropout)                 (None, 128)                     0           
## ________________________________________________________________________________
## dense_2 (Dense)                     (None, 2)                       258         
## ================================================================================
## Total params: 34,946
## Trainable params: 34,946
## Non-trainable params: 0
## ________________________________________________________________________________
```

```r
resultados_keras<-model %>% evaluate(X_test, y_test)
```
The accuracy for this model turns out to be 99.2127%.


# Best model and app deployment


The model that presented the best results was the XGboost. It was then used to make predictions and create the App available at https://naiufop.shinyapps.io/App_Mooc/
