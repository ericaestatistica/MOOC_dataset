library(xgboost)
require(Matrix)
require(ggplot2)
load("C:/Users/EricaCastilho/Dropbox/Recrutamentos/Github/MOOC Dataset/Resultados_MoocHarvard.Rdata")


dados_novos<-dados_filtrados[train==0,]

n=1000
dados_novos<-dados_novos[1:n,]
dados_novos$certified<-sample(c(0,1),n, replace=T)
dados_novos$grade<-seq(0,1,length=n)
dados_novos$nevents<-seq(0,n-1,by=1)
dados_novos$ndays_act<-seq(0,n-1,by=1)
dados_novos$nplay_video<-seq(0,n-1,by=1)
dados_novos$nchapters<-seq(0,n-1,by=1)
dados_novos$nforum_posts<-seq(0,n-1,by=1)


test_mat<-sparse.model.matrix(data = dados_novos,certified~.-1)
head(test_mat)
test_label=as.vector(dados_novos[,certified])
test_dMatrix<-xgb.DMatrix(data = as.matrix(test_mat),label=test_label)
dados_novos$predito<-predict(xgb_model,newdata = test_dMatrix)


ggplot(dados_novos,aes(x=grade,y=predito))+geom_point()
ggplot(dados_novos,aes(x=nevents,y=predito))+geom_point()

setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Github\\MOOC Dataset\\App_Predicao')


save(dados_novos,file='Dados_novos.Rdata')
