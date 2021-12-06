# Enviroment
setwd(".")
rm(list=ls())
library(easypackages)
libraries("randomForest","caret","mltools","MLmetrics")
cat("\014")
set.seed(11)

# Metrics
MetricsClass <- function(YF,YT,YP){
  ris = mltools::mcc(preds=YF,actuals=YT)
  ris = c(ris,MLmetrics::F1_Score(y_true=YT,y_pred=YF,positive=1))
  ris = c(ris,MLmetrics::Accuracy(y_true=YT,y_pred=YF))
  ris = c(ris,MLmetrics::Recall(y_true=YT,y_pred=YF,positive=1))
  ris = c(ris,MLmetrics::Specificity(y_true=YT,y_pred=YF,positive=1))
  ris = c(ris,MLmetrics::PRAUC(y_true=YT,y_pred=YP))
  ris = c(ris,MLmetrics::AUC(y_true=YT,y_pred=YP))
  ris = c(ris,caret::posPredValue(data=as.factor(YF),reference=as.factor(YT),positive="1"))
  ris = c(ris,caret::negPredValue(data=as.factor(YF),reference=as.factor(YT),negative="0")) }

# Results
D <- read.csv('journal.pone.0199920.s002_EDITED.csv',header=TRUE,sep = ',')
v = c(3,16,17,23)
D = D[,-v]; 
v = c(1,3:13,20)
for (i in v) {
  D[,i] = as.factor(D[,i]) }
ix = c(1:19)
iy = c(20)
#
n = nrow(D)
nl = n-1
YYT = c()
YYF = c()
YYP = c()
mc = 1000
IM1 = array(0,dim=c(length(ix),mc))
IM2 = array(0,dim=c(length(ix),mc))
for (i in c(1:mc)) {
  print(sprintf("mc: %04d",i))
  k = sample(n)
  il = k[1:nl]; it = k[(nl+1):n]
  XL = D[il,ix]; YL = D[il,iy]
  XT = D[it,ix]; YT = D[it,iy]
  tmp = min(sum(YL==levels(YL)[1]),sum(YL==levels(YL)[2]))
  M = randomForest(x=XL,y=YL,
                   #mtry = 1,
                   ntree=5000,
                   do.trace=FALSE,
                   importance=TRUE,
                   sampsize=c(tmp,tmp))
  tmp = c(1:length(ix))
  IM1[sort(importance(M,type=1,scale=FALSE),decreasing=TRUE,index.return=TRUE)$ix,i] = tmp 
  IM2[sort(importance(M,type=2,scale=FALSE),decreasing=TRUE,index.return=TRUE)$ix,i] = tmp 
  YF = predict(M,XT)
  YP = predict(M,XT,type="prob")
  YYT = c(YYT,as.numeric(YT)-1)
  YYF = c(YYF,as.numeric(YF)-1)
  YYP = c(YYP,as.numeric(YP)[2]) }
cat("\014")
#
I = array(0,dim=length(ix))
for (i in c(1:length(ix))) {
  I[i] = sum(IM1[i,])/mc }
tmp = sort(I,decreasing=FALSE,index.return=TRUE)$ix
cat("",file="RIS/RF_MDA.txt",append=FALSE,sep="")
print(sprintf("MDA"))
for (i in tmp){
  s = sprintf("%02.1f - %s",I[i],names(D)[ix[i]])
  print(s)
  cat(s,file="RIS/RF_MDA.txt",append=TRUE,sep="\n") }
I = array(0,dim=length(ix))
for (i in c(1:length(ix))) {
  I[i] = sum(IM2[i,])/mc }
tmp = sort(I,decreasing=FALSE,index.return=TRUE)$ix
cat("",file="RIS/RF_MDI.txt",append=FALSE,sep="")
print(sprintf("MDI"))
for (i in tmp){
  s = sprintf("%02.1f - %s",I[i],names(D)[ix[i]])
  print(s)
  cat(s,file="RIS/RF_MDI.txt",append=TRUE,sep="\n") }
#
XL = D[,ix]; YL = D[,iy]
tmp = min(sum(YL==levels(YL)[1]),sum(YL==levels(YL)[2]))
FinalModel = randomForest(x=XL,y=YL,
                          #mtry = 1,
                          ntree=5000,
                          do.trace=FALSE,
                          importance=TRUE,
                          sampsize=c(tmp,tmp))
par(mfrow=c(1,2))
varImpPlot(FinalModel,type=1,scale=FALSE)
varImpPlot(FinalModel,type=2,scale=FALSE)
write.table(importance(M, type=1, scale=FALSE),
            file=sprintf('RIS/RF_MDA_Round.txt',mc,s), col.names=T, row.names=T)
write.table(importance(M, type=2, scale=FALSE),
            file=sprintf('RIS/RF_MDI_Round.txt',mc,s), col.names=T, row.names=T)
#
ris = caret::confusionMatrix(as.factor(YYF), as.factor(YYT),positive="1")
print(ris[2])
riss = c()
for (i in c(1:mc)) {
  j = sample(length(YYF),replace=TRUE)
  riss = rbind(riss, MetricsClass(YYF[j],YYT[j],YYP[j])) }
rism = MetricsClass(YYF,YYT,YYP)
s = sprintf("mcc:    %.3f pm %.3f",rism[1],sd(riss[,1])); print(s); cat(s,file="RIS/RF_Met.txt",append=FALSE,sep="\n")
s = sprintf("f1:     %.3f pm %.3f",rism[2],sd(riss[,2])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("acc:    %.3f pm %.3f",rism[3],sd(riss[,3])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("rec:    %.3f pm %.3f",rism[4],sd(riss[,4])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("spec:   %.3f pm %.3f",rism[5],sd(riss[,5])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("ppv:    %.3f pm %.3f",rism[8],sd(riss[,8])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("npv:    %.3f pm %.3f",rism[9],sd(riss[,9])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("prauc:  %.3f pm %.3f",rism[6],sd(riss[,6])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
s = sprintf("rocauc: %.3f pm %.3f",rism[7],sd(riss[,7])); print(s); cat(s,file="RIS/RF_Met.txt",append=TRUE,sep="\n")
