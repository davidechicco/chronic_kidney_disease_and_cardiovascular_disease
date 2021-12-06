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
method = "MLP"
D <- read.csv('journal.pone.0199920.s002_EDITED.csv',header=TRUE,sep = ',')
v = c(3,16,17,23)
D = D[,-v]; 
if (method == "DT") {
  v = c(1,3:13,20) }
if (method == "SVL" || method == "SVK" || method == "MLP"  || method == "1R" || method == "XGB") {
  v = c(20) }
ix = c(1:19)
iy = c(20)
for (i in v) {
  D[,i] = as.factor(D[,i]) }
#
n = nrow(D)
nl = round(.97*n)
YYT = c()
YYF = c()
YYP = c()
mc = 10
IM1 = array(0,dim=c(length(ix),mc))
IM2 = array(0,dim=c(length(ix),mc))
for (i in c(1:mc)) {
  print(sprintf("mc: %04d",i))
  k = sample(n)
  il = k[1:nl]; it = k[(nl+1):n]
  XL = D[il,ix]; YL = D[il,iy]
  XT = D[it,ix]; YT = D[it,iy]
  if (method == "1R"){
    strmethod = "OneR"}
  if (method == "XGB") {
    strmethod =  "xgbLinear"
    grid = expand.grid(nrounds=c(3),
                       lambda=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50),
                       alpha=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50),
                       eta=c(.01,.1) ) }
  if (method == "DT") {
    strmethod = "rpart2"
    grid = expand.grid(maxdepth=c(2,4,6,8,10,12,14)) }
  if (method == "SVL") {
    strmethod = "svmLinear"
    grid = expand.grid(C=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50)) }
  if (method == "SVK") {
    strmethod = "svmRadial"
    grid = expand.grid(C=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50),
                       sigma=c(.0001,.0005,.001,.005,.01,.05,.1,.5,1,5,10,50)) }
  if (method == "MLP") {
    strmethod = "mlpKerasDropout"; 
    grid = expand.grid(size=c(5,10,20,40,80,160),
                       dropout=c(0,.001,.01,.1),           
                       batch_size=c(nl/10,nl),
                       lr=c(.001,.01,.1,1),
                       rho=c(.9,0.09),
                       decay=c(.001,.01,.1,1),activation=c("relu")) }
  trctrl = trainControl(method="repeatedcv",
                        number=10,
                        repeats=1,
                        sampling="up",
                        allowParallel=TRUE)
  if (method == "1R") {
    M <- train(x=XL,
               y=YL,
               method=strmethod,
               trControl=trctrl,
               preProcess=c("center","scale"),
               metric="Kappa")}
  else {
    M <- train(x=XL,
               y=YL,
               method=strmethod,
               trControl=trctrl,
               tuneGrid=grid,
               preProcess=c("center","scale"),
               metric="Kappa") }
  YF = predict(M,XT)
  YYT = c(YYT,as.numeric(YT)-1)
  YYF = c(YYF,as.numeric(YF)-1)
  YYP = c(YYP,as.numeric(YF)-1)
}
cat("\014")
#
ris = caret::confusionMatrix(as.factor(YYF), as.factor(YYT),positive="1")
print(ris[2])
riss = c()
for (i in c(1:mc)) {
  j = sample(length(YYF),replace=TRUE)
  riss = rbind(riss, MetricsClass(YYF[j],YYT[j],YYP[j])) }
rism = MetricsClass(YYF,YYT,YYP)
s = sprintf("mcc:    %.3f pm %.3f",rism[1],sd(riss[,1])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=FALSE,sep="\n")
s = sprintf("f1:     %.3f pm %.3f",rism[2],sd(riss[,2])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("acc:    %.3f pm %.3f",rism[3],sd(riss[,3])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("rec:    %.3f pm %.3f",rism[4],sd(riss[,4])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("spec:   %.3f pm %.3f",rism[5],sd(riss[,5])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("ppv:    %.3f pm %.3f",rism[8],sd(riss[,8])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("npv:    %.3f pm %.3f",rism[9],sd(riss[,9])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("prauc:  %.3f pm %.3f",rism[6],sd(riss[,6])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
s = sprintf("rocauc: %.3f pm %.3f",rism[7],sd(riss[,7])); print(s); cat(s,file=sprintf("RIS/%s_Met.txt",method),append=TRUE,sep="\n")
