library(doParallel)
library(foreach)
library(parallel)

cl<-parallel::makeCluster(detectCores())
doParallel::registerDoParallel(cl)

heart<-read.csv('heart.csv',head=T,sep=',',stringsAsFactors=F)

# gen sample training observations

set.seed(153)

#tridx<-sample(1:nrow(heart),nrow(heart)*0.70,replace=F)

listofidx<-lapply(1:100,FUN=function(x,N=nrow(heart) ){sample(1:N,N*0.70,replace=F)})


time_foreach <- system.time({
  
  #pexpt100<- foreach::foreach(i = 1:length(listofidx),.combine=c)
  pexpt100<- foreach::foreach(i = 1:length(listofidx)) %dopar% {
    data=heart
    x=listofidx[[i]]
    trdata=data[x,]
    tstdata=data[-x,]
    model.glm<-glm(target~.,data=trdata,family=binomial)##target is hardcoded
    pred=predict(model.glm,trdata[,1:13],type="response")
    pred.tst=predict(model.glm,tstdata[,1:13],type="response")
    tr.tbl=table(trdata[,14],ifelse(pred>0.5,1,0))
    tr.cfmx=caret::confusionMatrix(tr.tbl)
    tst.tbl=table(tstdata[,14],ifelse(pred.tst>0.5,1,0))
    tst.cfmx=caret::confusionMatrix(tst.tbl)
    list(idx=x,model=model.glm,
         tractual=data[x,14],trpred=ifelse(pred>0.5,1,0),trtbl=tr.tbl,trcfmx=tr.cfmx,
         tstactual=data[-x,14],tstpred=ifelse(pred.tst>0.5,1,0),tsttbl=tst.tbl,tstcfmx=tst.cfmx)
  } #%dopar%
}) # system.time
time_foreach
# Stop cluster to free up resources
parallel::stopCluster(cl)

table(pexpt100[99][[1]]$tstactual,pexpt100[99][[1]]$tstpred)
caret::confusionMatrix(table(pexpt100[99][[1]]$tstactual,pexpt100[99][[1]]$tstpred))

object.size(pexpt100[99][[1]]$model)

caret::confusionMatrix(table(pexpt100[99][[1]]$tstactual,pexpt100[99][[1]]$tstpred))

object.size(pexpt100[99][[1]]$model)