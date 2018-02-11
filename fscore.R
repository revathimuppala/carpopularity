tpf <- 0
tp <- 0
for(label in c(1,2,3,4)){
  predictedPositives <-(ypred_train==label)*1
  actualPositives <- (yactual==label)*1
  truePositives <- (ypred_train==label& yactual==label)*1
  precision <- sum(truePositives)/sum(predictedPositives)
  recal<-sum(truePositives)/sum(actualPositives)
  f  <- 2*precision*recal/(precision+recal)
  tp <- tp+sum(truePositives)
  tpf<- tpf+(f*sum(truePositives))
  
}

score = 1000*tpf/tp