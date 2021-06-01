#' @title get icluster classifier groups
#' @export
#' @param path path of icluster result
#' @param sample_id input of any data's sample_id, must be same order
#' @param label the prefix of icluster result
#' @return list, 1 is best.cluster,2 is best.fit
icluster_groups<-function(path,sample_id, label = 'PAN')
{
  setwd(path)
  output=alist()
  files=grep(label,dir(path))
  for(i in 1:length(files)){
    load(dir(path)[files[i]])
    output[[i]]=cv.fit
  }
  nLambda = nrow(output[[1]]$lambda)
  nK = length(output)
  BIC = getBIC(output)
  devR = getDevR(output)
  minBICid = apply(BIC,2,which.min)
  devRatMinBIC = rep(NA,nK)
  k_max =0
  max_k = 0
  for(i in 1:nK){
    devRatMinBIC[i] = devR[minBICid[i],i]
    if (devRatMinBIC[i] > max_k)
    {
      max_k = devRatMinBIC[i]
      k_max = i
    }
  }
  clusters=getClusters(output)
  print(length(clusters))
  rownames(clusters)=sample_id
  colnames(clusters)=paste("K=",2:(length(output)+1),sep="")
  best.cluster=clusters[,k_max]
  t = names(best.cluster)
  #t = trans_name(t)
  names(best.cluster) = t
  best.fit=output[[k_max]]$fit[[which.min(BIC[,k_max])]]
  return(list(best.cluster,best.fit,k_max))
}
