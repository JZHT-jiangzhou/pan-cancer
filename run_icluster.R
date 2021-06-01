#' @title use for run icluster
#' @export
#' @description pipeline for run icluster
#' @param data data_list
#' @param start start number of icluster
run_icluster<-function(data,start,end,savepath,param,cpu_num=24,lam = 2129,maxiter=20)
{
  if(length(data)>4)
  {
    stop('error, the number of data type could not more than four')
  }
  #if(sum(colnames(param)==c('type','scale'))!=2)
  #{
  #  stop('colnames of param is not type and scale')
  #}
  if(length(data)!=length(param$type))
  {
    stop('the length of data not equal to length of param')
  }
  else
  {
    date()
    if(length(data)==4)
    {
      for(k in start:end){
        cv.fit = tune.iClusterPlus(cpus=cpu_num,dt1=data[[1]],dt2=data[[2]],dt3=data[[3]],dt4=data[[4]],
                                   type=param$type,K=k,n.lambda=lam,
                                   scale.lambda=param$scale,maxiter=maxiter)
        save(cv.fit, file=paste(savepath,"PAN.k",k,".Rdata",sep=""))
      }
      date()
    }
    if(length(data)==3)
    {
      for(k in start:end){
        cv.fit = tune.iClusterPlus(cpus=cpu_num,dt1=data[[1]],dt2=data[[2]],dt3=data[[3]],
                                   type=param$type,K=k,n.lambda=lam,
                                   scale.lambda=param$scale,maxiter=maxiter)
        save(cv.fit, file=paste(savepath,"PAN.k",k,".Rdata",sep=""))
      }
      date()
    }
    if(length(data)==2)
    {
      for(k in start:end){
        cv.fit = tune.iClusterPlus(cpus=cpu_num,dt1=data[[1]],dt2=data[[2]],
                                   type=param$type,K=k,n.lambda=lam,
                                   scale.lambda=param$scale,maxiter=maxiter)
        save(cv.fit, file=paste(savepath,"PAN.k",k,".Rdata",sep=""))
      }
      date()
    }
  }
}
