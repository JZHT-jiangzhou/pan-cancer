#' @title survival for all
#' @description for all different groups's sur
#' @param n list of class of one cancer,need to plot sur curve.like,list(c(1,2,3).c(2,5))
#' @param type cancer type, like c('COAD','READ')
#' @param info info of cancer
#' @param groups goups result
#' @return list result of sur curve, use for plot.
#list n like list(c(1,2,5))
#data.frame info 
#c groups 

sur_all<-function(n,type,info,groups, rho=0)
{
  library(survival)
  library(survminer)
  result=list()
  legend = alist()
  pvalue = list()
  r= list()
  dd = list()
  print(names(n))
  for(i in 1:length(n))
  {
    q = n[[i]]
    for(j in 1:length(q))
    {
      t = class_name(q[j],type[i],groups,info,j)
      t1 = t[[2]]
      if(j==1)
      {
        t_f = t1
      }
      else
      {
        t_f = rbind(t_f,t1)
      }
      
    }
    d = max(t_f['day_to_all'])
    legend[[i]] = c(d*0.813,1)
    s = sur(t_f, rho=rho)
    result[[i]] = s[[1]]  
    pvalue[[i]] = s[[2]]
    dd[[i]] = s[[3]]
  }
  r[['result']] = result
  r[['legend']] = legend
  r[['pvalue']] = pvalue
  r[['data']] = dd
  return(r)
}
