#' @title  transform sample_id
#' @export
#' @description use for transfrom sample id
trans_name<-function(c)
{
  t = rep(1,length(c))
  for (i in 1:length(c))
  {
    
    t[i] = gsub('\\.','-',substring(c[i],1,37))
    t[i]=gsub('X','',substring(t[i],1,37))
  }
  return(t)
}