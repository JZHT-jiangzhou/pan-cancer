#' @export
#' @title for pinsplus
#' @param data data list
#' @param max_group max groups for pins
#' @param agreementCutoff pinsplus
pinsplus<-function(data, max_group=9,agreementCutoff = 0.5)
{
  r = SubtypingOmicsData(data,agreementCutoff = agreementCutoff,kMax=max_group)
  r1 =  data.frame(cluster = r$cluster2)
  f<-function(x)
  {
    if(typeof(x)=='integer')
    {
      return(x)
    }
    else if(typeof(x) =='character')
    {
      a = strsplit(x,'-')[[1]]
      if(length(a)>1)
      {
        return(as.numeric(a[2]))
      }
      else
      {
        return(as.numeric(a[1]))
      }
    }
  }
  r1 = apply(r1,1,f)
  for(i in 1:length(r$dataTypeResult))
  {
    k = r$dataTypeResult[[i]]$k
    t = r$dataTypeResult[[i]]$origS[[k]]
    if(i==1)
    {
      c_a = t
    }
    else
    {
      c_a = c_a +t
    }
  }
  c_a = c_a/(length(r$dataTypeResult))
  c_a = 1 - c_a
  return(list(r1,c_a))
}
