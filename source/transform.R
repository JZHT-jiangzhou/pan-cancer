#' @title transform from groups1 to groups2
#' @param groups1 group1 from
#' @param groups2 group2 to
#' @param info info 
#' @param cancer array
#' @export
transform<-function(groups1, groups2, info, cancer)
{
  class1 = rep(1:max(groups1))
  class2 = rep(1:max(groups2))
  m = list()
  for(k in 1:max(groups2))
  {
    m[[k]] = names(groups2[groups2==k])
  }
  r = list()
  for(j in 1:length(cancer))
  {
    can = row.names(info[info$project==cancer[j],])
    for(i in 1:max(groups1))
    {
      h = intersect(names(groups1[groups1==i]),names(groups2))
      can1 = intersect(h,can)
      t = c()
      for(k in 1:length(m))
      {
        t[k] = length(intersect(can1,m[[k]]))
      }
      name = paste(cancer[j],i,sep = '')
      r[[name]] = t 
    }
  }
  return(as.data.frame(r))
}
