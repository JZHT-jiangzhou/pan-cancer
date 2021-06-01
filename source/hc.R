#' @title for hclust at number
#' @export
#' @param data data need to be clustered
#' @param m method like'ward.D2'
#' @param group_num cluster number
#' @return groups:the cluster result
hc<-function(data,group_num,m)
{
  hc<-hclust(dist(t(data)),method=m)
  groups= cutree(hc,k=group_num)
  return(groups)
}