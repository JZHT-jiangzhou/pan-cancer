#' @export
#' @title get coca matrix for another class
#' @param data data list, col is sample id, row is gene
#' @param k list length the same to data, for the number of every type of data
#' @param pItem proportion of items to sample.default 0.8
#' @param pFeature proportion of features to sample. default 1
#' @return o,matrix for heatmap
coca<-function(data,k,method = 'NULL',pItem = 0.8,pFeature = 1, reps = 100,seed=1,plot = 'png',savepath = './')
{
  if(method=='NULL')
  {
    method = rep('pearson',length(data))
  }
  library(ConsensusClusterPlus)
  r_names = c()
  if('snv'%in%names(data))
  {
    s = data[['snv']]
    z = apply(s,2,sum)
    z = (z!=0)
    for(i in 1:length(data))
    {
      data[[i]] = data[[i]][,z]
      print(length(data[[i]][1,]))
    }
  }
  o = matrix(0,sum(k),length(colnames(data[[1]])))
  colnames(o) = colnames(data[[1]])
  count = 1
  for(i in 1:length(data))
  {
    print(i)
    save_name = paste(savepath,names(data)[i],sep='')
    rc = ConsensusClusterPlus(data[[i]],maxK=k[i],pItem=pItem,pFeature=pFeature,distance = method[i], reps = reps, plot = plot,seed=seed,title = save_name) 
    cluster = rc[[k[i]]]$consensusClass
    for(j in 1:k[i])
    {
      r_names[count] = paste(names(data)[i],j,sep='')
      print(length(cluster))
      t = seq(0,0,length.out = length(cluster))
      names(t) = names(cluster)
      t[names(cluster[cluster==j])] = 1
      o[count,] = t
      count = count +1
    }
  }
  row.names(o) = r_names
  return(o)
}

