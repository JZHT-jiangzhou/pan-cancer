#' @export
#' @title deseq to get diff gene
#' @param d1 data.frame(col sample, row gene)
#' @param d2 data to be compared
#' @param savepath savepath of info
#' @param top select top diff gene(down + up)
#' @param save if save info
#' @param name_compare the name of two groups
deseq<-function(d1,d2,savepath = './',top = 'all',save=TRUE,name_compare=c('cancer','normal'))
{
  gene = intersect(row.names(d1),row.names(d2))
  d1 = d1[gene,]
  d2 = d2[gene,]
  d1 = d1[order(row.names(d1)),]
  d2 = d2[order(row.names(d2)),]
  data = cbind(d1, d2)
  condition<-factor(c(rep(name_compare[1], length(colnames(d1))), rep(name_compare[2], length(colnames(d2)))))
  coldata = data.frame(row.names = c(colnames(d1),colnames(d2)), condition)
  dds<- DESeqDataSetFromMatrix(countData = data, colData = coldata, design = ~condition)
  dds <-DESeq(dds)
  res = results(dds)
  res <- res[order(res$padj),]
  resdata <- merge(as.data.frame(res), as.data.frame(counts(dds, normalized=TRUE)),by="row.names",sort=FALSE)
  resdata[name_compare[1]] = apply(resdata[,colnames(d1)], 1, mean)
  resdata[name_compare[2]] = apply(resdata[,colnames(d2)], 1, mean)
  resdata = na.omit(resdata)
  resdata = resdata[resdata$padj<0.05,]
  if(top!='all')
  {
    resdata =resdata[1:top,c(name_compare[1], name_compare[2], 'padj', 'Row.names')]
  }
  else
  {
    resdata = resdata[,c(name_compare[1], name_compare[2], 'padj', 'Row.names')]
  }
  resdata['up_down'] = 'up'
  resdata[resdata[name_compare[2]]>resdata[name_compare[1]],'up_down'] = 'down'
  file_name = paste(savepath,name_compare[1],'_',name_compare[2],'_diff_gene.csv',sep='')
  write.csv(resdata,file = file_name)
  up = resdata[resdata$up_down=='up','Row.names']
  down = resdata[resdata$up_down=='down','Row.names']
  return(list(up,down))
}
