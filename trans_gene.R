#' @title trans gene to data.frame
#' @export
#' @param x the name of gene with geneid and symbol
#' @return data.frame of symbol and id
trans_gene<-function(x){
  c1 = c()
  c2 = c()
  for(i in 1:length(x))
  {
    c1[i] = strsplit(as.character(x[i]),'|',fixed=TRUE)[[1]][1]
    c2[i] = strsplit(as.character(x[i]),'|',fixed=TRUE)[[1]][2]
  }
  d = data.frame(symbol=c1,id=c2)
  #row.names(d) = c1
  return(d)
}
