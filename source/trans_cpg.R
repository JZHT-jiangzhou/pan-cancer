#' @export
#' @title transform cpg_site to gene
#' @param cpg_site cpg_site list
trans_cpg<-function(cpg_site)
{
  data(cpg)
  sym = cpg[cpg_site,]$symbol
  return(as.character(sym))
}
