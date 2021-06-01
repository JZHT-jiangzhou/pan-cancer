###
#data.frame gene_to_id  output of trans_gene
#gene  up or down 
fuji<-function(gene,gene_to_id,q1)
{
  library(clusterProfiler)
  gene_id_up = as.character(gene_to_id[gene[[1]]]$id)
  gene_id_down = as.character(gene_to_id[gene[[2]]]$id)
  enrich_up =  as.data.frame(enrichKEGG(gene = gene_id_up,organism = "human", qvalueCutoff = q1))
  enrich_down = as.data.frame(enrichKEGG(gene = gene_id_down,organism = "human", qvalueCutoff = q1))
  k <-function(enrich)
  {
    z= as.character(enrich$BgRatio)
    t = c()
    for(i in 1:length(z))
    {
      t[i] = unlist(strsplit(z[i],'/'))[1]
    }
    enrich$Bg = as.numeric(t)
    enrich$richFactor =  enrich$Count/enrich$Bg
    return(enrich)
  }
  enrich_up = k(enrich_up)
  enrich_down = k(enrich_down)
  
  return(list(enrich_up,enrich_down))
}