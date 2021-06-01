#' @title for all diff pathway
#' @export
#' @param d data.frame col sample_id, row gene
#' @param diff_groups list  diff_groups  like, list(list(c(class,cancer_type),c())) with name  1|2 
#' @param groups cluster result
#' @param info cancer info
#' @param type data type, like 'rna'
#' @param q enrich yuzhi
#' @param rna_cut ratio cut for rna diff gene
#' @param met_cut diff cut for met diff gene
#' @param savepath savepath
#list  diff_groups  like, list(list(c(class,cancer_type),c())) with name  1|2 
#d fpkm or met
#d_l only for rna ==log10(d+1)
#q yuzhi  for pathway
#rna_cut  ratio cut for rna diff gene
#met_cut  diff cut for met diff gene
all_pathway<-function(d,diff_groups,groups,info,type,savepath,q=0.05,rna_cut=2,met_cut=0.5)
{
  sample_list1 = list()
  for(i in 1:length(diff_groups))
  {
    t = diff_groups[[i]]
    t1 = t[[1]]
    t2 = t[[2]]
    r1 = class_name(t1[1],t1[2],groups,info,0)
    r2 = class_name(t2[1],t2[2],groups,info,1)
    c1 = c()
    c1[1] = paste(t1[2],t1[1])
    c1[2] = paste(t2[2],t2[1])
    sa = list(r1[[1]],r2[[1]])
    names(sa) = c1
    sample_list1[[i]] = sa
  }
  names(sample_list1) = names(diff_groups)
  all_gene = diff_all(d,sample_list1,type,savepath,q,rna_cut,met_cut)
  return(all_gene)
}

