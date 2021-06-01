#' @title for all diff gene and pathway
#' @export
#' @param d row_data
#' @param sample_list all samples need to be compare,list(list(d1,d2)),d1 the output of class_name
#' @param type data type
#' @param q for pathway
#' @param rna_cut ratio cut for rna diff gene
#' @param met_cut diff cut for met diff gene
#' 
#list sample_list  all samples need to be compare,list(list(d1,d2)),d1 the output of class_name
#type data type like'rna'
diff_all<-function(d,sample_list,type,savepath = './',q=0.05,rna_cut=2,met_cut=0.2,top = 'all',save=TRUE)
{
  if(type=='rna')
  {
    t = row.names(d)
    t = trans_gene(t)
    row.names(d) = t['symbol']
    #row.names(d_l) = t['symbol']
  }
  all_gene = list()
  for(i in 1:length(sample_list))
  {
    d1 = d[,as.character(sample_list[[i]][[1]]$sample_id)]
    d2 = d[,as.character(sample_list[[i]][[2]]$sample_id)]
    r = diff_gene(d1,d2,type,savepath = savepath,rna_cut=rna_cut,met_cut=met_cut,top=top,save=TRUE)
    all_gene[[i]] = r
    enrich = fuji(r,t,q1=q)
    p = pathway(enrich[[1]])
    s_name = paste(savepath,names(sample_list[i]),'_up_pathway_',type,'.tif',sep='')
    s_n  = paste(savepath,names(sample_list[i]),'_up_pathway_',type,'.csv',sep='')
    write.csv(enrich[[1]],s_n)
    tiff(s_name)
    p
    dev.off()
    p2 = pathway(enrich[[2]])
    s_name2 = paste(savepath,names(sample_list[i]),'_down_pathway_',type,'.tif',sep='')
    s_n2  = paste(savepath,names(sample_list[i]),'_down_pathway_',type,'.csv',sep='')
    write.csv(enrich[[2]],s_n2)
    tiff(s_name2)
    p2
    dev.off()
    
  }
  return(all_gene)
}