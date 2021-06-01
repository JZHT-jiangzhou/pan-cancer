#' @title differential
#' @param d1 fpkm,list,1 is data.frame,(like cancer), 2 is log(data+1)
#' @param d2 to be compared list.1 is  data.frame(like normal), 2 is log(data+1)
#' @param type type of data
#' @param savepath savepath for diff gene info.default './'
#' @param rna_cut ratio of rna.default 2
#' @param met_cut ratio of met.default 0.3
#' @param top return number of gene(the sum of up and down).default 'all'
#' @param save if to save the info of diff gene.default TRUE
#' @return differienal genes,list, 1 up genes,2 down genes
###differential
#input 
#d fpkm
#d_l  log10(fpkm+1)
#list sample  1,is the fisrt sample_dataframe(output of class_name),2 is another
#<0.5 sample1|sample2 down
#>2 sample1|sample2 up

#output  differieanal genes
diff_gene<-function(d1,d2, type,savepath = './',rna_cut=2,met_cut=0.3,top = 'all',save=TRUE,name_compare=c('cancer','normal'))
{
  mean1 = apply(d1,1,'mean')
  mean2 = apply(d2,1,'mean')
  if((type=='rna')|type=='mirna')
  {
    dt1 = log10(d1+1)
    dt2 = log10(d2+1)
    p = c()
    for(j in 1:length(row.names(dt1)))
    {
      if((sum(!duplicated(t(dt1[j,])))==1)&(sum(!duplicated(t(dt2[j,])))==1))
      {
        p[j] = 1
      }
      else
      {
        p[j] =t.test(dt1[j,],dt2[j,])$p.value
      }
    }
    ratio = mean1/mean2
    diff12 = data.frame(sample1=mean1,sample2=mean2,gene=row.names(d1),ratio = ratio,p=p)
    diff12 = diff12[diff12$p<0.05,]
    c1 = name_compare
    diff12 = diff12[(diff12$ratio>rna_cut)|(diff12$ratio<(1/rna_cut)),]
    diff12 = diff12[order(diff12$p),]
    if(top!='all')
    {
      diff12 = diff12[1:top,]
    }
    up = as.character(diff12[diff12$ratio>rna_cut,]$gene)
    down = as.character(diff12[diff12$ratio<(1/rna_cut),]$gene)
    c1[3] = 'gene'
    c1[4] = 'ratio'
    c1[5] = 'p'
    colnames(diff12) = c1
  }
  if(type=='met')
  {
    t = mean1-mean2
    diff12 = data.frame(sample1=mean1,sample2=mean2,gene=row.names(d1),diff=t)
    c1 = names(sample)
    diff12 = diff12[(diff12$diff>met_cut)|(diff12$ratio<-met_cut),]
    diff12 = diff12[order(diff12$p),]
    if(top!='all')
    {
      diff12 = diff12[1:top,]
    }
    up = as.character(diff12[diff12$diff>met_cut,]$gene)
    down = as.character(diff12[diff12$ratio<-met_cut,]$gene)
    c1[3] = 'gene'
    c1[4] = 'diff'
    colnames(diff12) = c1
  }
  print(paste(type,'up number gene of',c1[1],'compare to',c1[2],'is',length(up)))
  print(paste(type,'down number gene of',c1[1],'compare to',c1[2],'is',length(down)))
  savename = paste(savepath,c1[1],"_",c1[2],'_diffgene_',type,'.csv',sep="")
  if(save)
  {
    write.csv(diff12,savename)
  }
  return(list(up,down))
}

