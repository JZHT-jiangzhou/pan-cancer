#' @export
#' @title diff_gene for compare cluster result
#' @description use for get the different groups of different cancer's diff gene, compared to normal sample
#' @param data all cancer data,col samples,row genes
#' @param data_normal all normal data, col samples, row genes 
#' @param class list, class name
#' @param cancer list, caner type list
#' @param groups result of cluster
#' @param info cancer info
#' @param type data type.default rna
#' @param top top number gene to compare.default 100
#' @param save if save info of diff gene.default FALSE
#' @param savepath default './'
#' @param rna_cut default 2
#' @param met_cut default 0.3
#' @param method the method of get diff gege.(t_test or Deseq)
#' @param  diff_method the method of compare(paired of not paired)
diff_groups<-function(data,data_normal,class,cancer,groups,info,type='rna',top=100,save=FALSE,savepath='./',rna_cut=2,met_cut=0.3,method='t.test',diff_method='paired',cut_ratio = 0.1)
{
  gene = intersect(row.names(data),row.names(data_normal))
  data = data[gene,]
  data_normal = data_normal[gene,]
  data = data[order(row.names(data)),]
  data_normal = data_normal[order(row.names(data_normal)),]
  r_up = list()
  r_down = list()
  r_class_up = list()
  r_class_down = list()
  f<-function(l)
  {
    m = 0
    for(i in 1:length(l))
    {
      if(length(l[[i]])>m)
      {
        m = length(l[[i]])
      }
    }
    for(i in 1:length(l))
    {
      t = l[[i]]
      length(t) = m
      if(i==1)
      {
        p = data.frame(t1 = t)
      }
      else
      {
        p = cbind(p,t)
      }
    }
    colnames(p) = names(l)
    return(p)
  }
  for(i in 1:length(class))
  {
    diff_g_up = c('NULL')
    diff_g_down = c('NULL')
    name_class = paste('class',i,sep='')
    count = 1
    for(j in 1:length(cancer))
    {
      s = class_name(class[i],cancer[j],groups,info)[[1]]
      if(length(s$sample_id)>cut_ratio*length(intersect(info[info$project==cancer[j],]$sample_id, colnames(data))))
      {
        s = s$sample_id
        s = intersect(colnames(data),s)
        s2 = intersect(s,colnames(data_normal))
        names = paste(cancer[j],class[i],sep='')
        n_c = paste(names,'_cancer',sep='')
        n_n = paste(names,'_normal',sep='')
        name_c = c(n_c,n_n)
        if(length(s2)>1)
        {
          if(diff_method=='paired')
          {
            d1 = data[,s2]
          }
          else if(diff_method=='not paired')
          {
            d1 = data[,s]
          }
          d2 = data_normal[,s2]
          if(method=='t.test')
          {
            r = diff_gene(d1,d2,type=type,save=save,top=top,rna_cut=rna_cut,met_cut=met_cut,savepath=savepath, name_compare = name_c)
          }
          else if(method=='DEseq')
          {
            f2<-function(name,label)
            {
              name2 = c()
              for(i in 1:length(name))
              {
                name2[i] = paste(name[i],'-',label,sep='')
              }
              return(name2)
            }
            colnames(d1) = f2(colnames(d1),'11A')
            colnames(d2) = f2(colnames(d2),'01A')
            r = deseq(d1,d2,savepath = savepath, save=save,top = top,name_compare = name_c)
          }
          r_up[[names]] = r[[1]]
          r_down[[names]] = r[[2]]
          if(count==1)
          {
            diff_g_up = r[[1]]
            diff_g_down = r[[2]]
            count = count +1
            name_class = names
          }
          else
          {
            diff_g_up = intersect(r[[1]], diff_g_up)
            diff_g_down = intersect(r[[2]], diff_g_down)
            name_class = paste(name_class,names,sep=':')
          }
          
        }
      }
      
    }
    r_class_up[[name_class]] = diff_g_up 
    r_class_down[[name_class]] = diff_g_down
  }
  r1 = f(r_up)
  name1 = paste(savepath,'all_up.csv',sep='')
  write.csv(r1,name1)
  r2 = f(r_down)
  name2 = paste(savepath,'all_down.csv',sep='')
  write.csv(r2,name2)
  r3 = f(r_class_up)
  name3 = paste(savepath,'class_up.csv',sep='')
  write.csv(r3,name3)
  r4 = f(r_class_down)
  name4 = paste(savepath,'class_down.csv',sep='')
  write.csv(r4,name4)
}
