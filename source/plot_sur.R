#' @title plot_sur
#' @export 
#' @description  plot sur all
#' @param n like list(c(1,2,3),c(2,5)), same as sur_all
#' @param type same as sur_all, cancer type
#' @param data_type string, like 'rna'
#' @title plot all sur curve
#' @param info cancer info
#' @param groups groups result
#' @param legend_p plot poistion of legend,like list(c(3000,1),c(3000,1)), defalut = 'NULL'
#' @param savepath picture savepath
#' @param c_sur color for curve.default:c('red','green','blue','#FF00FF')
#' @return NULL
#' @examples 
#' n = list(c(1,2,5),c(2,3)), 
#' type = c('COAD','READ')
#' data_type = 'rna'
#' legend_p = list(c(3000,1),c(3000,1))
###
#input
#list n the classification result of same cancer that to plot,like list(c(1,2,5))
#c type the type of cancer
#string data_type  'rna' use for name
#data.frame info info of cancer
#c groups the classification result of data
#list legend_p  the poistion of legend ,like list(c(1,2),c(3,4))
#c c_sur color of sur curve
plot_sur<-function(n,type,data_type,info,groups,savepath='./', legend_p='NULL',c_sur = c('red','green','blue','#FF00FF'),rho=0)
{
  r = sur_all(n,type,info,groups, rho=rho)
  surv = r[[1]]
  data = r[[4]]
  if(legend_p=='NULL')
  {
    legend_p = r[[2]]
    pvalue = r[[3]]
  }
  for(i in 1:length(type))
  {
    print(type[i])
    name = paste(data_type,'_',type[i],'_sur.tif',sep='')
    t = c()
    for(j in 1:length(n[[i]]))
    {
      t[j]= paste(type[i],n[[i]][j])
    }
    name = paste(savepath,name,sep='')
    dd = data[[i]]
    tiff(name,height= 800,width=1200)
    plot(surv[[i]],col=c_sur)
    #ggsurvplot(surv[[i]],data=dd, pval = T, conf.int=F)
    legend(legend_p[[i]][1],legend_p[[i]][2],t,fill=c_sur)
    legend(legend_p[[i]][1],0.1,paste('p=',pvalue[[i]],sep=''))
    dev.off()
  }
}
