#' @export
#' @title pipeline to get all icluster info
#' @param rawdata list, raw data list
#' @param d data_list, output of data_pretreat_icluster
#' @param param data.frame, with col of type(data_type like gaussion),scale(use for icluster, scale.lambda),plot_scale(for plot)
#' @param info sample info
#' @param start icluster groups start
#' @param end icluster groups end
#' @param cancer all cancer type
#' @param savepath the savepath of result of icluster
#' @param save_word_name the save of word info name.default:info_result.csv
#' @param save_pdf_name the save of picture name.default:icluster_result.pdf
#' @param col.scheme list of color for plot.default:NULL
#' @param savepath_sur the path to save sur picture
#' @param cut_ratio at least percent of numbers to plot sur curve
#' @param m function like mean
#' @param proba probalitity to get sig gene from lasso
#' @param save_type the save picture of heatmap,tiff or pdf
icluster_info<-function(rawdata,d,type,info,start,end,cancer,savepath,proba = 0.75,label='PAN',m=mean,cut_ratio=0.2,save_sig_name='sigfeature_info.csv',col.scheme='NULL',save_word_name='info_result.csv',save_pdf_name='icluster_result',savepath_sur='./', save_type='pdf',rho=0)
{
  data = d[[1]]
  plot_data = d[[2]]
  sampleid = row.names(data[[1]])
  r = icluster_groups(savepath,sampleid,label=label)
  best.fit = r[[2]]
  best.cluster = r[[1]]
  kk = intersect(names(best.cluster), row.names(info))
  info2 = info[kk,]
  g = best.cluster[kk]
  k_max = r[[3]]
  n = rep((start+1):(end+1))
  #k_max = n[k_max]
  k_max = max(best.cluster)
  #sur_info
  class = rep(1:k_max)
  r = class_num(class,cancer,g,info2)
  print(r)
  nn = partition(r, cut_ratio, g, info2)
  n1 = nn[[1]]
  n2 = nn[[2]]
  type_sur = nn[[3]]
  #plot_sur(n1,type_sur,'icluster',info2,g,savepath_sur, rho=rho)
  #part_info
  write.csv(r,save_word_name)
  #sig_info
  sig = get_feature(best.fit,data,savename=save_sig_name,prob=proba)
  s1 = get_sig(sig,rawdata,n2,m)
  write.csv(s1,'all_sig_info.csv')
  
  #icluster_pdf
  n = names(data)
  if(col.scheme=='NULL')
  {
    col.scheme = alist()
    for(i in 1:length(n))
    {
      if(n[i]=='snv')
      {
        col.scheme[['snv']] = colorpanel(2,low="white",high="black")
      }
      else
      {
        col.scheme[[n[i]]] = bluered(256)
      }
    }
  }
  if(save_type=='pdf')
  {
    pdf(file = paste(save_pdf_name,'.pdf',sep=''))
    plotHeatmap(fit=best.fit,datasets=plot_data,
              type=type, col.scheme = col.scheme)
    dev.off()
  }
  else if(save_type=='tiff')
  {
    f =  paste(save_pdf_name,'.tiff',sep='')
    #tiff(file = paste(save_pdf_name,'.tif',sep=''), width=1200,height = 800,pointsize = 600)
    p=plotHeatmap(fit=best.fit,datasets=plot_data,
                type=type, col.scheme = col.scheme)
    ggsave(p,filename = f,dpi=600)
    #dev.off()
  }
}
