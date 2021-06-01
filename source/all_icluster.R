#' @export
#' @title pipeline for icluster
#' @description input the data list
#' @param data data list,name is the type of data,col is sample_id,row is gene,sample_id should be same as info
#' @param param data.frame, with col of type(data_type like gaussion,binomial),scale(use for icluster, scale.lambda),plot_scale(for plot)
#' @param info sample info
#' @param start icluster groups start
#' @param end icluster groups end
#' @param cancer all cancer type
#' @param lam see iclusterplus
#' @param savepath the savepath of result of icluster
#' @param save_word_name the save of word info name.default:info_result.csv
#' @param save_pdf_name the save of picture name.default:icluster_result.pdf
#' @param col.scheme list of color for plot.default:NULL
#' @param cpu_num cpu number use for icluster.default:24
#' @param maxiter iteration times of icluster.default:20
#' @param top_rna threshold for rna data
#' @param top_met threshold for met data
#' @param snv_ratio threshold for snv mutation ratio
#' @param savepath_sur the path to save sur picture
#' @return NULL
all_icluster<-function(data,param,info,start,end,cancer,lam,label='PAN',savepath='./',save_word_name='info_result.csv',save_pdf_name='icluster_result',col.scheme='NULL',cpu_num=24,maxiter=20,top_rna=1000,top_met=1000,snv_ratio=0.05,save_sig_name='sigfeature_info.csv',savepath_sur='./', save_type='pdf')
{
  info = get_info(info,cancer)
  data2 = get_icluster_data(data,info,top_rna=top_rna,top_met = top_met,snv_ratio=snv_ratio)
  d = data_pretreat_icluster(data2,plot_scale = param$plot_scale)
  data3 = d[[1]]
  #run_icluster(data3,start,end,savepath,param,cpu_num=cpu_num,lam=lam,maxiter=maxiter)
  #icluster_info(data2,d,as.character(param$type),info,start,end,cancer,savepath,label=label,save_sig_name=save_sig_name,save_word_name=save_word_name,save_pdf_name=save_pdf_name,col.scheme=col.scheme,savepath_sur = savepath_sur,save_type = save_type)
}