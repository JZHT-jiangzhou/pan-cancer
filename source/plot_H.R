#' @export
#' @title plot heatmap for icluster
#' @param data col is sampleid
#' @param col.scheme col info, list
#' @param type data distribution(like gussian)
#' @param savepath icluster result savepath
#' @param save_pdf_name pdf name. defalut:'icluster_result.pdf'
#' @return NULL
plot_H<-function(data,col.scheme,type,savepath,save_pdf_name='icluster_result.pdf')
{
  sampleid = colnames(data[[1]])
  r = icluster_groups(savepath,sampleid)
  best.fit = r[[2]]
  pdf(file = save_pdf_name)
  plotHeatmap(fit=best.fit,datasets=data,
              type=type, col.scheme = col.scheme)
  dev.off()
}