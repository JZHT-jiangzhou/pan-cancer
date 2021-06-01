#' @export
#' @title get sur info for any clsuter result
#' @param groups cluster reuslt
#' @param cancer cancer list
#' @param info info of sample
#' @param cut_ratio at least percent of numbers to plot sur curve
#' @param savepath_sur the path to save sur picture
#' @param save_word_name the save of word info name.default:info_result.csv
groups_info<-function(groups,cancer,info,cut_ratio = 0.2,save_word_name = 'info_result.csv',savepath_sur = './')
{
  class = rep(1:max(groups))
  r = class_num(class,cancer,groups,info)
  s_w = paste(savepath_sur,save_word_name,sep='')
  write.csv(r,s_w)
  n1 = list()
  type_sur = c()
  count_sur = 1
  for(i in 1:length(row.names(r)))
  {
    c1 = c()
    count = 1
    for(j in 1:length(colnames(r)))
    {
      if(r[i,j]>=(cut_ratio*sum(r[i,])))
      {
        c1[count] = colnames(r)[j]
        count = count+1
      }
    }
    if(length(c1)>1)
    {
      type_sur[count_sur] = row.names(r)[i]
      n1[[count_sur]] = c1
      count_sur = count_sur+1
    }
  }
  plot_sur(n1,type_sur,'icluster',info,groups,savepath_sur)
}
