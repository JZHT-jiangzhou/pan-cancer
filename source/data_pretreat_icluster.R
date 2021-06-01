#' @title for icluster data pretreat
#' @export 
#' @description use for pretreat icluster data
#' @param data data_list 
#' @param plot_scale for icluster plot
#' @return list,1 is data_list for icluster,2 is data_list for draw
#list data, with name
data_pretreat_icluster<-function(data,plot_scale=c(0,1.5,1.5,2))
{
  t = names(data)
  result = list()
  plot_data = list()
  for(i in 1:length(data))
  {
    if((t[i]=='rna'|t[i]=='mirna'))
    {
      d1 = log10(data[[i]]+1)
    }
    else
    {
      d1 = data[[i]]
    }
    if(t[i]=='snv')
    {
      d1 = t(d1)
    }
    else
    {
      d1 = t(d1)
      d1 = scale(d1,center=TRUE,scale=TRUE)
    }
    
    if(t[i]=='snv')
    {
      plot_data[[i]] = d1
    }
    else
    {
      exp = d1
      exp[exp>plot_scale[i]] = plot_scale[i]
      exp[exp<-plot_scale[i]] = -plot_scale[i]
      plot_data[[i]] = exp
    }
    result[[i]] = d1
  }
  names(result) = t
  names(plot_data) = t
  return(list(result,plot_data))
}
