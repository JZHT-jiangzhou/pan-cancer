#' @title pheatmap for one data
#' @export
#' @param data use for cluster, output of data_pretreat
#' @param info2 output of data_pretreat
#' @param m cluster method like'ward.D2'
#' @param group_num group number for cluster
#' @param a_col list color for pheatmap
#' @param annotation data.frame for pheatmap
#' @param label_col for pheatmap,like [colorRampPalette(c("#36648B","#FFFACD","#FF6A6A"))(50)]
#' @return cluster groups result
pheat_groups<-function(data,info2,m,group_num,a_col,annotation,label_col,save_name)
{
  groups = hc(data,group_num,m)
  p_num = info2$p_num
  s_num = info2$stage_num
  r_num = info2$r_num
  colnames(p_num)<-colnames(groups)
  colnames(s_num)<-colnames(groups)
  colnames(r_num)<-colnames(groups)
  #a_col=list(Category=c(COAD='#FFFF00',PAAD='#FFBBFF',LIHC='#C1FFC1',ESCA='#EEB422',STAD='#87CEFF',READ='#76EE00'),Class=c(Class1='#C1FFC1',Class2='#FFFF00',Class3='#1874CD',Class4='#87CEFF',Class5='#76EE00',Class6='#8B7500',Class7='#EEB422',Class8='#FFBBFF'),Race=c(asian='#FFBBFF',white='#8DEEEE',black='#4EEE94',am.indian='#EE0000',not.reported='#CDAD00'))
  #annotation=data.frame(Stage=factor(s_num,levels=1:5,labels=c("i","ii","iii","iv","not_reported")),Category=factor(p_num,levels=1:6,labels=c("COAD","READ","LIHC","ESCA","PAAD",'STAD')),Class=factor(groups,levels=c(1,2,3,4,5,6,7,8),labels=c('Class1','Class2','Class3','Class4','Class5','Class6','Class7','Class8')),Race=factor(r_num,levels=1:5,labels=c("asian","white","black","am.indian","not.reported")))
  
  tiff(file=save_name,height=800,width=1200)
  pheatmap(data,annotation_col = (annotation),show_rownames=FALSE,show_colnames = FALSE,color=label_col,clustering_method = m,annotation_colors = a_col)
  dev.off()
  return(hc)
}
