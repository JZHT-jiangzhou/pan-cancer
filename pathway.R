###get picture p 
#enrich  output of fuji
pathway<-function(enrich)
{
  library(ggplot2)
  enrich = enrich[order(enrich$Description),]
  p = ggplot(enrich,aes(richFactor,Description))
  p = p + geom_point(aes(size=Count))
  p = p+ geom_point(aes(size=Count,color = -1*log10(qvalue)))
  p = p + scale_colour_gradient(low="green",high="red")
  p = p + labs(color=expression(-log10(qvalue)),size="Gene number",x="Rich factor",y="Pathway name") 
  p = p+theme_bw()
  return(p)
}