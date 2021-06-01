#' @title sur for one class
#' @description get the result of sur
#' @param info output of class_name use for sur
#' @return use plot sur curve
##input
#data.frame info
##output
#surv can plot get sur curve
sur<-function(info,rho=0)
{
  sur_time=as.numeric(info$day_to_all)
  state.map<-function(gender){if (gender=='live') 0 else 1}
  state<-lapply(info[,'state'],state.map)
  state=as.numeric(state)
  group=info$group
  dd = data.frame(state=state, sur_time = sur_time, group= group)
  #cox = coxph(Surv(sur_time,state)~group,ties = 'breslow',data = dd)
  my.surv<-survfit(Surv(sur_time,state)~group,data=dd)
  #my.surv<-survfit(cox,data = dd)
  d = survdiff(Surv(sur_time,state)~group,data=dd, rho=rho)
  d1 = round(1 - pchisq(d$chisq,length(d$n)-1),10)
  return(list(my.surv,d1,dd))
}

