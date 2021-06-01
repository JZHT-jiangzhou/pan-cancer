library(canceri)
library(iClusterPlus)
library(lattice)
library(gplots)
library(survival)
setwd('/lustre/home/bioxwz/jzht/R_Project')
#data_cnv = read.csv('all_five_cancer_cnv.csv',row.names = 1)
data_rna = read.csv('all_five_rna_icluster1000.csv',row.names = 1)
#data_snv = read.csv('all_five_snv.csv',row.names = 1)
data_met = read.csv('all_five_high_met.csv',row.names = 1)
info = read.csv('all_case_dayall_info.csv')
#info = read.csv('/home/zhanghang/cancer_exp/all_case_dayall_info.csv') ###
row.names(info) = info$sample_id
#t = trans_name(colnames(data_cnv))
#colnames(data_cnv) = t
t = trans_name(colnames(data_rna))
colnames(data_rna) = t
#t = trans_name(colnames(data_snv))
#colnames(data_snv) = t
t = trans_name(colnames(data_met))
colnames(data_met) = t
data = list(data_snv,data_cnv)#data = list(data_snv,data_cnv,data_met,data_rna)#多个表格形成一个list
names(data) = c('snv','cnv')#names(data) = c('snv','cnv','met','rna')
start = 7
end = 9
savepath = '/lustre/home/bioxwz/jzht/R_Project/result'
p1 = c("binomial","gaussian","gaussian","gaussian")
p2 = c(0.05,1,1,1)
p3 = c(0,1.5,1.5,2.5)
pa = data.frame(type = p1,scale=p2,plot_scale = p3)
cancer = c('COAD','READ','LIHC','ESCA','PAAD','STAD')
lam = 2129
all_icluster(data,pa,info,start,end,cancer, lam,savepath)
#d1 = get_icluster_data(data,info)
#d2 = data_pretreat_icluster(d1,pa$plot_scale)
#d3 = d2[[1]]
#run_icluster(d3,7,9,savepath,pa)
