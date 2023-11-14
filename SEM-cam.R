getwd() 
setwd("/Volumes/cassie/10plant stomata/datamapping/re-analysis")
dat<-read.table('cam-factors',header = TRUE,sep = ',',fileEncoding = 'gbk')
head(dat)
#install.packages("lavaan", dependencies = TRUE)
library(lavaan)
library(vegan)
library(PerformanceAnalytics)
library(semPlot)
rt<-dat[,-(1:2)]
chart.Correlation(rt,histogram = TRUE, pch=19)
model<-'
       Conduct~PAR+WS+VPD+STtop+STbottom+SM
       VPD~WS+SM
       STtop~PAR+WS+SM+STbottom
       STbottom~PAR+WS+SM+STtop
       '
mypath <- sem(model = model, data = rt, se = 'bootstrap', bootstrap = 1000)
mypath
summary(mypath, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
fitmeasures(mypath, c('chisq', 'rmsea', 'cfi', 'aic'))#查看模型拟合度
semPaths(mypath, what = 'std', layout = 'tree', residuals = FALSE, edge.label.cex = 1)
