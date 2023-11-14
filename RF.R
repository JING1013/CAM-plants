setwd('/Volumes/cassie/10plant stomata/datamapping/re-analysis')
getwd()
library(ggplot2)
library(vegan)
library(randomForest)
library(rfPermute)
library(rfUtilities)
library(tidyverse)
library(patchwork)
library(reshape)
#library(export)
rt<-read.table('cam-factors.csv',header = TRUE,sep = ',',fileEncoding = 'gbk')
head(rt)
str(rt)

set.seed(1)
RF<-randomForest(Conduct~.,rt[,-1:-2],importance=T)
RFs<-rfPermute(Conduct~.,rt[,-1:-2],nperm=99,ntree=501);RFs
RF;importance(RFs)[,1:2]

df1<- subset(rt, types == "SSB1")[,-1:-2]
set.seed(1)
RF1<-randomForest(Conduct~.,df1,importance=T)
RF1s<-rfPermute(Conduct~.,df1,nperm=99,ntree=501)
RF1;importance(RF1s)[,1:2]

df2<- subset(rt, types == "SSB2")[,-1:-2]
set.seed(1)
RF2<-randomForest(Conduct~.,df2,importance=T)
RF2s<-rfPermute(Conduct~.,df2,nperm=99,ntree=501)
RF2;importance(RF2s)[,1:2]

df3<- subset(rt, types == "SSB3")[,-1:-2]
set.seed(1)
RF3<-randomForest(Conduct~.,df3,importance=T)
RF3s<-rfPermute(Conduct~.,df3,nperm=99,ntree=501)
RF3;importance(RF3s)[,1:2]

df4<- subset(rt, types == "SAH1")[,-1:-2]
set.seed(1)
RF4<-randomForest(Conduct~.,df4,importance=T)
RF4s<-rfPermute(Conduct~.,df4,nperm=99,ntree=501)
RF4;importance(RF4s)[,1:2]

df5<- subset(rt, types == "SAH2")[,-1:-2]
set.seed(1)
RF5<-randomForest(Conduct~.,df5,importance=T)
RF5s<-rfPermute(Conduct~.,df5,nperm=99,ntree=501)
RF5;importance(RF5s)[,1:2]

df6<- subset(rt, types == "SAH3")[,-1:-2]
set.seed(1)
RF6<-randomForest(Conduct~.,df6,importance=T)
RF6s<-rfPermute(Conduct~.,df6,nperm=99,ntree=501)
RF6;importance(RF6s)[,1:2]

unique(rt$types)
bar<-data.frame(variable = c("whole",unique(rt$types)),
                Exp = c(66.86, 66.74, 59.11, 50.08,31.31,57.57,36.53))
bar$variable<- factor(bar$variable, levels = bar$variable)
barplot<- ggplot(bar, aes(variable, Exp))+
  #geom_bar(stat = "identity", fill = "steelblue")+
  geom_bar(stat = "identity", fill = "#99cdce",alpha = 0.8,width = 0.75 )+
  scale_y_continuous(expand = c(0,0))+
  theme_classic(base_line_size = 0.75)+
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(color = "black", size = 14))+
  labs(title = "Explained variation (%)", size = 14)
barplot


circle<- data.frame(rt = colnames(rt)[4:9]) %>%
  left_join(data.frame(rt = row.names(importance(RFs)),
                       whole = ifelse(importance(RFs)[,2]<0.05,
                                      importance(RFs)[,1], NA))) %>%
  left_join(data.frame(rt= row.names(importance(RF1s)),
                       SBB1 = ifelse(importance(RF1s)[,2]<0.05,
                                     importance(RF1s)[,1], NA))) %>%
  left_join(data.frame(rt = row.names(importance(RF2s)),
                       SBB2 = ifelse(importance(RF2s)[,2]<0.05,
                                     importance(RF2s)[,1], NA))) %>%
  left_join(data.frame(rt = row.names(importance(RF3s)),
                       SBB3 = ifelse(importance(RF3s)[,2]<0.05,
                                     importance(RF3s)[,1], NA))) %>%
  left_join(data.frame(rt = row.names(importance(RF4s)),
                       SAH1 = ifelse(importance(RF4s)[,2]<0.05,
                                     importance(RF4s)[,1], NA))) %>%
  left_join(data.frame(rt = row.names(importance(RF5s)),
                       SAH2 = ifelse(importance(RF5s)[,2]<0.05,
                                     importance(RF5s)[,1], NA))) %>% 
  left_join(data.frame(rt = row.names(importance(RF5s)),
                       SAH3 = ifelse(importance(RF6s)[,2]<0.05,
                                     importance(RF6s)[,1], NA))) %>% 
  melt(id = "rt", value.name = "Importance");circle
circle$rt<- factor(circle$rt, rev(colnames(rt)[4:9]))

r<-data.frame(rt=colnames(rt)[4:9],
              whole = (cor(rt[,-1:-2],method = c("spearman")))[1,2:7],
              SBB1 =  (cor(df1,method = c("spearman")))[1,2:7],
              SBB2 =  (cor(df2,method = c("spearman")))[1,2:7],
              SBB3 =  (cor(df3,method = c("spearman")))[1,2:7],
              SAH1 =  (cor(df4,method = c("spearman")))[1,2:7],
              SAH2 =  (cor(df5,method = c("spearman")))[1,2:7],
              SAH3 =  (cor(df6,method = c("spearman")))[1,2:7]) %>%
  melt(id = "rt", value.name = "Correlation");r
r$rt<-factor(r$rt,levels = rev(colnames(rt)[4:9]))
heatmap<- ggplot()+
  geom_tile(data = r, aes(x = variable, y = rt, fill = value))+ #热图
  scale_fill_gradientn(colors = c('#F16C23', 'white', '#1B7C3D'), 
                       limit = c(-1, 1),name='Correlation')+
  geom_point(data = circle, aes(x = variable, y = rt, 
                                size = value), shape = 21,)+ #圆圈大小
  #scale_fill_continuous(name='Importance')+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, color = "black",
                                   size = 12, vjust = 0.6), 
        axis.text.y = element_text(color = 'black', size = 12),
        legend.title = element_text(size = 10))+
  labs(y = '', x = '')
heatmap

barplot + heatmap + 
  plot_layout(ncol = 1, heights = c(1, 6))
