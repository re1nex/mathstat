library(EEM)
library(eemR)

Arctic<- c("1701.txt","1702.txt","1704.txt","1706.txt","1711.txt","1712.txt","1727.txt","1728.txt","1729.txt","1730.txt","1732.txt","1733.txt","1734.txt")
Africa <-c("1.1_70..txt","1.2_21.txt","1.3_68.txt","1.4_114.txt","1.5_11.txt","1.6_37.txt","2.3_5.txt","2.4_7.txt","3.1_14.txt","3.2_69.txt","3.4_20.txt","3.5_43.txt","4.1_45.txt","4.2_80.txt","4.3_84.txt","4.4_87.txt","4.5_108.txt","4.6_88.txt","5.1_90.txt","5.2_2.txt","5.3_66.txt","5.4_92.txt","5.5_28.txt","5.6_95.txt")

data_Africa <- readEEM(Africa) 
data_Africa_del<- delScattering(data_Africa, rep = NA) 
data_Africa_del <- delScattering2(data_Africa_del, rep = NA) 

data_Africa_cut<-cutEEM(data_Africa_del, cutEX = 0:250, cutEM = 0:250)
data_Africa_cut<-cutEEM(data_Africa_cut, cutEX = 300:700, cutEM = 450:700)

data_Africa_uf <- unfold(data_Africa_cut) 
data_Africa_norm <- normalize(data_Africa_uf ) 
result_Africa <- prcomp(data_Africa_norm) # mean-centering is enabled by default
screeplot(result_Africa, npcs = 10, type = "lines", main = "Screeplot")

data_Arctic <- readEEM(Arctic) 
data_Arctic_del <- delScattering(data_Arctic_del, rep = NA)
data_Arctic_del <- delScattering2(data_Arctic, rep = NA) 

data_Arctic_cut<-cutEEM(data_Arctic_del, cutEX = 0:250, cutEM = 0:250)
data_Arctic_cut<-cutEEM(data_Arctic_cut, cutEX = 300:700, cutEM = 450:700)

data_Arctic_uf <- unfold(data_Arctic_cut) 
data_Arctic_norm <- normalize(data_Arctic_uf ) 
result_Arctic <- prcomp(data_Arctic_norm) # mean-centering is enabled by default
screeplot(result_Arctic, npcs = 10, type = "lines", main = "Screeplot")

cor1Frame<-data.frame(colnames(c("Africa", "Arctic","cor")))
cor2Frame<-data.frame(colnames(c("Africa", "Arctic","cor")))
cor3Frame<-data.frame(colnames(c("Africa", "Arctic","cor")))

for(i in 1:dim(result_Africa$x)[1]){
  for (j in 1:dim(result_Arctic$x)[1]) {
    raf13<-c(result_Africa$x[i, 1],result_Africa$x[i, 2],result_Africa$x[i, 3],result_Africa$x[i, 4],result_Africa$x[i, 5],result_Africa$x[i, 6],result_Africa$x[i, 7],result_Africa$x[i, 8],result_Africa$x[i, 9],result_Africa$x[i, 10],result_Africa$x[i, 11],result_Africa$x[i, 12],result_Africa$x[i, 13])
    rar13<-c(result_Arctic$x[j, 1],result_Arctic$x[j, 2],result_Arctic$x[j, 3],result_Arctic$x[j, 4],result_Arctic$x[j, 5],result_Arctic$x[j, 6],result_Arctic$x[j, 7],result_Arctic$x[j, 8],result_Arctic$x[j, 9],result_Arctic$x[j, 10],result_Arctic$x[j, 11],result_Arctic$x[j, 12],result_Arctic$x[j, 13])
    cor13<-cor(raf13,rar13)
    if(cor13>0.95)
    {
      cor3Frame<-rbind(cor3Frame,list(names(result_Africa$x[,1])[i],names(result_Arctic$x[,1])[j],cor13))
    }else if(cor13>0.85)
    {
      cor2Frame<-rbind(cor2Frame,list(names(result_Africa$x[,1])[i],names(result_Arctic$x[,1])[j],cor13))
    }else if(cor13>0.75)
    {
      cor1Frame<-rbind(cor1Frame,list(names(result_Africa$x[,1])[i],names(result_Arctic$x[,1])[j],cor13))
    }
  }
}

write.table(cor1Frame, "cor75.txt", quote=FALSE, eol="\\\n", sep=" & ")
write.table(cor2Frame, "cor85.txt", quote=FALSE, eol="\\\n", sep=" & ")
write.table(cor3Frame, "cor95.txt", quote=FALSE, eol="\\\n", sep=" & ")

drawEEM(data_Arctic_norm,n=2)
drawEEM(data_Arctic_norm,n=10)
drawEEM(data_Africa_norm,n=7)
drawEEM(data_Africa_norm,n=11)
drawEEM(data_Africa_norm,n=16)






