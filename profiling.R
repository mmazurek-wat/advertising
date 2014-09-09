library(ggplot2)
library(scales)
library(reshape)
library(data.table)

hist(train.dt$I1)
qplot(I1, data=train_int_std.dt, geom="bar", fill = factor(Label))


#tabela czestosci 
x<-table(train.dt$I1, train.dt$Label)

chisq.test(x)

x<-table(train.dt$I3)
x


train_int.dt$I1_q<-with(train_int.dt, cut(I1,  breaks=as.numeric(rownames(rel[rel>0.01])), na.rm=TRUE)),
                                          include.lowest=TRUE)

quantile(train_int.dt$I2, probs=seq(0,1,by=0.2), na.rm=TRUE)
min(train_int.dt$I2)
max(train_int.dt$I2)


train_melted<-melt(train_int.dt, id=c("Id", "Label"))


ggplot(train_melted, aes(variable, log(value))) + geom_boxplot() 



generate_bins<-function(x,y){
  freq<-table(x)
  cumfreq<-cumsum(freq)/(length(x[!is.na(x)]))  
  rel<-freq/(length(x[!is.na(x)]))
  char_labels<-paste0(rownames(rel[rel>y]),"+")
  breaks<-append(as.numeric(rownames(rel[rel>y])), max(freq))
  cuts<-cut(x, breaks, char_labels, right=FALSE, include.lowest=TRUE) 
  return (cuts)
}


train_int.dt[,I1bin2:=generate_bins(I2,0.01)]
ggplot(train_int.dt,aes(x = I1bin2, fill = factor(Label))) + geom_bar(position="fill") 







