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
  #print(x) 
  freq<-table(x)
  cumfreq<-cumsum(freq)/(length(x[!is.na(x)]))  
  rel<-freq/(length(x[!is.na(x)]))
  char_labels<-paste0(rownames(rel[rel>y]),"+")
  breaks<-append(as.numeric(rownames(rel[rel>y])), max(freq))
  cuts<-cut(x, breaks, char_labels, right=FALSE, include.lowest=TRUE) 
  return (cuts)
}



generate_cuts<-function(x,y){
  freq<-table(x)
  cumfreq<-cumsum(freq)/(length(x[!is.na(x)]))  
  rel<-freq/(length(x[!is.na(x)]))
  char_labels<-paste0(rownames(rel[rel>y]),"+")
  breaks<-append(as.numeric(rownames(rel[rel>y])), max(freq))
  l=list(char_labels, breaks)
  return (l)  
}


lcuts<-list()

for (i in names(train_int.dt)[3:ncol(train_int.dt)]){
  q<-quote(i)
  lcuts[[i]]<- generate_cuts(train_int.dt[,eval(q), with=FALSE][[1]], 0.01)
}


#zastosowanie do zbioru testowego: 
test_bins<-test_int.dt[, list(Id, Label,
                               I1_bin=cut(I1, unlist(lcuts$I1[2]), unlist(lcuts$I1[1]), right=FALSE, include.lowest=TRUE)  ,
                                I2_bin=cut(I1, unlist(lcuts$I2[2]), unlist(lcuts$I2[1]), right=FALSE, include.lowest=TRUE)  ,
                                I3_bin=cut(I1, unlist(lcuts$I3[2]), unlist(lcuts$I3[1]), right=FALSE, include.lowest=TRUE)  ,
                                I4_bin=cut(I1, unlist(lcuts$I4[2]), unlist(lcuts$I4[1]), right=FALSE, include.lowest=TRUE)  ,
                                I5_bin=cut(I1, unlist(lcuts$I5[2]), unlist(lcuts$I5[1]), right=FALSE, include.lowest=TRUE)  ,
                                I6_bin=cut(I1, unlist(lcuts$I6[2]), unlist(lcuts$I6[1]), right=FALSE, include.lowest=TRUE)  ,
                                I7_bin=cut(I1, unlist(lcuts$I7[2]), unlist(lcuts$I7[1]), right=FALSE, include.lowest=TRUE)  ,
                                I8_bin=cut(I1, unlist(lcuts$I8[2]), unlist(lcuts$I8[1]), right=FALSE, include.lowest=TRUE)  ,
                                I9_bin=cut(I1, unlist(lcuts$I9[2]), unlist(lcuts$I9[1]), right=FALSE, include.lowest=TRUE)  ,
                                I10_bin=cut(I1, unlist(lcuts$I10[2]), unlist(lcuts$I10[1]), right=FALSE, include.lowest=TRUE)  ,
                                I11_bin=cut(I1, unlist(lcuts$I11[2]), unlist(lcuts$I11[1]), right=FALSE, include.lowest=TRUE)  ,
                                I12_bin=cut(I1, unlist(lcuts$I12[2]), unlist(lcuts$I12[1]), right=FALSE, include.lowest=TRUE)  ,
                                I13_bin=cut(I1, unlist(lcuts$I13[2]), unlist(lcuts$I13[1]), right=FALSE, include.lowest=TRUE)                                  
)
]





#wzorocowe dla pojedynczej zmiennej: 
train_int.dt[,I5bin:=generate_bins(I1,0.01)]

train_int.dt[,I5bin:=NULL]

#TODO: jak mozna zapisac taka petle inaczej  ?  
train_bins<-train_int.dt[, list(Id, Label,I1_bin=generate_bins(I1,0.01), 
                                I2_bin=generate_bins(I2,0.01),
                                I3_bin=generate_bins(I3,0.01),
                                I4_bin=generate_bins(I4,0.01),
                                I5_bin=generate_bins(I5,0.01),
                                I6_bin=generate_bins(I6,0.01),
                                I7_bin=generate_bins(I7,0.01),
                                I8_bin=generate_bins(I8,0.01),
                                I9_bin=generate_bins(I9,0.01),
                                I10_bin=generate_bins(I10,0.01),
                                I11_bin=generate_bins(I11,0.01),
                                I12_bin=generate_bins(I12,0.01),
                                I13_bin=generate_bins(I13,0.01)                                
                                )
             ]

train_bins$Label<-factor(train_bins$Label)
save(train_bins, file="train_bins")
save(test_bins, file="test_bins")
save(train_int.dt, file="train_int")


v<-"I1bin"

ggplot(train_bins,aes_string(x = v, fill = "Label")) + geom_bar(position="fill") 



remove_na = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)),i:="EMPTY",with=FALSE]
}

#usuniecie pustych wartosc 
remove_na(train_bins)




regr.fit<-glm(Label~I1_bin+I2_bin + I3_bin +I4_bin + I6_bin + I7_bin + I8_bin + I9_bin +I10_bin + I11_bin + I12_bin + I13_bin, data=train_bins, family = binomial )
summary(regr.fit)



test_bins$Label<-test_bins$Label
remove_na(test_bins)

score_output<-predict(regr.fit, test_bins, type="response")


llfun(test_bins$Label, score_output)



