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



train_int.dt[,I1bin:=NULL]
cols=c("I1", "I2", "I3", "I4")
qcols=quote(cols)


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


#zastosowanie tych samych tranformacji do zbioru testowego 
test_bins<-test_int.dt[, list(Id, Label,I1_bin=generate_bins(I1,0.01), 
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

test_bins$Label<-factor(test_bins$Label)
remove_na(test_bins)

score_output<-predict(regr.fit, test_bins, type="response")


llfun(test_int_std.dt$Label, score_output)


