library(data.table)
library(rpart) 
library(ggplot2)

library (pmml)

library (vcd)


load(file="train_bins")
vars<-colnames(train_bins)[3:ncol(train_bins)]
Label="Label"
for (v in vars){
  g<-ggplot(train_bins,aes_string(x = v , fill =Label)) + geom_bar(position="fill") 
  print(g )
  
  g<-ggplot(train_bins,aes_string(x = v , fill =Label)) + geom_bar()
  print(g )
  
}