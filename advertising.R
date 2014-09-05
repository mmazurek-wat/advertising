library(data.table)
library(rpart) 
library(ggplot2)

library (pmml)

library (vcd)

#regresja logistyczna na probie z zastapieniem brakujacych wartosci 
train<-read.csv2("D:/Kaggle/Display Advertising Challange/train_sample.csv", header = TRUE, sep = ",")
train.dt<-data.table(train)
train_int.dt<-train.dt[,list(Id,Label, I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)]


zero.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- 0
  return (imputed)
}
train_int.dt<-sapply(train_int.dt,zero.imp )





#drzewo decyzyjne 
tree.fit<-rpart(Label~I1+I2 + I3 +I4 + I6 + I7 + I8 + I9 +I10 + I11 + I12 + I13,data=train_int.df)
summary(tree.fit)

score_output<-predict(tree.fit, score.dt)
hist(score_output)

score_final<-cbind(score.dt[,Id], score_output)
score_final.dt<-data.table(score_final)
setnames(score_final.dt,c("Id", "Predicted"))

saveXML(pmml(tree.fit),'tree_pmml.pmml')

options(scipen=10)  #wylaczenie notacji naukowej
write.table(score_final.dt, file="submission_tree.csv", sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)
options(scipen=0)  #wylaczenie notacji naukowej

#sprawdzenie wyniku na wlasnych danych testowych 




#model regresji logistycznej 

regr.fit<-glm(Label~I1+I2 + I3 +I4 + I6 + I7 + I8 + I9 +I10 + I11 + I12 + I13, data=train_int.df, family = binomial )

summary(regr.fit)

score_output<-predict(regr.fit, score.dt)
hist(score_output)






regr.pmml<-pmml(regr.fit)
saveXML(regr.pmml,'regr_pmml.pmml')

summary(regr.fit)





# sprawdzenie separowalnosci ----------------------------------------------

qplot(log(I1), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I2), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I3), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I4), data=train_int.df, geom="bar", fill=factor(Label))

qplot(log(I5), data=train_int.df, geom="bar", fill=factor(Label))

qplot(scale(I2, center=TRUE, scale=TRUE), data=train_int.df, geom="bar", fill=factor(Label), binwidth=0.5)

hist(train_int.df$I2)

qplot(log(I6), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I7), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I8), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I9), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I10), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I11), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I12), data=train_int.df, geom="bar", fill=factor(Label))
qplot(log(I13), data=train_int.df, geom="bar", fill=factor(Label))


ggplot

score.dt<-data.table(read.csv2("D:/Kaggle/Display Advertising Challange/test_int_imp.csv", header = TRUE, sep = ","))
score_output<-predict(regr.fit, score.dt)

score.dt[,Id]
score_output[score.dt[,Id]]

score_final<-cbind(score.dt[,Id], score_output)



write.table(score_final, file="submission1.csv", sep=",", row.names=FALSE, col.names=FALSE)


llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}






test.dt<-read.csv2("D:/Kaggle/Display Advertising Challange/test_Sample.csv", header = TRUE, sep = ",")
test.dt$Prediction<-predict(tree.fit,test.dt)


test.dt<-read.csv2("D:/Kaggle/Display Advertising Challange/train_Sample_200M.csv", header = TRUE, sep = ",")
test.dt$Prediction<-predict(tree.fit,test.dt)


llfun(test.dt$Label, test.dt$Prediction)
#leaderboard = 0.54341
#wlasne dane = 0.51497  /100 tys. rekordow 
#wlasne dane = 0.53767  /250 tys. rekordow 


#wygenerowane losowe wartosci p-swta
test.dt$prediction<-runif(nrow(test.dt),0,1)





#rozwiazania out of the box 

#drzewa dezycyjne - do uzupelnienia problem zwiazany z dodatkowymi wartosciami zmiennych kategoryzujacych. 
m<-rpart(Label~C1 +  C2 +  C3 + C4, data=train)

summary(m)
p<-predict(m,test.dt)


#regresja logistyczna 





test.imp<-data.table(copy(test.dt))

int.col<-list('I1', 'I2', 'I3')

lapply(int.col,  function(x)  test.imp[is.na(x), x:=round(mean(test.imp$x, na.rm=TRUE))] )

#zastapienie wartosci w zbiorze testowym 
test.imp[is.na(I1), I1:=round(mean(test.imp$I1, na.rm=TRUE))]
test.imp[is.na(I2), I2:=round(mean(test.imp$I2, na.rm=TRUE))]
test.imp[is.na(I3), I3:=round(mean(test.imp$I3, na.rm=TRUE))]
test.imp[is.na(I4), I4:=round(mean(test.imp$I4, na.rm=TRUE))]
test.imp[is.na(I5), I5:=round(mean(test.imp$I5, na.rm=TRUE))]
test.imp[is.na(I6), I6:=round(mean(test.imp$I6, na.rm=TRUE))]
test.imp[is.na(I7), I7:=round(mean(test.imp$I7, na.rm=TRUE))]
test.imp[is.na(I8), I8:=round(mean(test.imp$I8, na.rm=TRUE))]
test.imp[is.na(I9), I9:=round(mean(test.imp$I9, na.rm=TRUE))]
test.imp[is.na(I10), I10:=round(mean(test.imp$I10, na.rm=TRUE))]
test.imp[is.na(I11), I11:=round(mean(test.imp$I11, na.rm=TRUE))]
test.imp[is.na(I12), I12:=round(mean(test.imp$I12, na.rm=TRUE))]
test.imp[is.na(I13), I13:=round(mean(test.imp$I13, na.rm=TRUE))]



test.imp[is.na(I1), I1:=0]
test.imp[is.na(I2), I2:=0]
test.imp[is.na(I3), I3:=0]
test.imp[is.na(I4), I4:=0]
test.imp[is.na(I5), I5:=0]
test.imp[is.na(I6), I6:=0]
test.imp[is.na(I7), I7:=0]
test.imp[is.na(I8), I8:=0]
test.imp[is.na(I9), I9:=0]
test.imp[is.na(I10), I10:=0]
test.imp[is.na(I11), I11:=0]
test.imp[is.na(I12), I12:=0]
test.imp[is.na(I13), I13:=0]



#przygotowanie zbioru wejsciowego 



regr.fit<-lm(Label~I1+I2 + I3 +I4 + I6 + I7 + I8 + I9 +I10 + I11 + I12 + I13, data=train )

pr<-predict(regr.fit, test.imp)
llfun(test.imp$Label,pr)


ftable(train.imp)

structable(train)


random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}


list(colnames(score.dt)[1:14])
score.int.dt<-score.dt[,list(Id, I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)]

score.int.imp.dt<-sapply(score.int.dt,zero.imp )


tappltest.imp$I1<-random.imp(test.imp$I1)

sapply(test.imp,random.imp )

