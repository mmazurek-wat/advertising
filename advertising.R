library(data.table)
library(rpart) 
library(ggplot2)

library (pmml)

library (vcd)



# Wspolne funkcje i zbiory danych -----------------------------------------


llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

zero.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- 0
  return (imputed)
}


random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}


train<-read.csv2("D:/Kaggle/Display Advertising Challange/train_sample.csv", header = TRUE, sep = ",")
train.dt<-data.table(train)
train_int.dt<-data.table(train.dt[,list(Id,Label, I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)])


train_int.dt<-data.table(sapply(train_int.dt,zero.imp ))
#train_int_std.dt<-sapply(train_int.dt, function(x) scale(x, scale=TRUE, center=TRUE))
#train_int_std.dt


vars <- c("I1", "I2") # <- Choose the variable you want to operate on
train.dt[,lapply(.SD,function(x) sum(as.numeric(x))),]

train_int.dt[, paste0(vars, "_", "NULL") := lapply(.SD, function(x) {x+1}), .SDcols = vars]



train_int.dt[is.na(train_int.dt$I1),I1_missing:=1,]

#inicjalizaca zbioru uczacego 
train_int_std.dt<-sapply(train_int.dt[,list( I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)], function(x) {scale(x, scale=TRUE, center=TRUE)})

vars <- c("I1", "I2") # <- Choose the variable you want to operate on


train_int.dt[, paste0(vars, "_", "NULL") := lapply(.SD, function(x) {if (is.na(x)) {1} else {0} }),, .SDcols = vars]
train_int.dt[, paste0(vars, "_", "sum") := lapply(.SD, sum), .SDcols = vars, by = Label]



train_int_std.dt<-sapply(train_int_std.dt[,list( I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)], function(x) {if(abs(x)>3){x=3}})

train_int_std.dt<-cbind(train_int.dt[,list( Id, Label)], train_int_std.dt)


#obciecie wartosci odstajacych 
trains_int_std_outliers.dt<-

train_int_std.dt<-data.table(train_int_std.dt)



#dwa  zbiory danych testowych 
test.dt<-read.csv2("D:/Kaggle/Display Advertising Challange/test_Sample.csv", header = TRUE, sep = ",")
test.dt<-read.csv2("D:/Kaggle/Display Advertising Challange/train_Sample_200M.csv", header = TRUE, sep = ",")

test.dt<-data.table(test.dt)
test_int.dt<-data.table(test.dt[,list(Id,Label, I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)])
save(test_int.dt, file="test_int")

#standaryzacja zbioru testowego 

test_int.dt<-data.table(sapply(test.dt,zero.imp ))
test_int_std.dt<-sapply(test_int.dt[,list( I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)], function(x) {scale(x, scale=TRUE, center=TRUE)})
test_int_std.dt<-cbind(test_int.dt[,list( Id, Label)], test_int_std.dt)
test_int_std.dt<-data.table(test_int_std.dt)




#zbior z konkursu do oceny 
score.dt<-data.table(read.csv2("D:/Kaggle/Display Advertising Challange/test_int_imp.csv", header = TRUE, sep = ","))
score.int.dt<-score.dt[,list(Id, I1, I2, I3, I4, I5, I6, I7,I8, I9, I10, I11, I12,I13)]
score.int.imp.dt<-sapply(score.int.dt,zero.imp )
sapply(test.imp,random.imp )



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

#mala proba 
test.dt<-read.csv2("D:/Kaggle/Display Advertising Challange/test_Sample.csv", header = TRUE, sep = ",")
test.dt$Prediction<-predict(tree.fit,test.dt)

#wlasciowa proba testowa 
test.dt<-read.csv2("D:/Kaggle/Display Advertising Challange/train_Sample_200M.csv", header = TRUE, sep = ",")
test.dt$Prediction<-predict(tree.fit,test.dt)


llfun(test.dt$Label, test.dt$Prediction)
#leaderboard = 0.54341
#wlasne dane = 0.51497  /100 tys. rekordow 
#wlasne dane = 0.53767  /250 tys. rekordow 


#wygenerowane losowe wartosci p-swta
test.dt$prediction<-runif(nrow(test.dt),0,1)






# Regresja logistyczna ----------------------------------------------------




#model regresji logistycznej 

regr.fit<-glm(factor(Label)~I1+I2 + I3 +I4 + I6 + I7 + I8 + I9 +I10 + I11 + I12 + I13, data=train_int.dt, family = binomial )
summary(regr.fit)

train_int_std.dt$Label

score_output<-predict(regr.fit, test_int_std.dt, type="response")


llfun(test_int_std.dt$Label, score_output)

#ocena 


hist(score_output)




