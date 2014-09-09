


load("train_int")
load("test_int")

#benchmark dla wartosci nie pogrupowanych zastapionych 0 

SetNAsToMean <- function(dt, vars) {                                                                                                                              
  for (var in vars) {                                                                                                                                            
    #set(dt, which(is.na(dt[[var]])), var, mean(dt[[var]], na.rm=T))                                                                                              
    #set(dt, which(is.na(dt[[var]])), var, 0)   
    #set(dt, which(is.na(dt[[var]])), var,   sample (table(dt[[var]][1:5]), 1, replace=TRUE))                                                                                              
  
  }                                                                                                                                                              
}  


SetNAsToMean(test_int.dt, vars)
SetNAsToMean(train_int.dt, vars)



regr.fit<-glm(factor(Label)~I1+I2 + I3 +I4 + I6 + I7 + I8 + I9 +I10 + I11  + I12+  I13, data=train_int.dt, family = binomial )
summary(regr.fit)
score_output<-predict(regr.fit, test_int.dt, type="response")
llfun(test_int.dt$Label, score_output)

#braki zastepowane 0 : 0.5389969
#braki zastepowane srednia :0.5389007
#braki zastepowane losowana wartoscia:  2.478152


load("train_int")
load("test_int")



generate_bins_num<-function(x,y){
  #print(x) 
  freq<-table(x)
  cumfreq<-cumsum(freq)/(length(x[!is.na(x)]))  
  rel<-freq/(length(x[!is.na(x)]))
  char_labels<-paste0(rownames(rel[rel>y]),"")
  breaks<-append(as.numeric(rownames(rel[rel>y])), max(freq))
  cuts<-cut(x, breaks, char_labels, right=FALSE, include.lowest=TRUE) 
  return (cuts)
}

train_bins_num<-train_int.dt[, list(Id, Label,I1=generate_bins_num(I1,0.01), 
                                I2=generate_bins_num(I2,0.01),
                                I3=generate_bins_num(I3,0.01),
                                I4=generate_bins_num(I4,0.01),
                                I5=generate_bins_num(I5,0.01),
                                I6=generate_bins_num(I6,0.01),
                                I7=generate_bins_num(I7,0.01),
                                I8=generate_bins_num(I8,0.01),
                                I9=generate_bins_num(I9,0.01),
                                I10=generate_bins_num(I10,0.01),
                                I11=generate_bins_num(I11,0.01),
                                I12=generate_bins_num(I12,0.01),
                                I13=generate_bins_num(I13,0.01)                                
)
]

#konwersja do numerycznej 


train_bins_num<-train_bins_num[, lapply(.SD,as.numeric),]

vars<-colnames(train_bins_num)[3:15]
  
SetNAsToMean <- function(dt, vars) {                                                                                                                              
  for (var in vars) {                                                                                                                                            
    #set(dt, which(is.na(dt[[var]])), var, mean(dt[[var]], na.rm=T))                                                                                              
    set(dt, which(is.na(dt[[var]])), var, 0)                                                                                              
  }                                                                                                                                                              
}           


SetNAsToMean(train_bins_num, vars)

regr.fit<-glm(factor(Label)~I1+I2 + I3 +I4 + I6 + I7 + I8 + I9 +I10 + I11 + I12 + I13, data=train_bins_num, family = binomial )
summary(regr.fit)



#koszyki dla zbioru testowego ? 
test_bins_num<-test_int.dt[, list(Id, Label,I1=generate_bins_num(I1,0.01), 
                                    I2=generate_bins_num(I2,0.01),
                                    I3=generate_bins_num(I3,0.01),
                                    I4=generate_bins_num(I4,0.01),
                                    I5=generate_bins_num(I5,0.01),
                                    I6=generate_bins_num(I6,0.01),
                                    I7=generate_bins_num(I7,0.01),
                                    I8=generate_bins_num(I8,0.01),
                                    I9=generate_bins_num(I9,0.01),
                                    I10=generate_bins_num(I10,0.01),
                                    I11=generate_bins_num(I11,0.01),
                                    I12=generate_bins_num(I12,0.01),
                                    I13=generate_bins_num(I13,0.01)                                
)
]
test_bins_num<-test_bins_num[, lapply(.SD,as.numeric),]
SetNAsToMean(test_bins_num, vars)

score_output<-predict(regr.fit, test_bins_num, type="response")
llfun(test_bins_num$Label, score_output)

#0.5401354 dla koszykow: 0.001

