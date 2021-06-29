train_set<-read.csv(file.choose(),TRUE)
test_set<-read.csv(file.choose(),TRUE)
head(train_set)



#Preprocessing missing values in Test set by median values::
# install.packages('superml') library(superml)-------------no need since this can be achieved with dat.matrix
test_set_new<-data.matrix(test_set_new)
str(test_set)
head(test_set)
for(i in 1:ncol(test_set)){
  test_set[is.na(test_set[,i]),i]<-median(test_set[,i],na.rm =TRUE)
}
library(data.table)

test_date_time<-as.data.frame(test_set$datetime,stringsAsFactors = FALSE)
testdatetime<-as.data.frame(tstrsplit(test_date_time[,1],'[ ]',type.convert = TRUE),stringsAsFactors = FALSE)
head(testdatetime)
testdate<-as.data.frame(tstrsplit(testdatetime[,1],'[-]',type.convert = TRUE),stringsAsFactors = FALSE)
head(testdate)
testtime<-as.data.frame(tstrsplit(testdatetime[,2],'[:]',type.convert = TRUE),stringsAsFactors = FALSE)
head(testtime)

years<-testdate[,1]
month<-testdate[,2]
day<-testdate[,3]

hour<-testtime[,1]
min<-testtime[,2]
sec<-testtime[,3]

test_set_new<-cbind(test_set[1],years,month,day,hour,min,sec,test_set[3:21])
head(test_set_new)
#---------------------removing NA values in test_set_new

for(i in 1:ncol(test_set_new)){
  test_set_new[is.na(test_set_new[,i]),i]<-median(test_set_new[,i],na.rm =TRUE)
}

#------------------Train data manupulation.

train_date_time<-as.data.frame(train_set$datetime,stringsAsFactors = FALSE)
traindatetime<-as.data.frame(tstrsplit(train_date_time[,1],'[ ]',type.convert = TRUE),stringsAsFactors = FALSE)
head(traindatetime)
traindate<-as.data.frame(tstrsplit(traindatetime[,1],'[-]',type.convert = TRUE),stringsAsFactors = FALSE)
head(traindate)
traintime<-as.data.frame(tstrsplit(traindatetime[,2],'[:]',type.convert = TRUE),stringsAsFactors = FALSE)
head(traintime)

years<-traindate[,1]
month<-traindate[,2]
day<-traindate[,3]

hour<-traintime[,1]
min<-traintime[,2]
sec<-traintime[,3]

train_set_new<-cbind(train_set[1],years,month,day,hour,min,sec,train_set[3:22])
head(train_set_new)
str(test_set_new)
test_set_new<-data.matrix(test_set_new)


#------------------------------------------------------------------------------------------------------

#----------------------Tried Feature Selection
library(Boruta)
boruta_output<-Boruta(windmill_generated_power.kW.h. ~ .,na.omit(train_set),doTrace=1)

#----------------------Tried Linear Regression
relation<-lm(windmill_generated_power.kW.h. ~ .,data=train_set[-c(1,2,17)])

test_set[is.na(test_set)]=0
head(test_set)
output<-predict(relation,test_set[-c(1,2,17)])
write.csv(output,"output1.csv")


#----------------------Extreme Gradient Boosting

install.packages('xgboost')
library(xgboost)
train_set<-data.matrix(na.omit(train_set))
test_set<-data.matrix(test_set)
xgb_train<-xgb.DMatrix(data=train_set[,-22],label=train_set[,22])
xgb_test<-xgb.DMatrix(data=test_set)
help(xgb)
xgbc<-xgboost(data=xgb_train,max.depth=100,nrounds=200,eta=0.07)
print(xgbc)
pred<-predict(xgbc,xgb_test)
write.csv(pred,"output1.csv")

#------------------Same for new set
install.packages('xgboost')
library(xgboost)
head(train_set_new)
train_set_new<-data.matrix(na.omit(train_set_new))
test_set_new<-data.matrix(test_set_new)
xgb_train<-xgb.DMatrix(data=train_set_new[,-c(1,27)],label=train_set_new[,27])
xgb_test<-xgb.DMatrix(data=test_set_new[,-c(1)])
help(xgb)
xgbc<-xgboost(data=xgb_train,max.depth=100,nrounds=300,eta=0.1)
print(xgbc)
pred<-predict(xgbc,xgb_test)
write.csv(pred,"output1.csv")