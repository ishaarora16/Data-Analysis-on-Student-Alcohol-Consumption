#Loading required packages
library(magrittr)
library(dplyr)
library(Gmisc)
library(rattle)
library(caret)
library(Boruta)
library(plyr)
library(ggplot2)

#Math class data
d1=read.table("c:/users/Tulika/Desktop/student-mat.csv",header=TRUE,sep=",")

#Pourtougese class data
d2=read.table("c:/users/Tulika/Desktop/student-por.csv",sep=",",header=TRUE)

d3<-rbind(d1,d2)
#combine the two datasets

data.source=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
                             "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                             "guardian","guardian","traveltime","studytime","failures",
                             "schoolsup","famsup","activities","higher","romantic",
                             "famrel","freetime","goout","Dalc","Walc","health","absences"))
data.source$mathgrades=rowMeans(cbind(data.source$G1.x,data.source$G2.x,data.source$G3.x))
data.source$portgrades=rowMeans(cbind(data.source$G1.y,data.source$G2.y,data.source$G3.y))



# and eliminate the repeats:
d3_unique<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,paid,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

#add a column with average grades (math or Portuguese, whichever is available)
d3_unique$avggrades=rowMeans(cbind(d3_unique$G3,d3_unique$G2))
# and drop grades in 3 marking periods.
d3_unique<-d3_unique[,-(31:33)]

#Summing up the daily and weekend alcohol consumption
d3_unique$t_alc<-rowSums(cbind(d3_unique$Dalc,d3_unique$Walc))

#Making a new attribute to be used in classifier(Low vs High Level Risk )
d3_unique$Risk<-"Low"
d3_unique$Risk[d3_unique$t_alc > 5 | d3_unique$Dalc >= 3 | d3_unique$Walc >= 3] <- "High"
d3_unique$Risk <- factor(d3_unique$Risk, levels = c("Low","High"))

#Correlation matrix
library(corrplot)
str(d3_unique)
d3_unique_numeric = data.frame(lapply(d3_unique,as.numeric))
str(d3_unique_numeric)
M<-cor(d3_unique_numeric)
corrplot(M,method="circle")
#We see that corr plot doesn't give us any proper information of which  attributes can discarded
#And using PCA we lose the column name

#Converting all columns into factor
d3_unique <- data.frame(lapply(d3_unique, as.factor))


#VISUALIZATIONS

#Does free time determine your alcohol intake?
c3 <- ggplot(d3_unique, aes(x=t_alc, y=freetime, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Total alcohol consumption")+
  ylab("Free time on a scale of 5")+
  ggtitle("Total alcohol consumption per school and sex")
c3
#As free time increases , the alcohol consumption is seen to high

#Do your failure rate determine your alcohol consumption
mothpl = ggplot(d3_unique, aes(y = as.numeric(t_alc) , x=failures,fill=failures ))
mothpl + geom_boxplot() + ggtitle("Failure vs Alcohol consumption")
c3 <- ggplot(d3_unique, aes(x=t_alc, y=failures, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff9999", "#468499"))+
  theme_bw()+
  xlab("Total alcohol consumption")+
  ylab("Failure rate on a scale of 5")+
  ggtitle("Total alcohol consumption vs failure rate")
c3


#Mother's education affecting grades
mothpl = ggplot(d3_unique, aes(y = as.numeric(avggrades) , x= Medu,fill=Medu ))
mothpl + geom_boxplot() + ggtitle("Performance and mother's education")
#Gardes tend to improve if mother is edcuated



#Mother's education affecting alcohol consumption
mothpl = ggplot(d3_unique, aes(y = as.numeric(t_alc) , x= Medu,fill=Medu ))
mothpl + geom_boxplot() + ggtitle("Alcohol consumption and mother's education")
#Alcohol consumption is higher if mother is un-edcuated



#Free time affeting alcohol consumption
mothpl = ggplot(d3_unique, aes(y = as.numeric(t_alc) , x= freetime,fill=freetime ,ylim(0,10) ))
mothpl + geom_boxplot() + ggtitle("Free time vs Alcohol consumption")
#More the free time , higher is the alcohol consumption rate


#Male Vs Female Alcohol consumption rate
male_female <- data.frame(d3_unique$sex)
male_female_numeric = data.frame(lapply(male_female,as.numeric))
val <- c("male","female")
# Plot the chart.
library(plotrix)
pie3D(table(male_female_numeric),labels=val, explode=0.1,col=c("pink","blue"),main="Pie Chart of Sex")
#Male students have more tendency to consume alcohol compared to female students.


#DAILY/WEEKLY ALCOHOL CONSUMPTION IN URBAN/RURAL AREAS
rural_data = subset(d3_unique,address == 'R')
urban_data = subset(d3_unique,address == 'U')
rural_data1 = data.frame(cbind("Dalc"=rural_data$Dalc,"Walc"=rural_data$Walc))
urban_data1 = data.frame(cbind("Dalc"=urban_data$Dalc,"Walc"=urban_data$Walc))
par(mfrow=c(2,2))
hist(rural_data1$Dalc,xlab="DALC",main="RURAL DALC")
hist(rural_data1$Walc,xlab="WALC",main="RURAL WALC")
hist(urban_data1$Dalc,xlab="DALC",main="URBAN DALC")
hist(urban_data1$Walc,xlab="WALC",main="URBAN WALC")


#10 fold cross validation
folds<-createFolds(d3_unique$t_alc, k=10, list = TRUE, returnTrain = TRUE)
train_control<- trainControl(method = "cv", number = 10)
model<-train(as.factor(t_alc)~. ,data=d3_unique, trControl=train_control, method="rpart")
fancyRpartPlot(model$finalModel)




#BORUTA PACKAGE FOR FEATURE SELECTION
indexes = sample(1:nrow(d3_unique), size=0.2*nrow(d3_unique))
# Split data
test_boruta = d3_unique[indexes,]
#dim(test) 
traindata_boruta = d3_unique[-indexes,]
traindata_boruta=traindata_boruta[-c(32,27,28)]
#IMPLEMENT boruta
set.seed(123)
x=traindata_boruta$Risk
boruta.train <- Boruta(x~.-traindata_boruta$Risk -traindata_boruta$t_alc, data = traindata_boruta,doTrace = 2)
print(boruta.train)
getSelectedAttributes(boruta.train, withTentative = F)
#Important Features short down to - Medu , Fedu , sex, age , freetime, goout ,school,guardian
# absences,Mjob, Fjob, famrel, avggrades



#SVD FOR FEATURE SELECTION
d3_unique_numeric <- data.frame(lapply(d3_unique, as.numeric))
svd_out <- svd(scale(d3_unique_numeric[-c(32,27,28,33)]))
getSvdMostInfluential(d3_unique_numeric[-c(27,28,32,33)], 
                      quantile=.8, 
                      similarity_threshold = .8,
                      plot_threshold = .05,
                      plot_selection = TRUE)
#Important features - Medu ,Fedu , school,sex,age,absences,goout,freetime


#Random Forest Classifier
library(party)
library(randomForest)
set.seed(4543)
train1 <- sample(nrow(d3_unique),0.8*nrow(d3_unique))
d3.train <- d3_unique[train1,]
d3.test <- d3_unique[-train1,]
d3.train <- data.frame(lapply(d3.train, as.factor))
d3.test<- data.frame(lapply(d3.test, as.factor))
# Create the forest.
output.forest <- randomForest((Risk) ~Medu+Fedu+freetime+sex+school+Mjob+Fjob+avggrades+failures+goout+guardian+absences, 
                              data = d3.train,trControl=train_control,ntree=500)
rf.predictions<-predict(output.forest,d3.test)
#n2=ifelse(rf.predictions>0.5,1,0)
cnf=confusionMatrix(rf.predictions,d3.test$Risk)
# View the forest results.
print(output.forest) 
rf.predictions
cnf

#Accuracy of random forest comes up to 61%


#Neural Network
#neural network
library(neuralnet)
library(plyr)
library(dplyr)
#Marking the daily and weekly alcohol consumption in terms of Safe , General and Risk
d3$dr.dalc <- mapvalues(d3$Dalc, 
                        from = 1:5, 
                        to = c("Safe", "General", "Risk", "Risk", "Risk"))
d3$dr.walc <- mapvalues(d3$Walc, 
                        from = 1:5, 
                        to = c("Safe", "Safe", "General", "General", "Risk"))

#Converting it to numeric
d3$All.alc[d3$dr.dalc == "Safe"& d3$dr.walc == "Safe"] <- 1
d3$All.alc[d3$dr.dalc != "Risk" & d3$dr.walc != "Risk" &
             d3$dr.dalc == "General"|d3$dr.walc == "General"] <- 2
d3$All.alc[d3$dr.dalc == "Risk"| d3$dr.walc == "Risk"] <- 3
d3$All.alc <- as.numeric(d3$All.alc)
d3$fAll.alc <- factor(d3$All.alc, levels = 1:3,
                      labels = c("Safe","General","Risk"))
d4 <- d3[,1:34]
d4$fAll.alc <- d3$fAll.alc
dwalc.v <- names(d4) %in% c("Dalc","Walc")
d4 <- d4[!dwalc.v]

#Making the train and test data
train <- sample(nrow(d4),0.8*nrow(d4))
d4.train <- d4[train,]
d4.test <- d4[-train,]

train1 <- sample(nrow(d3_unique),0.8*nrow(d3_unique))
d3.train <- d3_unique[train1,]
d3.test <- d3_unique[-train1,]
#Use of Validation
#10 fold cross validation repeated 3 times
t.control=trainControl(method="adaptive_boot",number=10,repeats=3)
#Training the model built
nnt.model <- caret::train(fAll.alc~ Medu+Fedu+failures+freetime+goout+sex+Mjob+Fjob+school+G1+G2+G3+absences+guardian+Pstatus, d4.train, trControl = t.control,metric="Kappa",
                  method = "nnet")
nnt.model1<- caret::train(Risk~ Medu+Fedu+failures+freetime+goout+sex+Mjob+Fjob+school+absences+guardian, d3_unique, trControl = t.control,metric="Kappa",
                   method = "nnet")
#Predicting the test model
predi.nnt<- predict(nnt.model,d4.test)
predi.nnt1<- predict(nnt.model1,d3.test)
kappa2(data.frame(d3.test$Risk,predi.nnt1))
#Making the confusion matrix
cM.nnt<- confusionMatrix(predi.nnt,d4.test$fAll.alc)
cM1.nnt<- confusionMatrix(predi.nnt1,d3.test$Risk)
#Accuracy of predicting Risk factor
cM1.nnt

#Accuracy of predicting total alcohol consumption
cM.nnt

#Gives an accuracy of about 77% for prediting the Risk factor
#Accuracy of 60% predicting grades
#Since the dataset is too small,a good accuracy is not being generated
#Future prediction is being done,by using 10 fold cross validation,
#and checking the kappa value which comes out be 0.55.The model can be improved further by removing predictors
#that do not contribute to the variability in the results.
#We could also change the threshold(0.5) value depending on experience.






#XGBOOST
library(xgboost)
require(xgboost)
library(data.table)
library(mlr)
library(caret)
#Converting data to numeric
d3_unique_numeric <- data.frame(lapply(d3_unique, as.numeric))
#Choosing the most important features
d3_unique_numeric<-d3_unique_numeric[c(7,8,9,10,1,2,3,24,12,15,25,26,30,33)]
#Creating a train sample  of 80% data
train_xg <- sample(nrow(d3_unique_numeric),0.8*nrow(d3_unique_numeric))
d3.train_num <- d3_unique_numeric[train_xg,]
d3.test_num <- d3_unique_numeric[-train_xg,]
setDT(d3.train_num) 
setDT(d3.test_num)
labels <- d3.train_num$Risk
ts_label <- d3.test_num$Risk
#One hot encoding of the train and test data is done
new_tr <- model.matrix(~.+0,data = d3.train_num[,-c("Risk"),with=F]) 
new_ts <- model.matrix(~.+0,data = d3.test_num[,-c("Risk"),with=F])

#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 1000, nfold =6, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds =891, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

confusionMatrix(xgbpred,ts_label)
table(factor(xgbpred, levels=min(ts_label):max(ts_label)), factor(ts_label, levels=min(ts_label):max(ts_label)))
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])
#Kappa value =0.63
#   0   1
#0 106  20
#1  17  58
#Accuracy : 83.59% 


#KNN
#K nearest neighbours
library(class)
knn.1 <-  knn(d3.train_num, d3.test_num, d3.train_num$Risk, k=1)
knn.5 <-  knn(d3.train_num, d3.test_num, d3.train_num$Risk, k=5)
knn.20 <-  knn(d3.train_num, d3.test_num, d3.train_num$Risk, k=20)
t1<-table(knn.1 ,d3.test$Risk)
t1
accuracy1<-sum(diag(t1))/sum(t1)
accuracy1
t2<-table(knn.5 ,d3.test$Risk)
t2
accuracy2<-sum(diag(t2))/sum(t2)
accuracy2
t3<-table(knn.20 ,d3.test$Risk)
t3
accuracy3<-sum(diag(t3))/sum(t3)
accuracy3

#SVM
library(e1071)
model_svm <- svm(Risk ~ Medu+Fedu+freetime+sex+school+Mjob+Fjob+failures+goout+guardian+absences+famrel, data = d3.train_num,gamma=3,kernel="radial")
summary(model_svm)
pred <- predict(model_svm, d3.test_num)
pred <- round(pred)
table(d3.test$Risk, pred)
t1 <- confusionMatrix(d3.test_num$Risk, pred)
t1
#Accuracy of 71%













.