#this file is SMS Spam filter codes with different classifiers in R language
#Written by: Reza Rahimi 
#Initialization: Raw Data (http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection), 
#loading required packages, libraries and function declaration

#required package for text mining
if(!require("tm"))
  install.packages("tm")

#required package for SVM
if(!require("e1071"))
	install.packages("e1071")

#required package for KNN
if(!require("RWeka"))
install.packages("RWeka", dependencies = TRUE)

#required package for Adaboost
if(!require("ada"))
	install.packages("ada")
library("tm")
library("e1071")
library(RWeka)
library("ada")

#Initialize random generator
set.seed(1245)

#This function makes vector (Vector Space Model) from text message using highly repeated words
vsm<-function(message,highlyrepeatedwords){
  
  tokenizedmessage<-strsplit(message, "\\s+")[[1]]
  
  #making vector
  v<-rep(0, length(highlyrepeatedwords))
  for(i in 1:length(highlyrepeatedwords)){
    for(j in 1:length(tokenizedmessage)){
      if(highlyrepeatedwords[i]==tokenizedmessage[j]){
        v[i]<-v[i]+1
      }
    }
  }
  return (v)
}
#loading data. Original data is from http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection
print("Uploading SMS Spams and Hams!\n")
sms.dir <- "./data/smsspamcollection/SMSSpamCollection"
smstable <- read.table(sms.dir,sep="\t",quote="",fill=FALSE,col.names=c("class", "sms"),)
#smstable<-read.csv("data/smsspamcollection/smsspamcollection", header = TRUE, sep = "\t", colClasses=c("type"="character","sms"="character"))

smstabletmp<-smstable

print("Extracting Ham and Spam Basic Statistics!")

smstabletmp$type[smstabletmp$class=="ham"] <- 1
smstabletmp$type[smstabletmp$class=="spam"] <- 0

smstabletmp <- smstabletmp[,2:3]

#Convert character data into numeric
tmp<-as.numeric(smstabletmp$type)

#Basic Statisctics like mean and variance of spam and hams
hamavg<-mean(tmp)
print("Average Ham is :");hamavg

hamvariance<-var(tmp)
print("Var of Ham is :");hamvariance

print("Extract average token of Hams and Spams!")

nohamtokens<-0
noham<-0
nospamtokens<-0
nospam<-0
smstable$sms <- as.character(smstable$sms)
for(i in 1:length(smstable$class)){
  if(smstable[i,1]=="ham"){
    nohamtokens<-length(strsplit(smstable[i,2], "\\s+")[[1]])+nohamtokens
    noham<-noham+1
  }else{ 
    nospamtokens<-length(strsplit(smstable[i,2], "\\s+")[[1]])+nospamtokens
    nospam<-nospam+1
  }
}

totaltokens<-nospamtokens+nohamtokens;
print("total number of tokens is:")
print(totaltokens)

avgtokenperham<-nohamtokens/noham
print("Avarage number of tokens per ham message")
print(avgtokenperham)

avgtokenperspam<-nospamtokens/nospam
print("Avarage number of tokens per spam message")
print(avgtokenperspam)

print(" Make two different sets, training data and test data!")

#select the percent of data that you want to use as training set
trdatapercent<-0.3

#training data set
trdata=NULL

#test data set
tedata=NULL

for(i in 1:length(smstable$class)){
  if(runif(1)<trdatapercent){
    trdata=rbind(trdata,c(smstable[i,1],tolower(smstable[i,2])))
  }
  else{
    tedata=rbind(tedata,c(smstable[i,1],tolower(smstable[i,2])))
  }
}

print("Training data size is!")
dim(trdata)

print("Test data size is!")
dim(tedata)

# Text feature extraction using tm package

trsmses<-Corpus(VectorSource(trdata[,2]))
trsmses<-tm_map(trsmses, stripWhitespace)
trsmses<-tm_map(trsmses, tolower)
trsmses<-tm_map(trsmses, removeWords, stopwords("english"))
trsmses<-tm_map(trsmses, PlainTextDocument)
dtm <- DocumentTermMatrix(trsmses)

highlyrepeatedwords<-findFreqTerms(dtm, 80)

#These highly used words are used as an index to make VSM 
#(vector space model) for trained data and test data

#vectorized training data set
vtrdata=NULL

#vectorized test data set 
vtedata=NULL
for(i in 1:length(trdata[,2])){
  if(trdata[i,1]==1){
    vtrdata=rbind(vtrdata,c(1,vsm(trdata[i,2],highlyrepeatedwords)))
  }
  else{
    vtrdata=rbind(vtrdata,c(0,vsm(trdata[i,2],highlyrepeatedwords)))
  }
  
}

for(i in 1:length(tedata[,2])){
  if(tedata[i,1]==1){
    vtedata=rbind(vtedata,c(1,vsm(tedata[i,2],highlyrepeatedwords)))
  }
  else{
    vtedata=rbind(vtedata,c(0,vsm(tedata[i,2],highlyrepeatedwords)))
  }
  
}

# Run different classification algorithms
# differnet SVMs with different Kernels
print("----------------------------------SVM-----------------------------------------")
print("Linear Kernel")
svmlinmodel <- svm(x=vtrdata[,2:length(vtrdata[1,])],y=vtrdata[,1],type='C', kernel='linear');
summary(svmlinmodel)
predictionlin <- predict(svmlinmodel, vtedata[,2:length(vtedata[1,])])
tablinear <- table(pred = predictionlin , true = vtedata[,1]); tablinear
precisionlin<-sum(diag(tablinear))/sum(tablinear);
print("General Error using Linear SVM is (in percent):");(1-precisionlin)*100
print("Ham Error using Linear SVM is (in percent):");(tablinear[1,2]/sum(tablinear[,2]))*100
print("Spam Error using Linear SVM is (in percent):");(tablinear[2,1]/sum(tablinear[,1]))*100

print("Polynomial Kernel")
svmpolymodel <- svm(x=vtrdata[,2:length(vtrdata[1,])],y=vtrdata[,1], kernel='polynomial', probability=FALSE)
summary(svmpolymodel)
predictionpoly <- predict(svmpolymodel, vtedata[,2:length(vtedata[1,])])
tabpoly <- table(pred = predictionpoly , true = vtedata[,1]); tabpoly

print("Radial Kernel")
svmradmodel <- svm(x=vtrdata[,2:length(vtrdata[1,])],y=vtrdata[,1], kernel = "radial", gamma = 0.09, cost = 1, probability=FALSE)
summary(svmradmodel)
predictionrad <- predict(svmradmodel, vtedata[,2:length(vtedata[1,])])
tabrad <- table(pred = predictionrad, true = vtedata[,1]); tabrad

print("----------------------------------KNN-----------------------------------------")
data<-data.frame(sms=vtrdata[,2:length(vtrdata[1,])],type=vtrdata[,1])
classifier <- IBk(data, control = Weka_control(K = 20, X = TRUE))
summary(classifier)
evaluate_Weka_classifier(classifier, newdata = data.frame(sms=vtedata[,2:length(vtedata[1,])],type=vtedata[,1]))

print("---------------------------------Adaboost-------------------------------------")
adaptiveboost<-ada(x=vtrdata[,2:length(vtrdata[1,])],y=vtrdata[,1],test.x=vtedata[,2:length(vtedata[1,])], test.y=vtedata[,1], loss="logistic", type="gentle", iter=100)
summary(adaptiveboost)
varplot(adaptiveboost)

vtrdata.f <- as.data.frame(vtrdata)
vtedata.f <- as.data.frame(vtedata)

vtrdata.f$V1 <- as.factor(vtrdata.f$V1)
vtedata.f$V1 <- as.factor(vtedata.f$V1)

baseTree1 <- rpartXse(V1~.,vtrdata.f,se=0.2)
confusion(predict(baseTree1,vtrdata.f[,2:7],type='class'),vtrdata.f$V1)

