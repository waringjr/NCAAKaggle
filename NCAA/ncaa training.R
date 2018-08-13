#Required Packages:
install.packages('caret',dependencies = TRUE)
library(caret)
install.packages('glmnet')
library(glmnet)
library(Matrix)
install.packages('Matrix')



setwd("C:/Users/Jeffrey.Waring/Desktop/Kaggle/NCAA")

df<-read.csv("2017reference2.3.csv")
#data from: https://www.sports-reference.com/cbb/

df_results<-read.csv("RegularSeasonCompactResults.csv")

res17 <- df_results[(df_results$Season==2017),]


#so flip through this
t<-df[res17[1,"WTeamID"],"School.Totals.2"]


#create training set
NUMROWS = 255
DATACOL = 7

#total number features PER team
NUMFEATURES = ncol(df)-DATACOL+1

#total number columns in training set
NUMCOLS = 2*NUMFEATURES

training<-data.frame(matrix(0,nrow=NUMROWS,ncol=NUMCOLS))

for(i in 1:NUMROWS-1)
{
  
  training[i,1:NUMFEATURES]<-df[which(df$id==res17[i,"WTeamID"]),DATACOL:ncol(df)]
  #training[i,NUMFEATURES+1:NUMCOLS]<-df[which(df$id==res17[i,"LTeamID"]),DATACOL:ncol(df)]
  #training[i,29:56]<-df[which(df$id==res17[i,"LTeamID"]),DATACOL:ncol(df)]
  training[i,25:48]<-df[which(df$id==res17[i,"LTeamID"]),DATACOL:ncol(df)]
}
#add results
training[1:NUMROWS-1,ncol(training)+1] <- res17[1:NUMROWS-1,"WScore"]-res17[1:NUMROWS-1,"LScore"]


trainModel <- function(model,df){
  #the purpose of this function is to create a learning curve
  #it iterates over the data entries of len 
  #I will work with subsets of the training set:
  #len <- c(10,30,99,199)
  len<-199
  #when grabbing final fit, just use len = 4000
  #len <- c(4000)
  error<-rep(0,length(len))
  
  CV_error<-rep(0,length(len))
  
  for(i in 1:length(len)){
    
    #loop over various data set sizes
    #first create a training, testing, and cross validation set from available data
    
    df_new <- df[1:len[i],]
    
    inTraining <- createDataPartition(df_new[,1], p = .6, list = FALSE)
    training <- df_new[ inTraining,1:(ncol(df_new)-1)]
    
    temp <- df_new[-inTraining,1:(ncol(df_new))]
    
    CVsplit <- createDataPartition(temp[,ncol(df_new)],p=0.5,list= FALSE)
    
    testing  <- temp[-CVsplit,1:(ncol(df_new)-1)]
    CV <- temp[CVsplit,1:(ncol(df_new)-1)]
    
    outcomes <- df_new[inTraining,ncol(df_new)]
    testOutcomes <-temp[-CVsplit,ncol(df_new)]
    crossValOut <- temp[CVsplit,ncol(df_new)]
    
    train_control <-trainControl(method="cv",number=10)
    #Regular regression
    #x_new <-preProcess(x=training,method="BoxCox")
    fit <- train(x=training,y=outcomes,method=model,trControl = train_control)
    
    p<-predict(fit,testing)
    
    CVp<-predict(fit,CV)
    
    #compare these predictions against
    error[i]<-RMSE(p,testOutcomes)
    CV_error[i]<-RMSE(p,crossValOut)
    print(summary(fit))
    
  }
  #return(list(error,CV_error))
  return(fit)
}

error<-trainModel("lm",training)[[1]]
CV_error<-trainModel("lm",training)[[2]]

###Now to make the a priori predictions
corpus<-read.csv("teamsToPredict.csv")

#for whatever reason, I can't populate a dataframe > 4587 entries, so breaking
#the prediction set into 3

corpus1<-corpus[1:4500,]
corpus2<-corpus[4501:9000,]
corpus3<-corpus[9001:nrow(corpus),]

corpus_features1<-data.frame(matrix(0,nrow=nrow(corpus1),ncol=48))
corpus_features2<-data.frame(matrix(0,nrow=nrow(corpus2),ncol=48))
corpus_features3<-data.frame(matrix(0,nrow=nrow(corpus3),ncol=48))

for(i in 1:nrow(corpus1))
{

  corpus_features1[i,1:24]<-df[which(df$id==corpus1[i,"team1"]),7:30]
  corpus_features1[i,25:48]<-df[which(df$id==corpus1[i,"team2"]),7:30]
}
for(i in 1:nrow(corpus2))
{

    corpus_features2[i,1:24]<-df[which(df$id==corpus2[i,"team1"]),7:30]
    corpus_features2[i,25:48]<-df[which(df$id==corpus2[i,"team2"]),7:30]
}
for(i in 1:nrow(corpus3))
{
  
  corpus_features3[i,1:24]<-df[which(df$id==corpus3[i,"team1"]),7:30]
  corpus_features3[i,25:48]<-df[which(df$id==corpus3[i,"team2"]),7:30]
}

fit <- trainModel("lm",training)


p1<-predict(fit,corpus_features1)
p2<-predict(fit,corpus_features2)
p3<-predict(fit,corpus_features3)

name<-paste(corpus$team1,corpus$team2,sep="_")
name2<-paste(corpus$year,name,sep="_")

answer<-data.frame(matrix(0,nrow=nrow(corpus),ncol=2))
answer[,1]<-name2
answer[1:4500,2]<-p1
answer[4501:9000,2]<-p2
answer[9001:nrow(corpus),2]<-p3

answer[which(answer[,2]<(-50)),2]<--50

#there are an extreme outlier, set it to -50

#finally, converet the predictions (currently point spreads), into probabilities
#shift such that biggest loss has prob = 0, and most probable = 1
shift = abs(min(answer[,2]))
max = max(answer[,2]+shift)
answer[,2]<-(answer[,2]+shift)/max

#Write answer to part 1
write.csv(answer,"answer.csv")

#Now part 2

df2<-read.csv("2018reference3.csv")
#data from: https://www.sports-reference.com/cbb/ now for the current year

corpus2018<-read.csv("teamsToPredict2.csv")
corpus_features2018<-data.frame(matrix(0,nrow=nrow(corpus2018),ncol=48))

for(i in 1:nrow(corpus2018))
{
  
  corpus_features2018[i,1:24]<-df2[which(df2$id==corpus2018[i,"team1"]),7:30]
  corpus_features2018[i,25:48]<-df2[which(df2$id==corpus2018[i,"team2"]),7:30]
}

p2018<-predict(fit,corpus_features2018)

name2018<-paste(corpus2018$team1,corpus2018$team2,sep="_")
name20182<-paste(corpus2018$year,name2018,sep="_")

answer2018<-data.frame(matrix(0,nrow=nrow(corpus2018),ncol=2))
answer2018[,1]<-name20182
answer2018[,2]<-p2018

shift = abs(min(answer2018[,2]))
max = max(answer2018[,2]+shift)
answer2018[,2]<-(answer2018[,2]+shift)/max

write.csv(answer2018,"answer5.csv")
