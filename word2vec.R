

# devtools::install_github("bmschmidt/wordVectors")
library(wordVectors)



setwd("D:\\인프런\\R로하는ds")

load("final_score.RData")
load("final_text.RData")
head(final_text)


comments<-gsub("<.*?>","",final_text)
comments<-gsub("\t","",comments)
comments<-gsub("[][!#$%*,:;<=>@_`|‘~{}&★☆ㅋㅎ《》◈△▲▽▼○●◎◇◆□◁◀▷▶♤♠♡♥♧♣◉◈▣◐◑♨☏☎☜☞↖↘♭♩♪♬㈜]", " ",comments)
comments<-gsub("rdquo|gt|lt|nbsp|amp|quot|apos","",comments)

comments<-gsub("  "," ",comments)

comments<-gsub("\\^"," ",comments)

comments<-gsub("ㅠ|ㅜ|ㅡ"," ",comments)

comments<-gsub("\"|\n|+","",comments)

comments<-gsub("\\+","",comments)

comments<-gsub("/|!|\\*|\\+|\\@"," ",comments)

comments<-gsub("'","",comments)

comments<-gsub("\"","",comments)

comments<-gsub("\"","",comments)

comments<-gsub("=","",comments)

comments<-gsub("~|;|<|>","",comments)

comments<-gsub("\\?","",comments)

comments<-gsub("\\[.*?\\]","",comments)

comments<-gsub("\\(.*?\\)","",comments)

comments<-gsub("\\(|\\)"," ",comments)

comments<-gsub("\\]|\\[|\\(|\\)|:|-|\\,|\\."," ",comments)

comments<-gsub("\\!","",comments)

comments<-gsub("”|“|’|·|…","",comments)

library(rvest)
comments<-repair_encoding(comments)

# str_detect(commnets,"이병헌 하정우 연기가")
# comments[1000:1100]
com<-paste(comments[-1523],collapse=" ")

write.csv(com,"pre_word2vec2.csv",row.names=F)

w2v_model <- train_word2vec('pre_word2vec2.csv', 'movie.bin',
                            vectors=30, threads=4, window=7, iter=1, negative_samples=4, force=TRUE, min_count = 15)
recomment<-comments[-1523]
length(recomment)
rescore<-final_score[-1523]
save(recomment,file="recomment.RData")
save(rescore,file="rescore.RData")

load("rescore.RData")

# 다음 번에 다시 불러오려면
# w2v_model <- read.vectors('nate.bin')
## cbow ->문맥을 가지고 word를 학습시킨다
#  skip gram -> word를 가지고 문맥ㄷ을 학습시킨다.
# w2v_model@.Data


dim(w2v_model@.Data)
w2v<-w2v_model@.Data
w2v[4,]
index<-which(rownames(w2v) %in% "이병헌")
w2v[4,]



w2v_model[["이병헌"]]

1-nearest_to(w2v_model,w2v_model[["이병헌"]])
1-nearest_to(w2v_model,w2v_model[["수지"]])
1-nearest_to(w2v_model,w2v_model[["하정우"]])

w2v_model[[c("이병헌","하정우"), average=T]]

## sentence vec


library(stringr)
rec<-str_split(recomment," ")
i<-1

review<-list()
for(i in 1:length(rec)){
  review[[i]]<-  as.vector(w2v_model[[rec[[i]] , average=T]])
  cat("\n",i)
}


review2<-do.call("rbind",review)
dim(review2)

save(review2,file="review2vec.RData")
load("review2vec.RData")

mat<-review2

score2<-ifelse(rescore > 5,1,0)


mat2<-mat[!is.na(mat[,1]),]
score3<-score2[!is.na(mat[,1])]
nrow(mat2)


sam<-sample(1:nrow(mat2),nrow(mat2)*0.7)
train<-mat2[sam,]
valid<-mat2[-sam,]
tr_y<-score3[sam]
valid_y<-score3[-sam]

# lasso(numeric method) ->회귀계수 0 / ridge(analytic) -> 회귀계수를 0에 가깝게만
# Shriankage regression /회귀 계수 축소법 
# labmda의 크기에 따라 회귀계수가 0으로 가는 비율이 결정됨. 
# glmnet
# alpha = 0 lasso / alpha = 1 ridge / 0 < alpha <1 elastic net 
library(glmnet)
##전체결과

ss<-sample(1:nrow(train),nrow(train),replace = F)
vv<-sample(1:ncol(train),ncol(train)*1)


fit1 = glmnet(as.matrix(train[ss,vv]), tr_y[ss],family="binomial",alpha=1,nlambda=500)
s1<-fit1$lambda[length(fit1$lambda)]
cos<-coef(fit1, s = s1) # extract coefficients at a single value of lambda
sort(cos[,1],decreasing = T)[1:30]
sort(cos[,1],decreasing = F)[1:30]

pr<-predict(fit1,as.matrix(valid[,vv]),s=s1)


pred<-ifelse(pr>0,1,0)

sum(pred==valid_y)/length(valid_y)
#0.7930155
# 0.7752346

## 73% 100
## 76.5% 200
## 78% 300
# 79.8 500
# 80.4 1000

fit2 = glmnet(as.matrix(train[ss,vv]), tr_y[ss],family="binomial",alpha=0,nlambda=500)
s1<-fit2$lambda[length(fit2$lambda)]
cos<-coef(fit1, s = s1) # extract coefficients at a single value of lambda
sort(cos[,1],decreasing = T)[1:30]
sort(cos[,1],decreasing = F)[1:30]
pr<-predict(fit2,as.matrix(valid[,vv]),s=s1)


pred<-ifelse(pr>0,1,0)
sum(pred==valid_y)/length(valid_y) #0.7941317
# 0.7762226



pr1<-0
pr2<-0
acc_list<-NULL
for(i in 1:30){
  
  ss<-sample(1:nrow(train),nrow(train),replace = T)
  vv<-sample(1:ncol(train),ncol(train)*0.5)
  
  
  fit1 = glmnet(as.matrix(train[ss,vv]), tr_y[ss],family="binomial",alpha=1,nlambda=50)
  s1<-fit1$lambda[length(fit1$lambda)]
  cos<-coef(fit1, s = s1) # 
  sort(cos[,1],decreasing = T)[1:30]
  sort(cos[,1],decreasing = F)[1:30]
  
  pr<-predict(fit1,as.matrix(valid[,vv]),s=s1)
  pr1<-pr1+pr
  
  pred<-ifelse(pr1/i>0,1,0)
  
  acc1<-sum(pred==valid_y)/length(valid_y) #0.7930155
  # 0.7752346

  fit2 = glmnet(as.matrix(train[ss,vv]), tr_y[ss],family="binomial",alpha=0,nlambda=50)
  s1<-fit2$lambda[length(fit2$lambda)]
  cos<-coef(fit1, s = s1) # extract coefficients at a single value of lambda
  sort(cos[,1],decreasing = T)[1:30]
  sort(cos[,1],decreasing = F)[1:30]
  pr<-predict(fit2,as.matrix(valid[,vv]),s=s1)
  pr2<-pr2+pr
  
  pred<-ifelse(pr2/i>0,1,0)
  acc2<-sum(pred==valid_y)/length(valid_y) #0.7941317
  # 0.7762226
  acc_list<-rbind(acc_list,c(acc1,acc2))
  ts.plot(acc_list,col=c("red","blue"))
  cat("\n",i)
  #73%   100 

  
}



library(randomForest)

## Classification:

train<-mat2[sam,]
valid<-mat2[-sam,]
tr_y<-score3[sam]
valid_y<-score3[-sam]

data1<-cbind(train,tr_y)

fit_rf <- randomForest(tr_y ~ ., data=data1, importance=TRUE,do.trace=T,ntree=300,  
                       proximity=TRUE)
print(fit_rf)
## Look at variable importance:
round(importance(fit_rf), 2)

pr<-predict(fit_rf, valid)
head(pr)
hist(pr)
pred<-ifelse(pr> 0.5,1,0)
sum(pred==valid_y)/length(valid_y) #0.8019772





## boosting
## ensemble - bagging/randomforest/boosting
## adaboost / gradient boosting(xgboost, lightgbm, catboost, ngboost)
## model1(Y를 예측) -> error1
## model2(erro1를 예측) -> error2
## model3(erro2를 예측) -> 


pr1<-0
pr2<-0
acc_list<-c()
tr_acc_list<-c()
data_p<-rep(1/nrow(train),nrow(train))

i<-1
for(i in 1:30){
  
  ss<-sample(1:nrow(train),nrow(train),replace = T,prob = data_p)
  vv<-sample(1:ncol(train),ncol(train)*0.5)
  
  
  fit1 = glmnet(as.matrix(train[ss,vv]), tr_y[ss],family="binomial",alpha=1,nlambda=50)
  s1<-fit1$lambda[length(fit1$lambda)]
  cos<-coef(fit1, s = s1) # 
  sort(cos[,1],decreasing = T)[1:30]
  sort(cos[,1],decreasing = F)[1:30]
  
  pr<-predict(fit1,as.matrix(valid[,vv]),s=s1)
  pr1<-pr1+pr
  
  pred<-ifelse(pr1/i>0,1,0)
  acc<-sum(pred==valid_y)/length(valid_y) #0.7930155
  acc_list[i]<-acc
  #73%   100 
  pr<-predict(fit1,as.matrix(train[,vv]),s=s1)
  pred<-ifelse(pr>0,1,0)
  
  tr_ac<-sum(pred==tr_y)/length(pred)
  data_p<-rep(1/nrow(train),nrow(train))
  data_p[pred!=tr_y]<-data_p[pred!=tr_y]*1.1
  
  tr_acc_list[i]<-tr_ac
  ts.plot(cbind(acc_list,tr_acc_list),col=c("red","blue"))
  
}



library(xgboost)

# data(agaricus.train, package='xgboost')
# data(agaricus.test, package='xgboost')
# train <- agaricus.train
# test <- agaricus.test
# dim(train)
# head(train)

train<-mat2[sam,]
valid<-mat2[-sam,]
tr_y<-score3[sam]
valid_y<-score3[-sam]
class(train)

dtrain <- xgb.DMatrix(data = train, label = tr_y)
bstSparse <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, 
                     nrounds = 20, objective = "binary:logistic")

# hist(predict(bstSparse,valid))

pr<-predict(bstSparse,valid)
pred<-ifelse(pr>0.5,1,0)
sum(pred==valid_y)/length(valid_y) # 79.3
xgb.importance(model=bstSparse)




## ensemble의 ensemble

acc_list<-c()
for(i in 1:30){
  
ss<-sample(1:nrow(train),nrow(train),replace = T)
vv<-sample(1:ncol(train),ncol(train)*0.8)

dtrain <- xgb.DMatrix(data = train[ss,vv], label = tr_y[ss])
bstSparse <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, 
                     nrounds = 20, objective = "binary:logistic")

pr<-predict(bstSparse,valid[,vv])
pred<-ifelse(pr>0.5,1,0)
acc<-sum(pred==valid_y)/length(valid_y) # 79.3
acc_list[i]<-acc
ts.plot(acc_list)

}

max(acc_list)
