rm(list=ls())

###########백두산 movie review

library(stringr)

score_list<-list()
text_list<-list()
for(i in 1:2500){
    url<-paste0("https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=187940&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=",i)

    b<-readLines(url,encoding="UTF-8")

    b2<-b[which(str_detect(b,"star_score"))+2]

    score<-as.numeric(str_sub(str_extract(b2,("(?<=<em>).*(</em>)")),end=-6))

    score_list[[i]]<-score
    text<-b[which(str_detect(b,"<span id=\"_filtered_ment_"))+4]

    text2<-gsub("\t","",text)
    text2[str_detect(text2,"javascript")]<-str_sub(text2[str_detect(text2,"javascript")],42,end=-36)
    text_list[[i]]<-text2
    cat("\n",i)

    if(i %% 500 == 0){
      cat("\n save ",i)
        final_score<-unlist(score_list)
        final_text<-unlist(text_list)
        
        setwd("./")
        save(final_score,file="final_score.RData")
        save(final_text,file="final_text.RData")
    }
}

final_score<-unlist(score_list)
final_text<-unlist(text_list)

setwd("./")
save(final_score,file="./movie/final_score.RData")
save(final_text,file="./movie/final_text.RData")

load("./movie/final_score.RData")
load("./movie/final_text.RData")
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
comments<-gsub("”|“|’|·","",comments)

head(comments)

# dtm 
library(stringr)

co<-str_split(comments," ")
head(co)
co2<-table(unlist(co))
length(co2)
wo<-names(sort(co2,decreasing = T)[2:301])
wo<-wo[!str_detect(wo,"\\d")]

i<-1

mat<-matrix(0,ncol=length(wo),nrow=length(co))

for(i in 1:length(co)){
  mat[i,wo %in%  co[[i]]] <-1
  cat("\n",i)
}
colnames(mat)<-wo

mat<-data.frame(mat)

mat2<-mat[apply(mat,1,sum) >0,]

score2<-final_score[apply(mat,1,sum) >0]

table(score2)
hist(score2)

score3<-ifelse(score2 > 5,1,0)

dim(mat2)
length(score3)


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

pr1<-0
pr2<-0
acc_list<-NULL
for(i in 1:10){

ss<-sample(1:nrow(train),nrow(train),replace = F)
vv<-sample(1:ncol(train),ncol(train)*1)

  
fit1 = glmnet(as.matrix(train[ss,vv]), tr_y[ss],family="binomial",alpha=1,nlambda=50)
s1<-fit1$lambda[length(fit1$lambda)]
cos<-coef(fit1, s = s1) # extract coefficients at a single value of lambda
sort(cos[,1],decreasing = T)[1:30]
sort(cos[,1],decreasing = F)[1:30]

pr<-predict(fit1,as.matrix(valid[,vv]),s=s1)
pr1<-pr1+pr

pred<-ifelse(pr1/i>0,1,0)

acc1<-sum(pred==valid_y)/length(valid_y) #0.7930155
# 0.7752346

## 73% 100
## 76.5% 200
## 78% 300
# 79.8 500
# 80.4 1000

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
# 76.6% 200
# 77.9% 300
# 79.9% 500
# 80.8% 1000    

}



###Randomforest

library(randomForest)

## Classification:
data(iris)
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
sum(pred==valid_y)/length(valid_y) #0.7595916



