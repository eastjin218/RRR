require(graphics)
## 100 -> 70 : 30 / 80 : 20
# 70 -> 학습/ 30-> 검증(테스트) 
# 4 : 3 : 3 (학습/검증/테스트)

m1<-c()
m2<-c()

for(i in 1:10){
sam<-sample(1:nrow(mtcars),nrow(mtcars)*0.7)


fit2<-lm(mpg~.,data=mtcars[sam,])
# mean((fit2$residuals^2)) # MSE

pred<-predict(fit2,mtcars[-sam,])
m1[i]<-mean((pred-mtcars[-sam,1])^2)

##회귀 분석의 경우 모델의 성능지표 MSE / MAPE / MAE
index<-abs(fit2$coefficients)[-1] >0.5
var<-names(index)[index==T]
fo<-paste0("mpg~",paste(var,collapse = "+"))
fit3<-lm(fo,data=mtcars[sam,])
pred2<-predict(fit3,mtcars[-sam,])
m2[i]<-mean((pred2-mtcars[-sam,1])^2)
cat("\n",i)
ts.plot(cbind(m1,m2),col=c("red","blue"))

}

mean(m1)
mean(m2)

## 다중 선형 회귀분석 
## (변수가 여러개, x와 y가 선형관계가 있다 라는 가정)
ts.plot(fit3$residuals)
