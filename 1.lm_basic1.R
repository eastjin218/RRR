#? lm
require(graphics)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)


lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

anova(lm.D9)
summary(lm.D90)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)

y<-runif(100)
x1<-runif(100)
x2<-runif(100)
x3<-runif(100)
x4<-runif(100)
x5<-runif(100)
x6<-runif(100)


data<-cbind(y,x1,x2,x3,x4,x5,x6)
data2<-data.frame(data)
head(data2)
fit<-lm(y~., data=data2)
anova(fit)
summary(fit)

head(data2)
library(stringr)

co<-colnames(data2)[str_detect(colnames(data2),"\\d")]
index<-as.numeric(str_sub(co,2)) %% 2 ==0
index2<-c(T,index)

data3<-data2[,index2]
head(data3)
index
fit<-lm(y~., data=data3)


"y~x2+x4+x6"
col<-colnames(data2)[index2]
fo<-paste0(col[1],"~",paste(col[2:length(col)],collapse = "+"))
fit<-lm(as.formula(fo),data=data3)
summary(fit)
anova(fit)

