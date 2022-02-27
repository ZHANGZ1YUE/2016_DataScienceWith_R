library(ISLR)
#Applied
#8.
College
fix(College)
College[,1]

summary(College)
matrix<- College[,1:10]
pairs(matrix)

boxplot(Outstate ~ Private, data = College)

rownames(College)
Elite=rep("No",nrow(College))
Elite[College$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(College ,Elite)

summary(college)
boxplot(Outstate ~ Elite, data = college)
hist(data = college, college$Enroll)

#9.
ISLR::Auto
str(ISLR::Auto)
#Name is not quantitative
summary(ISLR::Auto[, -c(9)])
sapply(ISLR::Auto[, -c(9)], mean)
sapply(ISLR::Auto[, -c(9)], sd)

auto<- ISLR::Auto[-c(10:85), -c(9)]
sapply(auto, mean)
sapply(auto, sd)
pairs(ISLR::Auto)


#10.
library(MASS)
?Boston
View(Boston)
plot(Boston$zn, Boston$crim, date = Boston)
plot(Boston$age, Boston$crim, date = Boston)
plot(Boston$dis, Boston$crim, date = Boston)

pairs(Boston)
hist(Boston$crim)
hist(Boston$tax)
hist(Boston$ptratio)

median(Boston$ptratio)

min(Boston$medv)

Boston$chas == 1


#Apllied 3.6
library(MASS)
library(ISLR)
names(Boston)

lm.fit = lm(medv~lstat, data = Boston)
attach(Boston)
lm.fit = lm(medv~lstat)

lm.fit

summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat = c(5,10,15)),
        interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)),
        interval = "prediction")

plot(lstat, medv)
abline(lm.fit)

abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2, 2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit=lm(medv ~ lstat+age,data=Boston) 
summary(lm.fit)

lm.fit=lm(medv ~ .,data=Boston) 
summary(lm.fit)

library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston) 
summary(lm.fit1)

lm.fit1=update(lm.fit, ~.-age)
summary(lm(medv~lstat*age,data=Boston))

lm.fit2=lm(medv~lstat+I(lstat^2)) 
summary(lm.fit2)

lm.fit=lm(medv~lstat) 
anova(lm.fit ,lm.fit2)




lm.fit = lm(balance~income*student, data = Default)
summary(lm.fit)

plot(x = Default$income, y = Default$balance,
     col = Default$student)

abline(a = lm.fit$coefficients[1],
       b = lm.fit$coefficients[2],
       col = "black")
abline(a = lm.fit$coefficients[1]+lm.fit$coefficients[3],
       b = lm.fit$coefficients[2]+lm.fit$coefficients[4],
       col = "red")

#3.7 applied
##8.
library(ISLR)
lm.fit <- lm(mpg~horsepower, data = Auto)
summary(lm.fit)

#i. P value indicates that they have relationship
#ii. R^2 is 60%
#iii. negative
#iv. 
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")
predict(lm.fit, data.frame(horsepower = 98), interva = "prediction")


plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(lm.fit, col = "red")

par(mfrow = c(2, 2))
plot(lm.fit)

##9.
#scatter plot matrix
pairs(Auto)
cor(Auto[1:8])
fit2 <- lm(mpg ~ . - name, data = Auto)
summary(fit2)
par(mfrow = c(2, 2))
plot(fit2)
fit3 <- lm(mpg ~ cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(fit3)
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)

##10.
fit3 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit3)
#Only associate with outcome
fit4 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit4)
confint(fit4)
#outliers
par(mfrow = c(2, 2))
plot(fit4)

##11.
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
fit5<- lm(y ~ x+0)
summary(fit5)
fit6 <- lm(x ~ y + 0)
summary(fit6)
cor(Boston[-c(1, 4)])
lm.fit <- lm(mpg~horsepower, data = Auto)
plot(lm.fit)





library(dplyr)
sample(1:5)
sample(1:100, replace = T) %>%
  unique() %>%
  length()






####Caret package for classification (resample with caretis in another file "caret - linear model.R")
library(caret)
library(ISLR)

df <- iris %>% filter(Species != "virginica") %>% mutate(Species = factor(Species))


dim(df)
names(df)

df$Species

set.seed(107)
inTrain <- createDataPartition(df$Species, 
                               p =0.8, 
                               list =F)
training <- df[ inTrain,]
testing <- df[-inTrain,]
nrow(training)
nrow(testing)

sum(complete.cases(training))

fitControl <- trainControl(method = "cv", 
                           number = 10)

fit <- train(Species ~ ., 
             data = training, 
             method = "glm", 
             trControl = fitControl)

fit
fit$resample
fit$results

confusionMatrix(fit)

############### ROC curve
out <- predict(object = fit, newdata = training)
out

trainPred <- predict(object = fit, newdata = training, 
                     type = "prob")
library(dplyr)
trainPred %>% head()


library(pROC)
rocCurve   <- roc(response = training$Species,
                  predictor = trainPred[, "setosa"])
plot(rocCurve)
plot(iris)


############################################
##next day

fitControl <- trainControl(method = "cv", 
                           number = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

fit <- train(Purchase ~ ., 
             data = training, 
             method = "glm", 
             trControl = fitControl,
             ## Specify which metric to optimize
             metric = "ROC")
fit
fit$results


#######tm package extracting datasets from sets of documents
library(readr)
library(caret)
library(dplyr)
library(tm)


data("crude")
## Document access triggers the stemming function
## (i.e., all other documents are not stemmed yet)
tm_map(crude, stemDocument, lazy = TRUE)[[1]]
## Use wrapper to apply character processing function
tm_map(crude, content_transformer(tolower))
## Generate a custom transformation function which takes the heading as new content
headings <- function(x)
  PlainTextDocument(meta(x, "heading"),
                    id = meta(x, "id"),
                    language = meta(x, "language"))
inspect(tm_map(crude, headings))
inspect(crude[[1]])  ##display the content
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   



docs <- c("This is a text.", "This another one.")
VCorpus(VectorSource(docs))

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                     readerControl = list(reader = readReut21578XMLasPlain))
inspect(reuters[[1]])

reuters <- tm_map(reuters, stripWhitespace)
reuters <- tm_map(reuters, removeWords, stopwords("english"))
tm_map(reuters, stemDocument)
dtm <- DocumentTermMatrix(reuters)

# remove sparse terms to simplify the cluster plot
# Note: tweak the sparse parameter to determine the number of words.
# About 10-30 words is good.
mydata.dtm2 <- removeSparseTerms(dtm, sparse=0.8)

# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(as.matrix(mydata.dtm2))



#############
#############
#Chapter 6

#Example of nonsense model: P = N
library(tidyverse)
set.seed(123)
dt<- tibble(x1 = runif(5),
            x2 = runif(5),
            x3 = runif(5),
            x4 = runif(5),
            x5 = runif(5),
            x6 = runif(5),
            y = runif(5)
            )
summary(lm(y ~ . , data = dt))

### N > P
set.seed(123)
n = 100
dt <- tibble(
  x1 = runif(n),
  x2 = runif(n),
  x3 = runif(n),
  x4 = runif(n),
  x5 = runif(n),
  y = runif(n)
)
summary(lm(y ~ . , data = dt))


##Lab1 Subset selection method
library(ISLR)
names(Hitters)
df<-na.omit(Hitters)
library(leaps)
regfit.full<-regsubsets(Salary~.,df)
summary(regfit.full)

regfit.full<-regsubsets(Salary~.,data = df,nvmax=19)
reg.summary<-summary(regfit.full)
names(reg.summary)

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=10)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

coef(regfit.full,6)

###Forward and backward stepwise selection
regfit.fwd=regsubsets(Salary~.,data=df,nvmax=19,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)


#choosing among models using the validation set approach and cross validation
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(df),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=df[train,],nvmax=19)

test.mat=model.matrix(Salary~.,data=df[test,])

val.errors=rep(NA,19)

for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((df$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
}

regfit.best=regsubsets(Salary~.,data=df,nvmax=19)
coef(regfit.best,10)

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=df[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,df[folds == j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b")

reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)



##Ridge and lasso
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


#Lab3 

library(pls)
library(ISLR)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

Hitters=na.omit(Hitters)

x=model.matrix(Salary~.,Hitters)[,-1] 
y=Hitters$Salary

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

set.seed(1)

pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)




##PCA
goo.gl/VERw0N


##6.8.6.b
b = seq(-10, 10, by = 0.1)
f = 1 - 2*b +b^2 +abs(b)
qplot(x = b, y = f)
ggplotly()





#7.8 Non-linear model
library(ISLR)
attach(Wage)

fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

fit2=lm(wage~poly(age,4,raw=T),data=Wage) #Other ways
coef(summary(fit2))

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage) #Other ways
coef(fit2a)

fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)

agelims=range(age)
agelims
age.grid=seq(from=agelims[1],to=agelims[2])
age.grid

preds=predict(fit,newdata=list(age=age.grid),se=T)

se.bands=cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")

title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

preds2=predict(fit2,newdata=list(age=age.grid),se=T)
max(abs(preds$fit-preds2$fit))


fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

coef(summary(fit.5))

fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)

fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=T)

pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))


#7.8.2 Splines
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot.new()
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se.fit,lty="dashed")
lines(age.grid,pred$fit-2*pred$se.fit,lty="dashed")
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

plot.new()
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF", "6.8 DF"), col=c("red","blue"), lty=1, lwd=2, cex=.8)

plot.new()
par(mfrow=c(1,1))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2", "Span=0.5"), col=c("red","blue"),lty=1,lwd=2,cex=.8)


#GAM
gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
plot.new()
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")
plot.gam(gam1,se=TRUE,col="red")

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3)

summary(gam.m3)

preds=predict(gam.m2,newdata=Wage)

gam.lo=gam(wage~s(year,df=4)+lo(age,span=.7)+education,data=Wage)
plot.new()
plot.gam(gam.lo,se=TRUE,col="green")

gam.lo.i=gam(wage~lo(year,age,span=.5)+education,data=Wage)
library(akima)
plot.new()
plot(gam.lo.i)

gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
plot.new()
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

table(education,I(wage>250))
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage
             ,subset = (education!="1. < HS Grad"))
plot.new()
plot(gam.lr.s,se=T,col="green")


#Tree based regression
library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales <= 8, "No", "Yes")

Carseats = data.frame(Carseats, High)
tree.carseats = tree(High~. -Sales, Carseats)
summary(tree.carseats)


plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

## 8.3.2 Fittingregr regression tree
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~.,Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)




## caret package tree
library(caret)
library(rattle)
data(iris)
formula <- as.formula(Species ~.)
t <- train(formula,iris,method = "rpart",cp=0.002,maxdepth=8)
plot(t)
plot(t$finalModel)
text(t$finalModel)




library(tree)

iris<- na.omit(iris)

split <- createDataPartition(y=iris$Species, p=0.5, list=FALSE)

train <- iris[split,]
test <- iris[-split,]


trees <- tree(Species~., train)
plot(trees)
text(trees, pretty=0)



##Random forest
library(caret)
library(MASS)
attach(Boston)
set.seed(123)
split <- createDataPartition(y=Boston$medv, p = 0.7, list=FALSE)
train <- Boston[split,]
test<- Boston[-split,]

rf.caret <- train(medv ~., train,
                  method='rf',
                  trControl = trainControl(method = "oob", verboseIter = TRUE),
                  importance=TRUE)


rf.caret

bag.caret <- train(medv ~., train,
                  preProc=c('center', 'scale'),
                  method='treebag',
                  importance=TRUE,
                  verbose = TRUE)
bag.caret

###svm

# Training SVM Models
library(caret)
library(dplyr)   
library(kernlab)      
library(pROC)	      

data(segmentationData)  	# Load the segmentation data set
trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
trainData <- segmentationData[trainIndex,]
testData  <- segmentationData[-trainIndex,]
trainX <-trainData[,4:61]        # Pull out the variables for training
sapply(trainX,summary)           # Look at a summary of the training data


set.seed(1)
# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=2,		    # do 2 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)


svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 9,					# 9 values of the cost function
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)

plot(svm.tune)
