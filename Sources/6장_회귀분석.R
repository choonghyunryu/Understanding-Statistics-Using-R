#################################
# 예제 6-1
#################################
dim(cars)
names(cars)
plot(cars, xlab="Speed (mph)", ylab="Stopping distance (ft)",
  pch=16, main="Scatter Plot")


#################################
# 예제 6-2
#################################
dim(iris)
names(iris)

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

library(lattice)
splom(~iris[1:4]|Species, data=iris, 
  layout=c(2,2), pscales=0, 
  varnames=c("Sepal\nLength", "Sepal\nWidth", 
    "Petal\nLength", "Petal\nWidth"))




#################################
# 예제 6-3
#################################
variable <- names(iris)[1:4]
(sets <- t(combn(variable, 2)))

# (1) 상관계수 사용자 정의 함수
corr <- function (set) {
  x <- iris[, set[1]]
  y <- iris[, set[2]]
  n <- length(iris[, set[1]])

  (n*sum(x*y)-sum(x)*sum(y))/
    (sqrt(n*sum(x^2)-sum(x)^2)*sqrt(n*sum(y^2)-sum(y)^2))
}
data.frame(relation=apply(sets, 1, paste, collapse=" & "),
   correlation=apply(sets, 1, corr))

# (2) cor 함수를 이용한 상관계수 구하기
apply(sets, 1, function(x) cor(iris[, x[1]], iris[, x[2]]))

# (3) Species별로 상관계수 구하기
cor(iris[iris$Species=="setosa", 1:4])
cor(iris[iris$Species=="versicolor", 1:4])
cor(iris[iris$Species=="virginica", 1:4]) 



#################################
# 그림 6-4
#################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(4, 4, 4, 1))

# (1)
x <- 1:10
y <- x
plot(x, y, xlim=c(-10,20), ylim=c(-10,30), pch=16)
rho <- cor(x, y)
title(substitute(list((a),r[xy]==b), list(a=1, b=rho)))

# (2)
x <- 1:10
y <- 10:1
plot(x, y, xlim=c(-10,20), ylim=c(-10,30), pch=16)
rho <- cor(x, y)
title(substitute(list((a),r[xy]==b), list(a=2, b=rho)))

# (3)
set.seed(1)
x <- jitter(1:10, amount=10)
set.seed(2)
y <- jitter(1:10, amount=10)
plot(x, y, xlim=c(-10,20), ylim=c(-10,30), pch=16)
rho <- cor(x, y)
title(substitute(list((a),r[xy]==b), list(a=3, b=rho)))

# (4)
x <- -5:5
y <- x^2
plot(x, y, xlim=c(-10,20), ylim=c(-10,30), pch=16)
rho <- cor(x, y)
title(substitute(list((a),r[xy]==b), list(a=4, b=rho)))
par(op)



#################################
# 그림 6-6
#################################
x <- 1:3
y <- c(3, 5, 4)

op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2), mar=c(4, 4, 4, 1))

# (1)
f1 <- function(x) 3+1/2*x
curve(f1, xlim=c(0, 5), ylim=c(1, 7), col=4, ylab="y")
points(x, y, pch=16)
arrows(x, y, x, f1(x), length=0.1)
text(x+0.3, f1(x)+(y-f1(x))/2, y-f1(x))
(sum.r <- sum(y-f1(x)))
title(main=(substitute(sum((y[i]-hat(y[i])), 
  i==1, n)==r, list(r=sum.r))))

# (2)
f2 <- function(x) 7-3/2*x
curve(f2, xlim=c(0, 5), ylim=c(1, 7), col=4, ylab="y")
points(x, y, pch=16)
arrows(x, y, x, f2(x), length=0.1)
text(x+0.3, f2(x)+(y-f2(x))/2, y-f2(x))
(sum.r <- sum(y-f1(x)))
title(main=(substitute(sum((y[i]-hat(y[i])), i==1, 
  n)==r, list(r=sum.r))))

# (3)
curve(f1, xlim=c(0, 5), ylim=c(1, 7), col=4, ylab="y")
points(x, y, pch=16)
arrows(x, y, x, f1(x), length=0.1)
text(x+0.3, f1(x)+(y-f1(x))/2, y-f1(x))
(sum.r <- sum(abs(y-f1(x))))
title(main=(substitute(sum(group("|",y[i]-hat(y[i]),"|"), 
  i==1, n)==r, list(r=sum.r))))

# (4)
f3 <- function(x) 2.5+1/2*x
curve(f3, xlim=c(0, 5), ylim=c(1, 7), col=4, ylab="y")
points(x, y, pch=16)
arrows(x, y, x, f3(x), length=0.1)
text(x+0.3, f3(x)+(y-f3(x))/2, y-f3(x))
(sum.r <- sum(abs(y-f3(x))))
title(main=(substitute(sum(group("|",y[i]-hat(y[i]),"|"), 
  i==1, n)==r, list(r=sum.r))))

par(op)



#################################
# 예제 6-4; 그림 6-7
#################################
x <- cars$speed 
y <- cars$dist

# (1) 회귀선 추정
sxx <- sum((x-mean(x))^2)
sxy <- sum((x-mean(x))*(y-mean(y)))
(beta1 <- sxy/sxx)
(beta0 <- mean(y)-beta1*mean(x))

# (2) lm() 함수 이용
lm(y~x)

# (3) 그래프 출력
plot(cars, pch=16)
abline(lm(y~x), col="blue")



#################################
# 예제 6-5
#################################
x <- cars$speed
y <- cars$dist

lm.cars <- lm(y~x)
lm.cars$coefficients
lm.cars$coef
beta0 <- lm.cars$coef[1]
beta1 <- lm.cars$coef[2]

# y의 추정치
yhat.1 <- beta0 + beta1*x
yhat.2 <- predict.lm(lm.cars)
all(yhat.1 == yhat.2)
all.equal(yhat.1, yhat.2, check.attributes=F)

# 잔차
residual.1 <- y - yhat.1
residual.2 <- y - (beta0 + beta1*x)
residual.3 <- lm.cars$residuals
all.equal(residual.1, residual.2)
all.equal(residual.2, residual.3, check.attributes=F)

# (1) 잔차의 합
sum(residual.1)

# (2) 잔차의 x에대한 가중합
sum(x*residual.1)

# (3)
(mean.y <- mean(cars$dist))
mean(yhat.1)

# (4) 
(mean.x <- mean(cars$speed))
predict.lm(lm.cars,  data.frame(x=mean.x))
mean.y



#################################
# 예제 6-6
#################################
lm.cars <- lm(dist ~ speed, data=cars)		# lm 객체
n <- NROW(cars)					# 관측값 수
(se <- sqrt(sum(residuals(lm.cars)^2)/(n-2)))	# (1) 

SST <- sum((cars$dist - mean(cars$dist))^2)
SSR <- sum((predict.lm(lm.cars)-mean(cars$dist))^2)
SSE <- sum(residuals(lm.cars)^2)
(R.sq <- SSR/SST)				# (2-1)
1-SSE/SST					# (2-2)

df.R <- 1; df.E <- n-2				# 자유도
MSR <- SSR/df.R; MSE <- SSE/df.E		# 평균제곱
(F0 <- MSR/MSE)					# F0
pf(F0, df.R, df.E, lower.tail=F)		# (3) p-value

summary(lm.cars)				# (4) summary()



#################################
# 예제 6-7
#################################
lm.cars <- lm(dist ~ speed, data=cars)		# (1) lm 객체
resid.cars <- residuals(lm.cars)		# (2) 잔차

x <- cars$speed
X <- cbind(1, x)				# (3) Design Matrix
hii1<- diag(X%*%solve(t(X)%*%X)%*%t(X))		# (4) Hat Matrix 이용한 hii
hii2 <- 1/length(x) + (x - mean(x))^2 / 
  sum((x - mean(x))^2) 				# (5) 공식 이용한 hii
all.equal(hii1, hii2)				# (6) 두 계산의 비교	

# (7) 사용자 정의 함수 - 표준화 잔차, 외적 스튜던트화 잔차
std.resid <- function(obj, type=c("standard", "externally")) {
  type <- match.arg(type)

  rsdl <- residuals(obj)
  datas <- obj$model
  var.names <- names(datas)
  n <- NROW(datas)

  x <- obj$model[,2]
  hii <- 1/length(x) + (x - mean(x))^2 / sum((x - mean(x))^2)

  ss <- function() {
    apply(t(1:n), 2,
      function(idx) sum(lm(as.formula(paste(var.names[1], 
        var.names[2], sep="~")), data=datas[-idx,])$resid^2)/(n-3))
  }

  sigma.sq <- switch(type,
             standard = sum(rsdl^2)/(n-2),
             externally = ss())

  rsdl / sqrt(sigma.sq * (1-hii))
}

# 사용자 정의 함수 사용
my.sr.cars <- std.resid(lm.cars, "standard")	# (8) 표준화 잔차
my.esr.cars <- std.resid(lm.cars, "externally")	# (9) 외적 스튜던트화 잔차

# R 함수 사용
sr.cars <- rstandard(lm.cars)			# (10) 표준화 잔차
esr.cars <- rstudent(lm.cars)			# (11) 외적 스튜던트화 잔차

# (12) 사용자 정의함수와 결과 비교
all.equal(my.sr.cars, sr.cars)
all.equal(my.esr.cars, esr.cars)
head(resid.cars); head(sr.cars); head(esr.cars)



#################################
# 예제 6-8, 그림 6-9, 6-10, 6-11
#################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 1), mar=c(4, 4, 2, 1))
# (1) 등분산성
plot(cars$speed, resid.cars, ylab="Residuals")
abline(h=0, lty=3, col="gray")

# (2) 정규성
qqnorm(sr.cars)
qqline(sr.cars, lty=3, col="gray")
par(op)

# (3) 독립성
dw <- function(obj) {
  rsdl <- residuals(obj)
  sum(diff(rsdl)^2)/sum(rsdl^2)
}

dw(lm.cars)

# (4) R의 plot.lm() 함수를 이용한 오차항 검정
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2), oma=c(0, 0, 1, 0), mar=c(4, 4, 4, 1.5))
plot(lm.cars)
par(op)


# (5) R의 plot.lm() 함수를 이용한 오차항 검정 2
op <- par(no.readonly=TRUE)
par(mfrow=c(3, 2))
plot(lm.cars, which=1:6)
par(op)


#################################
# 그림 6-12, 6-13, 6-14 
#################################
x <- seq(-10, 10, length=100)
set.seed(1); y1 <- 10*x + rnorm(100, sd=10)
set.seed(1); y2 <- -x^2 + rnorm(100, sd=10)
set.seed(1); y3 <- x^3 + rnorm(100, sd=10)
set.seed(1); y4 <- 2*x + (x+10) * rnorm(100, sd=10)

lm.1 <- lm(y1 ~ x)
lm.2 <- lm(y2 ~ x)
lm.3 <- lm(y3 ~ x)
lm.4 <- lm(y4 ~ x)

# 산점도
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2), mar=c(4, 4, 4, 1))
plot(x, y1, pch=16, ylab="y1", xlab="x",
  main="Scatter Plot (Linear)")
abline(lm.1, col="blue")
plot(x, y2, pch=16, ylab="y2", xlab="x",
  main="Scatter Plot (Quadratic)")
abline(lm.2, col="blue")
plot(x, y3, pch=16, ylab="y3", xlab="x",
  main="Scatter Plot (Cubic)")
abline(lm.3, col="blue")
plot(x, y4, pch=16, ylab="y4", xlab="x",
  main="Scatter Plot (Unequal Variance)")
abline(lm.4, col="blue")
par(op)


# 잔차그림
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2), mar=c(4, 4, 4, 1))
plot(residuals(lm.1), pch=16, ylab="residuals", xlab="x",
  main=" Random Residuals(Linear)")
abline(h=0)
plot(residuals(lm.2), pch=16, ylab="residuals", xlab="x",
  main="Quadratic Curve Residuals")
abline(h=0)
plot(residuals(lm.3), pch=16, ylab="residuals", xlab="x",
  main="Cubic Curve Residuals")
abline(h=0)
plot(residuals(lm.4), pch=16, ylab="residuals", xlab="x",
  main="Gradually Increasing Residuals")
abline(h=0)
par(op)


# 정규확률그래프
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2), mar=c(4, 4, 4, 1))
qqnorm(residuals(lm.1)); qqline(residuals(lm.1), col = 2)
qqnorm(residuals(lm.2)); qqline(residuals(lm.2), col = 2)
qqnorm(residuals(lm.3)); qqline(residuals(lm.3), col = 2)
qqnorm(residuals(lm.4)); qqline(residuals(lm.4), col = 2)
par(op)


# Durbin-Watson 통계량
dw(lm.1)
dw(lm.2)
dw(lm.3)
dw(lm.4)

#################################
# 예제 6-9
#################################

head(stackloss)				# (1) Stack Loss Plant Data
head(stack.x)					# (2) stackloss의 독립변수 
stack.loss					# (3) stackloss의 종속변수 
X <- cbind(1, stack.x)			# (4) 독립변수 행렬 X 
(beta <- solve(t(X) %*% X) %*% t(X) %*% stack.loss)	# (5) 회귀계수 계산
lm(stack.loss ~ stack.x)			# (6) lm을 이용한 회귀계수 추정 
head(X %*% beta)				# (7) 추정 회귀값 
head(res <- stack.loss - X %*% beta)	# (8) 잔차의 계산 
sum(res)					# (9) 잔차의 합 
round(sum(res),10)				# (10) 잔차의 합2 

 


#################################
# 예제 6-9
#################################
X <- cbind(1, stack.x)				# (1) 독립변수 행렬 X 
C <- solve(t(X) %*% X)				# (2) C
beta.hat <- C %*% t(X) %*% stack.loss		# (3) 회귀계수 계산
res <- stack.loss - X %*% beta.hat 		# (4) 잔차의 계산 
(cii <- diag(C))					# (5) Cii
beta <- 0						# (6) beta
(n <- NROW(stack.x))				# (7) 자료 개수
(k <- NCOL(stack.x))				# (8) 독립변수 개수
(df.E <- n - k - 1)                			# (9) 오차의 자유도
(SSE <- sum(res^2))					# (10) SSE
(MSE <- SSE/df.E)                       		# (11) 평균제곱합
SE <- sqrt(cii*MSE)					# (12) 표준오차
T0 <- (beta.hat-beta)/SE				# (13) T_0
p.val <- 2*pt(abs(T0), df=df.E, lower.tail=F)	# (14) p-value
coefs <- cbind(beta.hat, SE, T0, p.val)		# (15) 계수들의 정보
rownames(beta.hat)[1] <- "(Intercept)"		# (16) 계수의 이름
dimnames(coefs) <- list(rownames(beta.hat), 	# (17) 행렬 차원이름 지정
  c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
coefs							# (18) 최종계수 정보

lm.fit <- lm(stack.loss ~ stack.x)			# (19) lm 함수 fit
summary(lm.fit)					# (20) lm 객체의 서머리


#################################
# 예제 6-11
#################################
variables <- function(x) {			# (1) 회귀식의 변수 산출
  combination <- "Intercept"

  for (i in 1:length(x)) {
    tmp <- apply(t(combn(x, i)), 1, function(x) paste(x, collapse=","))
    tmp <- paste(tmp, "Intercept", sep=",")
    combination <- c(combination, tmp) 
  }
  combination 
}

variables(LETTERS[1:3]) 			# (2) 3개의 변수
variables(LETTERS[1:5]) 			# (3) 5개의 변수
2^10						# (4) 10개의 변수


#################################
# 예제 6-12
#################################
model <- function(x, y) {				# (1) 모형식 만들기 함수
  formulas <- "-." 

  for (i in 1:length(x)) {
    tmp <- apply(t(combn(x, i)), 1, function(x) paste(x, collapse="+"))
    formulas <- c(formulas , tmp) 
  }
  formulas <- paste(y, formulas, sep="~")

  return(formulas)
}

variable <- c("fert","agr","exam","edu","cath","infant") # (2-1) 변수명 수정
names(swiss) <- variable			# (2-2) 변수명 수정

x <- variable[-1]					# (3) 독립변수명
y <- variable[1]					# (4) 종속변수명

models <- model(x, y) 				# (5) 모델식 만들기	

rs <- t(						# (6) 결정계수 구하기
apply(as.matrix(models), 1, 
  function(x) {
    fit <- lm(formula=x, data=swiss)
    do.call("c", summary(fit)[ c("r.squared", "adj.r.squared")])
  }
))

result <- cbind(rs, apply(-rs, 2, rank))	# (7) 결정계수와 Rank
rownames(result) <- models				# (8) 모델과 결과 묶기
colnames(result) <- c("R-Square","Adj-RSquare","RS.Rank","Adj.Rank")
result							# (9) 결과


#################################
# 예제 6-13, 그림 6-15
#################################
fits <-							# (1) lm 객체 적용하기
apply(as.matrix(models), 1, 
  function(x) {
    fit <- lm(formula=x, data=swiss)
  }
)

complete.fit <- fits[[length(fits)]]			# (2) 완전모형
(sst <- sum(anova(complete.fit)$Sum))		# (3) SST
(mse <- anova(complete.fit)$Mean[length(complete.fit$coefficients)]) # (4) MSE


fit <- fits[[26]]					# (5) 26번째 회귀식
(p <- length(fit$coefficients)-1)			# (6) 변수 개수
(n <- length(fit$fitted.values))			# (7) 데이터 개수 1
(n <- nobs(fit))					# (8) 데이터 개수 2
(sse.p <- t(residuals(fit))%*%residuals(fit))		# (9) SSEp 1
(sse.p <- anova(fit)$Sum[p+1])			# (10) SSEp 2	

(cp <- sse.p/mse - n + 2*(p+1))			# (11) Cp 공식

(aic <- n*log(sse.p/n)+2*(p+1))			# (12) AIC 공식
(aic2 <- extractAIC(fit))				# (13) AIC 함수

(bic <- n*log(sse.p/n)+(p+1)*log(n))		# (14) BIC 공식
(bic2 <- extractAIC(fit, k=log(n)))			# (15) BIC 함수

ic <- t(sapply(fits, 					# (16) Cp, AIC, BIC 구하기
  FUN=function(x) {
    n <- nobs(x)
    p <- length(x$coefficients)-1
    sse.p <- anova(x)$Sum[p+1]
    cp <- sse.p/mse - n + 2*(p+1)
    aic <- n*log(sse.p/n)+2*(p+1)
    bic <- n*log(sse.p/n)+(p+1)*log(n)
    return(c(p, cp, aic, bic))
  }
))
rownames(ic) <- 1:NROW(ic)
colnames(ic) <- c("p", "Cp", "AIC", "BIC")
ic							# (17) p, Cp, AIC, BIC

# (18) Plot Cp vs p	
plot(ic[, "p"], ic[,"Cp"], pch=16, xlab="number of variables",	
  ylab="Mallows Cp Statistic", main="Cp Statistic vs Number of Variables")


#################################
# 예제 6-14
#################################
fit <- lm(Fertility ~ . , data = swiss)			# (1) 완전모형
summary(fit)						# (2) 회귀모형 결과
fit <- update(fit, .~. -Examination)			# (3) Examination 제거
summary(fit)						# (4) 회귀모형 결과


#################################
# 예제 6-15
#################################
model <- function(x, y) {				# (1) 모형만들기 함수
  formulas <- data.frame(integer(0), character(0))

  for (i in 1:length(x)) {
    tmp <- apply(t(combn(x, i)), 1, 
       function(x) paste(y, paste(x, collapse="+"), sep="~"))
    formulas <- rbind(formulas, data.frame(cnt=i, formula=tmp))
  }
  formulas$formula <- as.character(formulas$formula)

  return(formulas)
}

variable <- names(swiss) 				

x <- variable[-1]					# (2) 독립변수
y <- variable[1]					# (3) 종속변수

head(models <- model(x, y)) 			# (4) 모델식 만들기

forward <- function(models, cnt, formula, p) {	# (5) 전진선택법 구현
  models <- as.character(models[models$cnt==cnt, "formula"])

  if (cnt > 1) {
    formula <- strsplit(formula, "~")[[1]][2]
    variabls <- strsplit(formula, "\\+")[[1]]

    for(variable in variabls) {
       models <- grep(variable, models, value=T)
    }
  }

  result <- c(character(0), numeric(0))

  for (i in 1:length(models)) {
    fit <- lm(models[i], data=swiss)	
    p.val <- summary(fit)$coefficients[-1, 4]
    min.val <- p.val[p.val==min(p.val)]

    if (min.val>p) next

    result <- rbind(result, 
      data.frame(formula=models[i], p.val=min.val))
  }

  if (NROW(result)==0) return(NULL)
  row.names(result) <- NULL
  return(result[min(result[,"p.val"])==result$p.val,])
}

p <- 0.05							# (6) 유의수준
# (7) 1개 변수 추가
(var1 <- forward(models, 1, "", p))			
# (8) 2개 변수 추가
(var2 <- forward(models, 2, as.character(var1$formula), p))
# (9) 3개 변수 추가
(var3 <- forward(models, 3, as.character(var2$formula), p))
# (10) 4개 변수 추가
(var4 <- forward(models, 4, as.character(var3$formula), p))
# (11) 5개 변수 추가
(var5 <- forward(models, 5, as.character(var4$formula), p))


#################################
# 예제 6-16
#################################
summary(fit <- lm(Fertility ~ ., data = swiss))	# (1) 완전모형
step.fit <- step(fit)					# (2) 단계적 변수선택
summary(step.fit)					# (3) 선택된 모형
step.fit$anova						# (4) 제거된 변수에 대한 정보	


back.fit <- step(fit, direction="backward")		# (5) 후진 제거법
summary(back.fit)					# (6) 선택된 모형

forward.fit <- step(fit, direction="forward")		# (7) 전진 선택법
summary(forward.fit)					# (8) 선택된 모형


#################################
# 6.3.3, 그림 6-16
#################################
if (length(grep("corrplot", installed.packages()[,1]))==0)	# (1) corrplot 패키지 설치
  install.packages("corrplot", repos="http://cran.nexr.com")
library(corrplot)						# (2) corrplot 패키지
(corr.mat <- cor(LifeCycleSavings))				# (3) 상관계수 행렬
corrplot(corr.mat, method="ellipse", order = "AOE")		# (4) 상관 행렬도
fit <- lm(sr ~ ., data = LifeCycleSavings)			# (5) 완전모형
summary(fit)							# (6) 완전모형 정보

fit <- update(fit, .~. -(dpi+pop75), data = LifeCycleSavings) # (7) dpi, pop75 제거모형
summary(fit)							# (8) 모형 정보


#################################
# 예제 6-17
#################################
variable <- names(LifeCycleSavings) 		# (1) LifeCycleSavings	
x <- variable[-1]					# (2) 독립변수

for (i in x) {						# (3) 공차한계, VIF 구하기
  form <- paste(i, ".", sep="~")
  fit <- lm(form, data=LifeCycleSavings[, -1])
  rs <- summary(fit)$r.squared
  tolerance <- 1-rs
  vif <- 1/tolerance
  print(sprintf("%5s : Tolerance = %f, VIF = %f", i, tolerance, vif))
}

library(car)						# (4) car 라이브러리
fit <- lm(sr~., data=LifeCycleSavings)    		# (5) 완전모형
vif(fit)							# (6) vif 함수
1/vif(fit)						# (7) 공차한계


#################################
# 예제 6-18
#################################
X <- cbind(1, LifeCycleSavings[, -1])	# (1) X
X <- scale(as.matrix(X), center=F)		# (2) 표준화행렬로 변환	
(XX <- t(X) %*% X)				# (3) X'X
(e.val <- eigen(XX)$values)			# (4) 고유값
(CI <- sqrt(max(e.val)/e.val))		# (5) 상태지수

# (6) perturb패키지 설치
if (length(grep("perturb", installed.packages()[,1]))==0)
  install.packages("corrplot", repos="http://cran.nexr.com")
library(perturb)					
colldiag(LifeCycleSavings[, -1])		# (7) colldiag 함수 



#################################
# 그림 6-17, 6-18 
#################################
dim(anscombe)
names(anscombe)
head(anscombe)

# (1) Statistic
colMeans(anscombe)
apply(anscombe, 2, var)


# (2) correlation
attach(anscombe)
apply(t(1:4), 2, function(x) cor(get(paste("x", x, sep="")),get(paste("y", x, sep=""))))
detach(anscombe)


tmp <- unlist(as(anscombe, "vector"))
x <- tmp[1:(NROW(anscombe)*4)]
y <- tmp[(NROW(anscombe)*4+1):(NROW(anscombe)*8)]
quartet <- rep(c("I", "II", "III", "IV"), each=NROW(anscombe))

anscombe.dat <- data.frame(x, y, quartet) 

# (3) Regression
out <- 
apply(t(c("I", "II", "III", "IV")), 2, 
  function(key) {
    lm.obj <- lm(y~x, subset=quartet==key, data=anscombe.dat)
    round(c(lm.obj$coef["(Intercept)"],
    lm.obj$coef["x"],
    anova(lm.obj)["x", "Sum Sq"],
    anova(lm.obj)["Residuals", "Sum Sq"],
    summary(lm.obj)$coefficients["x", "Std. Error"],
    summary(lm.obj)$r.squared),2)})
dimnames(out) <- 
  list(c("Coefficient Intercept", "Coefficient x",
  "Regression sum of squares", "Residuals sum of squares",
  "Estimated standard error of b1", "Multiple R-Square"), 
  c("I", "II", "III", "IV"))
out


# (4) Chart
library(lattice)

xyplot(y ~ x | quartet, data=anscombe.dat,
  panel = function(x, y, ...) {
	panel.xyplot(x, y, col="orange", pch=16, cex=1.1)
	panel.xyplot(x, y, type="g")
	panel.lmline(x, y, ...)
  },
  main="y ~ x | quartet")


# (5) Residuals
resi <- apply(t(c("I","II","III","IV")), 2, 
  function(key) lm(y~x, subset=quartet==key, data=anscombe.dat)$resi)

anscombe.dat <- data.frame(anscombe.dat, residual=as.vector(resi)) 

qqmath(~ residual | quartet, data=anscombe.dat,
       panel = function(x, ...) {
          panel.qqmathline(x, ...)
          panel.qqmath(x, col="orange", pch=16, cex=1.1)
          panel.qqmath(x, type="g") 
       },
  main="Normal Q-Q Plot")


#################################
# 그림 6-19
#################################
set.seed(1)
x <- rnorm(40)
x <- x[x > -2 & x < 1]
set.seed(13)
y <- x + rnorm(length(x))

x1 <- 10; y1 <- 10
x2 <- 10; y2 <- 0

fit0 <- lm(y~x)
fit1 <- lm(c(y, y1)~c(x, x1))
fit2 <- lm(c(y, y2)~c(x, x2))

plot(x, y, xlim=c(-2, 10), ylim=c(-2, 10), cex=1.2,
  main="Two Kinds of Outliers Differently \nAffecting Regression Analysis")
points(x1, y1, pch=17, cex=1.2, col="red")
points(x2, y2, pch=15, cex=1.2, col="blue")

abline(fit0)
abline(fit1, lty="longdash", col="red")
abline(fit2, lty="dotted", col="blue")


#################################
# x 관점의 이상치
#################################
xx <- c(x, x1, x2)					# (1) x
length(xx)						# (2) x의 개수
X <- cbind(1, xx)					# (3) Design Matrix
hii<- diag(X%*%solve(t(X)%*%X)%*%t(X))		# (4) hii
which(hii > 2*mean(hii))				# (5) Outlier 위치
xx[hii > 2*mean(hii)]					# (6) Outlier 값


#################################
# 예제 6-19
#################################
lund.criticl <- function(n, k, alpha) {			# (1) 기각치 구하는 함수
  F <- qf(1-alpha/n, df1=1, df2=n-k-1, lower.tail=TRUE)
  crit <- sqrt((n-k)*F/(n-k-1+F))
  crit
}

x <- c(x, x1, x2)					# (2) x
y <- c(y, y1, y2)					# (3) y
lm.fit <- lm(y~x)					# (4) lm() 함수이용
sr.fit <- rstandard(lm.fit)				# (5) 표준화잔차
alpha <- 0.05						# (6) 유의수준
(n <- length(sr.fit))					# (7) 관측치
k <- 1							# (8) 변수의 개수
(cirt <- lund.criticl(n, k, alpha))			# (9) Lund 기각치 구하기
(idx <- which(abs(sr.fit) > cirt))			# (10) Outlier 위치
sr.fit[idx]						# (11) Outlier 표준화잔차
sprintf("(%s, %s)", x[idx], y[idx])			# (12) Outlier 관측치


#################################
# 예제 6-20
#################################
esr.fit <- rstudent(lm.fit)				# (1) 외적 스튜던트화 잔차
alpha <- 0.05						# (2) 유의수준
(cirt <- qt(1-alpha/2, df=n-k-2, lower.tail=TRUE))	# (3) 기각치 구하기
(idx <- which(abs(esr.fit) > cirt))			# (4) Outlier 위치
esr.fit[idx]						# (5) Outlier 표준화잔차
sprintf("(%s, %s)", x[idx], y[idx])			# (6) Outlier 관측치



#################################
# 6.4.2
#################################
fit <- lm(sr ~ ., data=LifeCycleSavings)		# (1) lm fit
x <- LifeCycleSavings[,-1]				# (2) x
X <- as.matrix(cbind(1, x))				# (3) Design Matrix
hii <- diag(X%*%solve(t(X)%*%X)%*%t(X))		# (4) 지렛값(공식 이용)
hii2 <- hat(X)						# (5) 지렛값(hat() 함수 이용)
eps <- 1e-10						# (6) EPS
all(hii2-hii<eps)					# (7) 두 값의 비교
(k <- length(fit$coefficients) - 1)			# (8) 변수의 개수
(n <- NROW(X))					# (9) 자료의 개수	
hii[hii > (2*(k+1))/n]					# (10) 영향치 판정(지렛값)

esr <- rstudent(fit)					# (11) 외적스튜던트잔차
DFFITS <- sqrt(hii/(1-hii))*esr 			# (12) DFFITS
DFFITS2 <- dffits(fit)					# (13) DFFITS(dffits() 함수이용)
all(hii2-hii<eps)					# (14) 두 값의 비교
DFFITS[abs(DFFITS) > 2*sqrt((k+1)/n)]		# (15) 영향치 판정(DFFITS)

DFBETAS <- dfbetas(fit)				# (16) DFBETAS(dfbetas() 이용)
cond <- abs(DFBETAS)> 2/sqrt(n)			# (17) 판정기준 결과
idx <- apply(cond, 1, any) 				# (18) 영향치 판정 관측치 번호
(cond.true <- cond[idx,]) 				# (19) 영향치 판정 논리값
ifelse(DFBETAS[idx,]* cond.true==0, NA,   	# (20) 영향치 판정 결과
       DFBETAS[idx,]* cond.true)

sr <- rstandard(fit)					# (21) 표준화잔차
cook <- hii/((k+1)*(1-hii))*sr^2			# (22) Cook의 D통계량
cook2 <- cooks.distance(fit)			# (23) cooks.distance() 함수
all(cook2-cook<eps)					# (24) 두 값의 비교
alpha <- 0.5						# (25) 유의수준
cook[cook > qf(alpha, k+1, n-k-1)]		# (26) 영향치 판정 결과

COVRATIO <- 1/((1+(esr^2-1)/(n-k-1))^(k+1)*(1-hii)) # (27) COVRATIO
COVRATIO2 <- covratio(fit)				# (28) COVRATIO(covratio() 이용)
all(COVRATIO2-COVRATIO<eps)			# (29) 두 값의 비교
COVRATIO[abs(COVRATIO-1)>3*(k+1)/n]		# (30) 영향치 판정 결과

MD <- (n-1)*(hii-1/n)				# (31) Mahalanobis 거리
MD[MD > quantile(MD, prob=0.95)]			# (32) 95% 이상 값

infl <- influence.measures(fit)			# (33) influence.measures()
attributes(infl)						# (34) infl 객체
infl$infmat[apply(infl$is.inf, 1, any),]		# (35) 영향치 



summary(lm.cars)

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(4, 4, 4, 1))
plot(lm.cars)
par(op)

influence.measures(lm.cars)
cooks.distance(lm.cars)















