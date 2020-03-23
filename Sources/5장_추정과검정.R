#################################
# 예제 5-4
#################################
# 1. 표본평균
X <- 1:5; n <- 3

pop.var <- function(x) {			# (1) 모분산 함수
  sum((x-mean(x))^2)/length(x)
}

samples <- expand.grid(X.1=X, X.2=X, X.3=X)	# (2) 표본의 경우 수
NROW(samples)					# (3) 표본의 개수
head(samples,3)					# (4) 표본의 앞 3건
tail(samples,3)					# (5) 표본의 뒤 3건
mean(apply(samples, 1, mean))			# (6) E(X_bar)
mean(X)						# (7) mu
pop.var(apply(samples, 1, mean))   		# (8) V(X_bar)
pop.var(X)/n					# (9) sigma^2/n
sqrt(pop.var(apply(samples, 1, mean)))    	# (10) standard error
sqrt(pop.var(X)/n)				# (11) sigma/sqrt(n)
mean(apply(samples, 1, var))			# (12) E(S^2)
pop.var(X)					# (13) sigma^2


# 2. 표본비율
is.even <- function(x) !x%%2			# (1) 짝수 판별함수
pr <- function(x) sum(is.even(x))/n		# (2) 확률구하는 함수
p.hat <- apply(samples, 1, pr)			# (3) p.hat	
mean(p.hat)					# (4) E(p.hat)
(p <- sum(is.even(X))/length(X))		# (5) p
pop.var(p.hat)					# (6) V(p.hat)
p*(1-p)/n					# (7) p*(1-p)/n
sqrt(pop.var(p.hat))				# (8) standard error


#################################
# 그림 5-1
#################################
curve(dnorm, -3, 3, ylab="", xlab="", axes=F)
x <- seq(-3, -2, length=100)
px <- dnorm(x)
abline(h=0)
polygon(c(-3, x,-2), c(0, px,0), col="blue")
polygon(c(3, -x,2), c(0, px,0), col="blue")
text(0, 0.3, expression(1-alpha))
text(-2.5, 0.06, expression(over(alpha,2)))
text(2.5, 0.06, expression(over(alpha,2)))
mtext(expression(-z[over(alpha,2)]), side=1, adj=0.5, at=-2)
mtext(expression(z[over(alpha,2)]), side=1, adj=0.5, at=2)



#################################
# 그림 5-2, 예제 5-5
#################################
mu <- 0; sigma <- 1						# (1) 모수
n <- 20								# (2) 표본 크기
rep.cnt <- 50							# (3) 실험회수
samples <- matrix(rnorm(rep.cnt*n, mu, sigma), ncol=n)		# (4) 실험표본
xbar <- apply(samples, 1, mean)					# (5) 표본평균
SE <- sqrt(sigma^2/n) 						# (6) 표준오차
alpha <- 0.10							# (7) 신뢰수준 
z.quantile   <- qnorm(1-alpha/2)				# (8) z_alpha/2
CI.L <- xbar - z.quantile  * SE					# (9) 신뢰하한
CI.U <- xbar + z.quantile  * SE					# (10) 신뢰상한
plot(0, xlim=c(-1.2,1.2), ylim=c(0,50), type='n',
  xlab="x", ylab="",
  main=expression("90%"~Confidence~Intervals~with~bar(X)))
arrows(CI.L, 1:rep.cnt, CI.U, 1:rep.cnt,			# (11) 신뢰구간출력
  angle=90, code=3, length=0.03)
points(xbar, 1:rep.cnt, pch=16)					# (12) 점추정치출력
abline(v=mu) 							# (13) 모평균출력
mtext(expression(mu), side=1, at=mu, line=2)           		# (14) 모평균출력




#################################
# 예제 5-6
#################################
x <- c(28.9, 32.4, 29.8, 30.6, 27.8, 29.4, 31.3)	# (1) 데이터
n <- length(x)						# (2) 데이터 개수	
sigma <- 1.5						# (3) 표준편차
xbar <- mean(x)						# (4) 표본평균
alpha <- 0.1						# (5) 유의수준
# 1.모분산을 알 경우
SE <- sqrt(sigma^2/n) 					# (6) 표준오차
z.quantile <- qnorm(1-alpha/2)				# (7) z_alpha/2
(CI.L <- xbar - z.quantile * SE)			# (8) 신뢰하한
(CI.U <- xbar + z.quantile * SE)			# (9) 신뢰상한
# 2.모분산을 모를 경우
SE <- sqrt(var(x)^2/n)					# (10) 표준오차 (S/sqrt(n))
t.quantile <- qt(1-alpha/2, n-1)			# (11) t_alpha/2
(CI.L <- xbar - t.quantile * SE)			# (12) 신뢰하한
(CI.U <- xbar + t.quantile * SE)			# (13) 신뢰상한



#################################
# 그림 5-3
#################################
df.chisq <- 5
f <- function(x) dchisq(x, df=df.chisq)
curve(f, 0, 15, ylab="", xlab="", axes=F)
abline(h=0)
x <- seq(0, 1, length=100)
px <- dchisq(x, df=df.chisq)
polygon(c(x, 1), c(px, 0), col="blue")
x <- seq(11, 15, length=100)
px <- dchisq(x, df=df.chisq)
polygon(c(11, x, 15), c(0, px, 0), col="blue")
text(4, 0.1, expression(1-alpha))
text(0, 0.02, expression(over(alpha,2)))
text(13, 0.02, expression(over(alpha,2)))
mtext(expression(chi[(list(1-over(alpha,2),n))]^2), 
  side=1, adj=0.5, at=1)
mtext(expression(chi[(list(over(alpha,2),n))]^2),
  side=1, adj=0.5, at=11)



#################################
# 그림 5-4, 예제 5-7
#################################
mu <- 0; sigma <- 1						# (1) 모수
n <- 20								# (2) 표본 크기
rep.cnt <- 50							# (3) 실험회수
samples <- matrix(rnorm(rep.cnt*n, mu, sigma), ncol=n)		# (4) 실험표본
S.sq <- apply(samples, 1, var)					# (5) 표본분산
alpha <- 0.10							# (6) 신뢰수준 
chi.quantile1 <- qchisq(alpha/2, n-1, lower.tail=T)		# (7) chi_alpha/2
chi.quantile2 <- qchisq(1-alpha/2, n-1, lower.tail=T)		# (8) chi_1-alpha/2
CI.L <- (n-1)*S.sq/chi.quantile1				# (9) 신뢰하한
CI.U <- (n-1)*S.sq/chi.quantile2				# (10) 신뢰상한
plot(0, xlim=c(0,4), ylim=c(0,50), type='n', xlab="x", ylab="",
  main=expression("90%"~Confidence~Intervals~with~S^2))
arrows(CI.L, 1:rep.cnt, CI.U, 1:rep.cnt,			# (11) 신뢰구간출력
  angle=90, code=3, length=0.03)
points(S.sq, 1:rep.cnt, pch=16)					# (12) 점추정치출력
abline(v=sigma^2) 						# (13) 모분산위치
mtext(expression(sigma^2), side=1, at=1, line=2)		# (14) 모분산출력


#################################
# 예제 5-8
#################################
x <- c(120, 122, 118, 121, 117, 116, 120)			# (1) 데이터
n <- length(x)							# (2) 데이터 개수	
mu <- 120								# (3) 모평균
S.sq <- var(x)							# (4) 표본분산
alpha <- 0.1							# (5) 유의수준
# 1.모평균을 알 경우
SS <- sum((x-mu)^2) 						# (6) 편차제곱합
chi.quantile1 <- qchisq(alpha/2, n, lower.tail=F)		# (7) chi_alpha/2
chi.quantile2 <- qchisq(1-alpha/2, n, lower.tail=F)		# (8) chi_1-alpha/2
(CI.L <- SS/chi.quantile1)					# (9) 신뢰하한
(CI.U <- SS/chi.quantile2)					# (10) 신뢰상한
# 2.모평균을 모를 경우
chi.quantile1 <- qchisq(alpha/2, n-1, lower.tail=F)		# (11) chi_alpha/2
chi.quantile2 <- qchisq(1-alpha/2, n-1, lower.tail=F)		# (12) chi_1-alpha/2
(CI.L <- (n-1)*S.sq/chi.quantile1)				# (13) 신뢰하한
(CI.U <- (n-1)*S.sq/chi.quantile2)				# (14) 신뢰상한


#################################
# 예제 5-9
#################################
n <- 100					# (1) 표본 크기
X <- 3						# (2) 확률변수의 실현치
alpha <- 0.05					# (3) 유의수준
phat <- X/n					# (4) 표본비율
SE <- sqrt(phat*(1-phat)/n)			# (5) 표준오차
z.quantile <- qnorm(1-alpha/2)			# (6) z_alpha/2
(CI.L <- phat - z.quantile * SE)		# (7) 신뢰하한
(CI.U <- phat + z.quantile * SE)		# (8) 신뢰상한



#################################
# 예제 5-10, 그림 5-5
#################################
mu1 <- 0; sigma1 <- 2				# (1) X1~N(0,2^2)
mu2 <- 2; sigma2 <- 1				# (2) X2~N(2,1^2)
n1 <- 30; n2 <- 35				# (3) 확률표본 크기

f1 <- function(x) dnorm(x, mu1, sigma1)		# (4) X1 확률밀도함수
f2 <- function(x) dnorm(x, mu2, sigma2)		# (5) X2 확률밀도함수
SE <- sqrt(sigma1^2/n1+sigma2^2/n2)		# (6) SE
f3 <- function(x) dnorm(x, mu1-mu2, SE)		# (7) 모평균차 확률밀도함수

curve(f1, xlim=c(-5, 7), ylim=c(0, 1), 		# (8) X1 분포도
  col=2, lty=3, ylab="Density")
curve(f2, add=T, col=4, lty=5)			# (9) X2 분포도
curve(f3, add=T, col=1)				# (10) 두 표본평균차 분포도
	
abline(v=mu1-mu2, lty=2)			# (11) 모평균차
mtext(expression(mu[1]-mu[2]), side=1, 
  at=mu1-mu2, line=2)				# (12) 모평균차출력

alpha <- 0.1					# (13) 유의수준
z.quantile <- qnorm(1-alpha/2)			# (14) z_alpha/2

xbar1 <- mean(rnorm(n1, mu1, sigma1))		# (15) 표본평균 1
xbar2 <- mean(rnorm(n2, mu2, sigma2))		# (16) 표본평균 2
	
(CI.L <- xbar1-xbar2 - z.quantile * SE)		# (17) 신뢰하한
(CI.U <- xbar1-xbar2 + z.quantile * SE)		# (18) 신뢰상한

arrows(CI.L, 0, CI.U, 0,			# (19) 신뢰구간출력
  angle=90, code=3, length=0.03, lwd=2)
points(xbar1-xbar2, 0, pch=16, cex=1.5)		# (20) 점추정치출력

legend(1, 1, col=c(2,4,1), lty=c(3,5,1), cex=0.8, bty="n",
  c(expression(X[1]*"~"*N(0, 2^2)),expression(X[2]*"~"*N(2, 1^2)),
    expression(bar(X[1])-bar(X[2])*"~"*N(mu[1]-mu[2],
    over(sigma[1]^2,n[1])+over(sigma[2]^2,n[2])))))  # (21) 범례



#################################
# 예제 5-11
#################################
n1 <- 30; n2 <- 35				# (1) 표본 크기
Xbar1 <- 107; Xbar2 <- 112			# (2) 표본평균
sigma1 <- sqrt(2.5); sigma2 <- sqrt(3.2)	# (3) 모표준편차
SE <- sqrt(sigma1^2/n1+sigma2^2/n2)		# (4) SE
alpha <- 0.05					# (5) 유의수준
z.quantile <- qnorm(1-alpha/2)			# (6) z_alpha/2
(CI.L <- Xbar1-Xbar2 - z.quantile * SE)		# (7) 신뢰하한
(CI.U <- Xbar1-Xbar2 + z.quantile * SE)		# (8) 신뢰상한




#################################
# 예제 5-12
#################################
n1 <- n2 <- 20					# (1) 표본 크기
Xbar1 <- 27.8; Xbar2 <- 25.4			# (2) 표본평균
S1 <- 1.5; S2 <- 2.1				# (3) 표본표준편차
alpha <- 0.1					# (4) 유의수준

# 1. 모분산이 같을 경우
t.df <- n1+n2-2					# (5) 자유도
sp.s <- ((n1-1)*S1^2+(n2-1)*S2^2)/t.df		# (6) S_p^2
t.quantile <- qt(1-alpha/2, t.df)		# (7) t_alpha/2
SE <- sqrt(sp.s)*sqrt(1/(n1-1)+1/(n2-1))	# (8) SE
(CI.L <- Xbar1-Xbar2 - t.quantile * SE)		# (9) 신뢰하한
(CI.U <- Xbar1-Xbar2 + t.quantile * SE)		# (10) 신뢰상한

# 2. 모분산이 다를 경우
t.df <- (S1^2/n1+S2^2/n2)^2/
  ((S1^2/n1)^2/(n1-1)+(S2^2/n2)^2/(n2-1))	# (11) 자유도
SE <- sqrt(S1^2/n1+S2^2/n2)			# (12) SE
t.quantile <- qt(1-alpha/2, t.df)		# (13) t_alpha/2
(CI.L <- Xbar1-Xbar2 - t.quantile * SE)		# (14) 신뢰하한
(CI.U <- Xbar1-Xbar2 + t.quantile * SE)		# (15) 신뢰상한



#################################
# 예제 5-13
#################################
X.bef <- c(25,27,30,28,30,27,29,29,30,29)	# (1) 비투약
X.aft <- c(29,30,29,30,30,28,30,28,30,30)	# (2) 투약
n <- length(X.bef)				# (3) 데이터 개수
t.df <- n-1					# (4) 자유도
Dbar <- mean(X.aft-X.bef)			# (5) 표본평균(D)
S.D <- sd(X.aft-X.bef)				# (6) 표본표준편차(D)
SE <- S.D/sqrt(n)				# (7) SE	
alpha <- 0.1					# (8) 유의수준
t.quantile <- qt(1-alpha/2, n-1)		# (9) t_alpha/2
(CI.L <- Dbar - t.quantile * SE)		# (10) 신뢰하한
(CI.U <- Dbar + t.quantile * SE)		# (11) 신뢰상한



#################################
# 예제 5-14
#################################
n.male <- 450; X.male <- 180				# (1) 남자
n.female <- 400; X.female <- 100			# (2) 여자
phat.male <- X.male/n.male				# (3) 남자 비율
phat.female <- X.female/n.female			# (4) 여자 비율
SE <- sqrt(phat.male*(1-phat.male)/n.male+
  phat.female*(1-phat.female)/n.female)			# (5) SE	
alpha <- 0.05						# (6) 유의수준
z.quantile <- qnorm(1-alpha/2)				# (7) z_alpha/2
(CI.L <- (phat.male - phat.female) - z.quantile * SE)	# (8) 신뢰하한
(CI.U <- (phat.male - phat.female) + z.quantile * SE)	# (9) 신뢰상한



#################################
# 예제 5-15
#################################
n1 <- 10; n2 <- 15					# (1) 표본 크기
S1.s <- 3.2; S2.s <- 2.8				# (2) 표본분산
alpha <- 0.1						# (3) 유의수준
F.quantile1 <- qf(alpha/2, n1-1, n2-1, lower.tail=F)	# (4) F_alpha/2
F.quantile2 <- qf(1-alpha/2, n1-1, n2-1, lower.tail=F)	# (5) F_1-alpha/2
(CI.L <-(S1.s/S2.s)/F.quantile1)			# (6) 신뢰하한
(CI.U <-(S1.s/S2.s)/F.quantile2)			# (7) 신뢰상한



#################################
# 예제 5-16, 그림 5-6
#################################
sigma <- 1						# (1) 모표준편차
f.95 <- function(d, alpha=0.05) {			# (2) 95% 신뢰구간에서
  z.quantile <- qnorm(1-alpha/2)         		# (2) 표본 크기 구하기 
  ceiling((z.quantile*sigma/d)^2)
}
f.90 <- function(d, alpha=0.1) {			# (3) 90% 신뢰구간에서
  z.quantile <- qnorm(1-alpha/2)   			# (3) 표본 크기 구하기
  ceiling((z.quantile*sigma/d)^2)		
}

curve(f.95, xlim=c(0.1,1), xlab="d", ylab="n", 		# (4) n과 d 관계
  main=expression("Sample Size by Limit of Error ("*sigma^2*"=1)"))	
curve(f.90, add=T, lty=2, col=2)			# (5) n과 d 관계
legend(0.7, 300, c("95%","90%"), lty=1:2, col=1:2)




#################################
# 예제 5-17
#################################
X <- c(12.3, 13.2, 14.0, 12.5, 14.2, 13.9, 11.9)	# (1) 표본
S.s <- var(X)						# (2) 표본분산
alpha <- 0.05						# (3) 유의수준
z.quantile <- qnorm(1-alpha/2)  			# (4) z_alpha/2
ceiling((z.quantile*sqrt(S.s)/0.2)^2)			# (5) 표본의 수




#################################
# 예제 5-18, 그림 5-7
#################################
# (1) 함수에 대해서 특정 구간에서 최대값을 찾는 방법
f <- function (p) p*(1-p)
(maximum <- optimize(f, c(0, 1), maximum=T)$maximum)

# (2) 미분하여 도함수를 구하고, 이를 방정식의 해로 찾는 방법
D(expression(p*(1-p)), "p")
D.f <- function (p) 1 - 2*p
uniroot(D.f, c(0, 1))$root

# (3) 그래프를 그리는 방법
curve(f, xlim=c(0,1), xlab="p", ylab="p(1-p)", xaxp=c(0,1,10))
abline(v=maximum, lty=2)
abline(h=maximum*(1-maximum), lty=2)



#################################
# 예제 5-19
#################################
alpha <- 0.1				# (1) 유의수준
z.quantile <- qnorm(1-alpha/2)  	# (2) z_alpha/2
d <- 0.05 				# (3) 오차의 한계
ceiling(1/4 * (z.quantile/d)^2)		# (4) 표본의 수



#################################
# 그림 5-8
#################################
# (1) 2차원 좌표 상에서 지정한 함수 f(x)에 대해서 주어진
#     x의 구간에서 그래프 면적을 채우는 사용자 정의 함수
fill <- function (f, interval, ...) {
  x <- seq(interval[1], interval[2], length=100)
  y <- f(x)
  polygon(c(x, rev(x)), c(y, rep(0, length(y))), ...)
}

alpha <- 0.05					# (2) 유의수준(1종 오류 확률)
curve(dnorm, xlim=c(-3, 3), type="l", 
  ylab="Pr(z)", xlab="z", main="Type I error")	# (3) 표준정규분포 그래프
fill(dnorm, c(-3, qnorm(alpha/2)), col="blue")  # (4) 1종 오류 범할 확률 α/2
fill(dnorm, c(qnorm(1-alpha/2), 3), col="blue") # (5) 1종 오류 범할 확률 α/2

text(0, 0.2, expression(1-alpha))
text(-2.5, 0.05, expression(over(alpha,2)))
text(2.5, 0.05, expression(over(alpha,2)))


#################################
# 그림 5-9
#################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(4, 4, 4, 1))

alpha <- 0.05					# (1) 유의수준(1종 오류 확률)
f.05 <- function(x, sigma=1) dnorm(x-sigma*0.5)	# (2) Z(X-0.5) 
f.35 <- function(x, sigma=1) dnorm(x-sigma*3.5)	# (3) Z(X-3.5)					
curve(dnorm, xlim=c(-6, 6), type="l", ylab="Pr(z)", xlab="z", 
  main="High Probability of type II error")		# (4) 표준정규분포 그래프

# (5)β의 크기가 큰 경우
fill(f.05, c(qnorm(alpha/2), qnorm(1-alpha/2)), col="blue")
segments(qnorm(alpha/2),0,qnorm(alpha/2),dnorm(qnorm(alpha/2)), 
  col="red", lwd=3)
segments(qnorm(1-alpha/2),0,qnorm(1-alpha/2),dnorm(qnorm(1-alpha/2)), 
  col="red", lwd=3)
curve(dnorm, xlim=c(-6, 6), type="l", add=T)
curve(f.05, xlim=c(-6, 6), type="l", add=T)

# (6)β의 크기가 작은 경우
curve(dnorm, xlim=c(-6, 6), type="l", ylab="Pr(z)", xlab="z",
  main="Lower Probability of type II error")		
fill(f.35, c(qnorm(alpha/2), qnorm(1-alpha/2)), col="blue")
segments(qnorm(alpha/2),0,qnorm(alpha/2),dnorm(qnorm(alpha/2)), 
  col="red", lwd=3)
segments(qnorm(1-alpha/2),0,qnorm(1-alpha/2),dnorm(qnorm(1-alpha/2)), 
  col="red", lwd=3)
curve(dnorm, xlim=c(-6, 6), type="l", add=T)
curve(f.35, xlim=c(-6, 6), type="l", add=T)

par(op)



#################################
# 그림 5-10
#################################
f <- function(x)
  1-power.t.test(delta=x, n=20, type="one.sample", strict=T)$power

curve(f, xlim=c(-1.5, 1.5), xlab="difference in means", 
  ylab="propability of a type II error",
  main="type II error in a Student T test")
abline(h=0,lty=3)
abline(h=0.05,lty=3)
abline(v=0,lty=3)


#################################
# 
#################################
1- power.t.test(n=20, delta=0.8, sd=1, sig.level=0.05,
    type="one.sample", alternative="two.sided", strict=T)$power

qt(0.025, df=19, lower.tail=F)

sqrt(20)*0.8/1

pt(2.093024, df=19, ncp=3.577709) - pt(-2.093024, df=19, ncp=3.577709)


1- power.t.test(n=20, delta=0.8, sd=1, sig.level=0.05,
    type="one.sample", alternative="two.sided")$power
pt(2.093024, df=19, ncp=3.577709)



#################################
# 그림 5-11
#################################
f <- function(x)
  power.t.test(delta=x, n=20, type="one.sample", strict=T)$power
curve(f, xlim=c(0, 1.5), ylab=expression("power ("~gamma~")"), 
  xlab=expression(theta),
  main="Power of a one-sample t-test")
arrows(0.53,0.6, 0.75, 0.6, length=0.1, code=1)
text(1, 0.6, expression(pi(theta)==P(Reject~H[0]~"|"~theta)))



#################################
# 그림 5-12
#################################
f.05 <- function(n) {
  f <- function(x)
    power.t.test(n=x, sig.level=.05, power=.80, type="one.sample", strict=T)$delta
  apply(t(n), 2, f)
}

f.01 <- function(n) {
  f <- function(x)
    power.t.test(n=x, sig.level=.01, power=.80, type="one.sample", strict=T)$delta
  apply(t(n), 2, f)
}

f.001 <- function(n) {
  f <- function(x)
    power.t.test(n=x, sig.level=.001, power=.80, type="one.sample", strict=T)$delta
  apply(t(n), 2, f)
}

curve(f.05, xlim=c(10, 200), xlab="sample size", 
  ylab=expression(theta==group("|",mu-mu[0],"|")),
  main="Difference in means vs Sample size for fixed power 0.80")
curve(f.01, add=T, col="red", lty=2)
curve(f.001, add=T, col="blue", lty=3)

legend("topright", lty=1:3, col=c(par("fg"), "red", "blue"), 
  paste("significance level =", c(.05, .01, .001)))



#################################
# 그림 5-13
#################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(2, 1, 4, 1))
alpha <- 0.05					# (1) 유의수준
# (2) H0: μ>=μ0
curve(dnorm, xlim=c(-3, 3), type="l", ylab="", xlab="", axes=F, 
  main=expression(H[0]:mu<=mu[0]))		# 표준정규분포 그래프
abline(h=0)
fill(dnorm, c(qnorm(1-alpha), 3), col="blue") 	# 1종 오류 범할 확률 α
text(0, 0.3, expression(1-alpha))
text(0, 0.25, "Acceptance\nRegion")
text(2.5, 0.06, expression(alpha))
text(2.5, 0.12, "Rejection\nRegion")
mtext(expression(z[alpha]), side=1, adj=0.5, at=1.7)
# (3) H0: μ<=μ0
curve(dnorm, xlim=c(-3, 3), type="l", ylab="", xlab="", axes=F, 
  main=expression(H[0]:mu>=mu[0]))		# 표준정규분포 그래프
abline(h=0)
fill(dnorm, c(-3, qnorm(alpha)), col="blue") 	# 1종 오류 범할 확률 α
text(0, 0.3, expression(1-alpha))
text(0, 0.25, "Acceptance\nRegion")
text(-2.5, 0.06, expression(alpha))
text(-2.5, 0.12, "Rejection\nRegion")
mtext(expression(-z[alpha]), side=1, adj=0.5, at=-1.7)
# (4) H0: μ=μ0
curve(dnorm, xlim=c(-3, 3), type="l", ylab="", xlab="", axes=F, 
  main=expression(H[0]:mu==mu[0]))		# 표준정규분포 그래프
abline(h=0)
fill(dnorm, c(qnorm(1-alpha/2), 3), col="blue") # 1종 오류 범할 확률 α/2
fill(dnorm, c(-3, qnorm(alpha/2)), col="blue") 	# 1종 오류 범할 확률 α/2
text(0, 0.3, expression(1-alpha))
text(0, 0.25, "Acceptance\nRegion")
text(-2.5, 0.06, expression(over(alpha,2)))
text(-2.5, 0.14, "Rejection\nRegion")
text(2.5, 0.06, expression(over(alpha,2)))
text(2.5, 0.14, "Rejection\nRegion")
mtext(expression(-z[over(alpha,2)]), side=1, adj=0.5, at=-2)
mtext(expression(z[over(alpha,2)]), side=1, adj=0.5, at=2)
par(op)



#################################
# 예제 5-20
#################################
n <- 100; xbar <- 23
mu <- 25; sigma <- sqrt(150)
(z0 <- (xbar-mu)/(sigma/sqrt(n)))	# (1) Z_0
z <- abs(z0)				# (2) |Z_0|
pnorm(-z) + pnorm(z,lower.tail=F)	# (3) P(Z<-z)+P(Z>z)
2 * pnorm(-z)				# (4) 2 * P(Z<-z)
2 * pnorm(z, lower.tail=F)		# (5) 2 * P(Z>z)
2 * ifelse(z0<=0, pnorm(z0), 
  pnorm(z0, lower.tail=F))		# (6) 2 * (P(Z<-z) 또는 P(Z>z))



#################################
# 예제 5-21
#################################
n <- 25; xbar <- 4.8
mu <- 5; S <- sqrt(0.3)
(T0 <- (xbar-mu)/(S/sqrt(n)))		# (1) T_0
pt(T0, df=n-1)				# (2) P(T<T_0)
pt(-T0, df=n-1, lower.tail=F)		# (3) P(T>-T_0)



#################################
# 예제 5-22
#################################
x <- c(198, 201, 199, 189, 200, 199, 198, 189, 205, 195)
n <- length(x); (xbar <- mean(x))
mu <- 200; (S <- sd(x))
alpha <- 0.05
(T0 <- (xbar-mu)/(S/sqrt(n)))				# (1) T_0
qt(alpha, df=n-1, lower.tail=F)				# (2) t_0.05;9
pt(T0, df=n-1, lower.tail=F)				# (3) p-value

(result<-t.test(x, mu=200, alternative="greater"))	# (4) t.test() 함수
unlist(result)						# (5) Return value



#################################
# 예제 5-23
#################################
x <- c(198, 201, 199, 189, 200, 199, 198, 189, 205, 195)
n <- length(x)
mu <- 200; sigma0 <- 5
alpha <- 0.05
(chisq <- sum((x-mu)^2)/sigma0^2)			# (1) chi^2
qchisq(alpha/2, df=n, lower.tail=F)			# (2) chi_0.025;10
qchisq(1-alpha/2, df=n, lower.tail=F)			# (3) chi_0.975;10
PVAL <- pchisq(chisq, df=n)
2 * min(PVAL, 1 - PVAL)					# (4) p-value



#################################
# 예제 5-24
#################################
x <- c(198, 201, 199, 189, 200, 199, 198, 189, 205, 195)
n <- length(x)
(S <- sd(x)); sigma0 <- 5
alpha <- 0.05
(chisq <- (n-1)*S^2/sigma0^2)		# (1) chi^2
qchisq(1-alpha, df=n-1, lower.tail=F)	# (2) chi_0.95;9
PVAL <- pchisq(chisq, df=n)
1 - PVAL				# (3) p-value



#################################
# 예제 5-25
#################################
n <- 100; x <- 3; (phat <- x/n) 
p0 <- 0.05
alpha <- 0.05
(z0 <- (phat-p0)/sqrt(p0*(1-p0)/n))		# (1) Z_0
-qnorm(alpha, lower.tail=F)			# (2) -Z_0.05
pnorm(z0)					# (3) p-value=P(Z<Z_0)
prop.test(x=3, n=100, p=0.05, alternative="less",
   conf.level=0.95)				# (4) prop.test() 함수
alternative <- "less"				# (5) 좌단측검정
x <- cbind(x, n-x)				# (6) 관측치
E <- cbind(n*p0, n*(1-p0))			# (7) 기대치
(chisq <- sum((abs(x-E) - 0.5)^2/E))		# (8) Yates의 카이제곱 통계량
z <- sign(phat - p0) * sqrt(chisq)		# (9) z 통계량
pnorm(z, lower.tail=(alternative=="less"))	# (10) p-value



#################################
# 예제 5-26
#################################
n1 <- 30; n2 <- 35
xbar1 <- 107; xbar2 <- 112
sigma1 <- sqrt(2.5); sigma2 <- sqrt(3.2)
alpha <- 0.05
(z0 <- (xbar1 - xbar2)/sqrt(sigma1^2/n1+sigma2^2/n2)) 	# (1) z0
-qnorm(alpha, lower.tail=F)				# (2) Z_0.05
pnorm(z0)						# (3) p-value



#################################
# 예제 5-27
#################################
x1 <- c(105, 110, 120, 90, 100, 136, 125, 188, 105, 118, 97, 109,
        103, 110, 115, 99)
x2 <- c(95, 120, 110, 115, 116, 108, 125, 98, 85)
(n1 <- length(x1)); (n2 <- length(x2))
(xbar1 <- mean(x1)); (xbar2 <- mean(x2))
df.t <- n1+n2-2
(S1 <- sd(x1)); (S2 <- sd(x2))
alpha <- 0.05
# 분산이 같을 경우
(SP <- ((n1-1)*S1^2+(n2-1)*S2^2)/df.t)		# (1) SP^2
(t0 <- (xbar1 - xbar2)/sqrt(SP*(1/n1+1/n2))) 	# (2) t0
qt(alpha/2, df=df.t, lower.tail=F)		# (3) t_0.05/2
2*pt(t0, df=df.t, lower.tail=F)			# (4) p-value
t.test(x1, x2, var.equal=T)			# (5) t.test()
# 분산이 다를 경우
(t0 <- (xbar1 - xbar2)/sqrt(S1^2/n1+S2^2/n2)) 	# (6) t0
(df.t <- (S1^2/n1+S2^2/n2)^2/
  ((S1^2/n1)^2/(n1-1)+(S2^2/n2)^2/(n2-1)))      # (7) df
qt(alpha/2, df=df.t, lower.tail=F)		# (8) t_0.05/2
2*pt(t0, df=df.t, lower.tail=F)			# (9) p-value
t.test(x1, x2, var.equal=F)			# (10) t.test()




#################################
# 예제 5-28
#################################
bef <- c(145,154,149,178,167,154,166,144,143,150)
aft <- c(143,150,140,166,150,134,160,145,140,145)
(D.bar <- mean(bef-aft))
(S.D <- sd(bef-aft))
(T <- D.bar/(S.D/sqrt(10)))				# T_0
t.df <- length(bef) - 1					# 자유도
alpha <- 0.05
qt(alpha, t.df, lower.tail=F)				# 기각역
pt(T, t.df, lower.tail=F)				# 유의확률
t.test(x=bef, y=aft, alternative="greater", paired=T)	# t.test()



#################################
# 예제 5-29
#################################
(certi <- matrix(c(107, 63, 43, 37), ncol=2, byrow=T,
  dimnames=list(c("Pass","Fail"),c("Tablet","General"))))
(n <- colSums(certi))					# n₁,n₂
(pr.2 <- prop.table(certi, margin=2))			# p₁,p₂,(1-p₁),(1-p₂) 추정치 
(pr <-prop.table(apply(certi, 1, sum)))			# p, (1-p) 추정치

(Z <- (pr.2["Pass","Tablet"]-pr.2["Pass","General"]) / 	# Z
  sqrt(pr["Pass"]*pr["Fail"]*(1/n["Tablet"]+1/n["General"])))

alpha <- 0.05
qnorm(alpha, lower.tail=F)				# 기각역
pnorm(Z, lower.tail=F)					# 유의확률

prop.test(certi["Pass",], n, alternative="greater")	# prop.test



#################################
# 예제 5-30
#################################
A <- c(172, 180, 181, 180, 175, 178, 182, 176, 180)
B <- c(180, 175, 177, 180, 178, 177, 180, 180, 181, 182)

(SS.A <- var(A)); (SS.B <- var(B))
df.A <- length(A)-1; df.B <- length(B)-1

(F <- SS.A/SS.B)				# (1) F

alpha <- 0.05
qf(alpha/2, df1=df.A, df2=df.B, lower.tail=F)	# (2) F(0.025,ν1=8,ν2=9)
qf(1-alpha/2, df1=df.A, df2=df.B, lower.tail=F)	# (3) F(0.975,ν1=8,ν2=9)

PVAL <- pf(F, df1=df.A, df2=df.B)
2*min(PVAL, 1-PVAL)				# (4) 유의확률

var.test(A, B)					# (5) var.test()









