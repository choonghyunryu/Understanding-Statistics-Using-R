# population {1, 2, 3, 4}, sample size n=2
X <- 1:4						# 모집단 
(mu <- mean(X))						# 모평균

pop.var <- function(x) {				# 모분산 함수
  mu <- mean(x)
  n <- length(x)
  sum((x-mu)^2)/n
}

(sigma.2 <- pop.var(X))					# 모분산
(sigma <- sqrt(pop.var(X)))				# 모표준편차

(N <- length(X))					# N
n <- 2							# n

#################################
# sampling with replacement
#################################
(s.wr <- expand.grid(X.1=X, X.2=X))			# (1) 16개 표본 종류 생성
rownames(s.wr) <- 
  paste("Sample", 1:NROW(s.wr))				# (2) 표본의 이름 부여
s.wr.mean <- apply(s.wr, 1, mean)			# (3) 표본평균 계산
s.wr.var <- apply(s.wr, 1, var)				# (4) 표본분산 계산
s.wr <- 
  cbind(s.wr, s.wr.mean, s.wr.var)  			# (5) 데이터 병합
dimnames(s.wr)[[2]][c(3,4)] <- c("mean","var")	# (6) 열 이름 지정
s.wr							# (7) 최종 데이터

mean(s.wr.mean)						# (8) E(X_):mean 함수
(X.bar <- as.numeric(names(table(s.wr.mean))))		# (9) X_
p.X.bar <- table(s.wr.mean)/length(s.wr.mean)		# (10) X_의 분포
(e.Xbar <- sum(X.bar*p.X.bar))				# (11) E(X_):ΣX_P(X_)	

p.X <- table(as.matrix(s.wr[,1:2]))/
  length(as.matrix(s.wr[,1:2]))				# (12) P(X)

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1))
plot(X, p.X, type="h", lwd=10, ylim=c(0,0.5),		# (13) X PDF
  ylab="f(X)", main="Distribution of X")
plot(X.bar, p.X.bar, type="h", lwd=10, ylim=c(0,0.3),
  ylab=expression(f(bar(X))), xlab=expression(bar(X)), 
  main=expression("Sampling Distribution of "*bar(X))) 	# (14) X_ PDF
par(op)

pop.var(s.wr.mean)					# (15) V(X_):pop.var 함수
sigma^2/n       					# (16) V(X_):공식적용				
sum((X.bar-e.Xbar)^2 * p.X.bar)				# (17) V(X_):Σ(X_-mu)^2P(X_)

mean(s.wr.var)						# E(S^2):mean 함수

#################################
# sampling without replacement
#################################
X.1 <- c(1,1,1,2,2,3)
X.2 <- c(2,3,4,3,4,4)
s.wor <- cbind(X.1, X.2)				# (1) 6개 표본 종류 생성
rownames(s.wor) <- paste("Sample",1:NROW(s.wor))	# (2) 표본의 이름 부여
s.wor.mean <- apply(s.wor, 1, mean)			# (3) 표본평균 계산
s.wor.var <- apply(s.wor, 1, var)			# (4) 표본분산 계산
s.wor <- cbind(s.wor, s.wor.mean, s.wor.var)		# (5) 데이터 병합
dimnames(s.wor)[[2]][c(3,4)] <- c("mean","var") 	# (6) 열 이름 지정
s.wor							# (7) 최종 데이터

mean(s.wor.mean)					# (8) E(X_):mean 함수
X.bar <- as.numeric(names(table(s.wor.mean)))		# (9) X_
p.X.bar <- table(s.wor.mean)/length(s.wor.mean) 	# (10) X_의 분포
(e.Xbar <- sum(X.bar*p.X.bar))				# (11) E(X_):ΣX_P(X_)
 
pop.var(s.wor.mean)					# (12) V(X_):pop.var 함수
(N-n)/(N-1)*(sigma^2/n)       				# (13) V(X_):공식적용
sum((X.bar-e.Xbar)^2 * p.X.bar)				# (14) V(X_):Σ(X_-mu)^2P(X_)

mean(s.wor.var)						# (15) E(S^2):mean 함수
N/(N-1)*sigma^2						# (16) E(S^2):공식적용



#################################
# 그림 4-3, 4-4
#################################
mu <- 0; sigma <- 10					# (1) X~N(0, 10^2)
n <- c(10, 50, 100)					# (2) 표본의 수
	
x <- sapply(n, FUN=function(n) {			# (3) 표본추출
  set.seed(0); rnorm(n, mu, sigma)})
x.var <- sapply(x, var)					# (4) 표본분산
names(x.var) <- c("n=10","n=50","n=100")
x.var

N <- 500						# (5) 실험회수
S.sq <- x.var						# (6) 표본분산 벡터
S.sq.mean <- numeric()					# (7) 표본분산 기대치 벡터
for (i in 2:N) {                                        # (8) 표본분산 평균 구하기 
  S.sq <- rbind(S.sq, sapply(n, FUN=function(n) {
  var(rnorm(n, mu, sigma))}))
  S.sq.mean <- rbind(S.sq.mean, apply(S.sq, 2, mean))
}

matplot(x=2:N, S.sq.mean, type="l", xlab="Experiment Counts",
  ylab=expression(E(S^2)), col=c(4,2,1), lty=3:1,
  main=expression(E(S^2)~"Plots"))
legend("topright", c("n=10","n=50","n=100"), lty=3:1, col=c(4,2,1))
abline(h=100)


f1 <- function(x) dnorm(x, mu, sigma/sqrt(10))
f2 <- function(x) dnorm(x, mu, sigma/sqrt(50))
f3 <- function(x) dnorm(x, mu, sigma/sqrt(100))

curve(f3, lwd=1.5, xlab=expression(bar(X)), 
  ylab=expression(f(bar(X))), xlim=c(-10,10),
  main=expression("Sampling Distribution of "*bar(X)))
curve(f2, add=T, col=2, lty=2, lwd=1.5)
curve(f1, add=T, col=4, lty=3, lwd=1.5)

legend(5, 0.35, c("n=10","n=50","n=100"), 
  lty=3:1, col=c(4,2,1), lwd=1.5)

#################################
# 그림 4-5, 4-6
#################################
n <- c(5, 10, 20, 50)
rep.cnt <- 100

simul.xbar <- function(n, rep.cnt) {
  xbar <- numeric(rep.cnt)
  for (i in 1:rep.cnt)
    xbar [i] <- mean(rexp(n))
  xbar
}

xbar <- mapply(simul.xbar, n, rep.cnt)

# Kernel Density Estimation Plot
simul.plot <- function(xbar, n) {
  plot(density(xbar), main=paste("Sample size =", n))
  f <- function(x) dnorm(x, 1, 1/sqrt(n))
  curve(f, add=T, col=2, lty=2)
}

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) simul.plot(xbar[,i], n[i])
par(op)

# Q-Q Plot
simul.qqnorm <- function(xbar, n) {
  quantile.seq <- seq(1, 199, 2)/200
  quantile.value <- qnorm(seq.quantile, mean=1, sd=1/n)
  order.xbar <- sort(xbar)
  
  plot(quantile.value, order.xbar, 
    main=paste("Q-Q Plot : Sample size =", n))

  y <- quantile(xbar, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75), mean=1, sd=1/n)
  
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  abline(int, slope)
}

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) simul.qqnorm(xbar[,i], n[i])
par(op)



#################################
# 예제 4-2
#################################

mu <- 250; sigma <- 25; n <-35			
x.bar.mu <- mu				 
x.bar.sigma <- sigma/sqrt(n)
pnorm(270, x.bar.mu, x.bar.sigma)-
  pnorm(255, x.bar.mu, x.bar.sigma)


#################################
# 예제 4-3
#################################
p <- 0.4; q <- 1-p				# p
n <- 35						# n
mu <- p; sigma <- sqrt(p*q/n)			# phat~N(p, p*q/n)
1- pnorm(0.5, mu, sigma)			# Q(0.5)
pnorm(0.6, mu, sigma)-pnorm(0.45, mu, sigma)	# P(0.45<phat<0.6)


#################################
# Chi-Square 분포
#################################
df.chisq <- 3				# (1) 자유도
x <- 0:3				# (2) x

pdf.chisq <- function(x, df) {		# (3) pdf 정의
  hdf <- df/2
  1/(2^(hdf)*gamma(hdf))*x^(hdf-1)*exp(-x/2)
}
  
pdf.chisq(x, df=df.chisq) 		# (4) pdf using pdf.chisq
dchisq(x, df=df.chisq)			# (5) dchisq 함수

sapply(x, function(x) {			# (6) F(x):integrate 함수 이용
  f <- function(x) pdf.chisq(x, df.chisq);  
  integrate(f, 0, x)$value})  	            
pchisq(x, df=df.chisq)			# (7) F(x):pchisq 함수

p <- 1:10/10
qchisq(p, df.chisq)			# (8) Quantile:qchisq 함수

n <- 10^(1:5)				# (9) for Random Sample
rnd.chisq <- (sapply(n, function(n) 
  {set.seed(1); rnd <- rchisq(n, df.chisq);
  rbind(mean(rnd), var(rnd))}))  	# (10) rchisq 함수
dimnames(rnd.chisq) <- list(c('E(X)','V(X)'),
  c('n=10','n=100','n=1000','n=10000','n=100000'))
rnd.chisq				# (11) 난수의 평균,분산


#################################
# Chi-Square 그림 4-7
#################################
df.chisq <- c(1, 5, 10, 20)		# 자유도

f1 <- function(x) dchisq(x, df.chisq[1])
f2 <- function(x) dchisq(x, df.chisq[2])
f3 <- function(x) dchisq(x, df.chisq[3])
f4 <- function(x) dchisq(x, df.chisq[4])

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(4, 4, 4, 1))
# 확률밀도함수 그래프
curve(f1, xlim=c(0,30), ylim=c(0, 0.2),col=1, lty=1, ylab="Density",
  main=expression(chi^2~~"Probability Density"))
curve(f2, add=T, col=2, lty=2)
curve(f3, add=T, col=3, lty=3)
curve(f4, add=T, col=4, lty=4)

legend(23, 0.2, col=1:4, lty=1:4, cex=0.8,
  c(expression(X*"~"*chi^2*(1)),expression(X*"~"*chi^2*(5)),
  expression(X*"~"*chi^2*(10)),expression(X*"~"*chi^2*(20))))

f1 <- function(x) pchisq(x, df.chisq[1])
f2 <- function(x) pchisq(x, df.chisq[2])
f3 <- function(x) pchisq(x, df.chisq[3])
f4 <- function(x) pchisq(x, df.chisq[4])

# 누적분포함수 그래프
curve(f1, xlim=c(0,30), ylim=c(0, 1),col=1, lty=1, ylab="Probability",
  main=expression(chi^2~~"Cumulative Distribution"))
curve(f2, add=T, col=2, lty=2)
curve(f3, add=T, col=3, lty=3)
curve(f4, add=T, col=4, lty=4)

legend(23, 0.6, col=1:4, lty=1:4, cex=0.8,
  c(expression(X*"~"*chi^2*(1)),expression(X*"~"*chi^2*(5)),
  expression(X*"~"*chi^2*(10)),expression(X*"~"*chi^2*(20))))
par(op)


#################################
# Chi-Square 그림 4-8
#################################
mu <- 3; sigma <- 2				# (1) X~N(3, 2^2)
n <- c(5, 10, 20, 30)				# (2) 표본의 수
rep.cnt <- 100					# (3) 실험 회수

simul.chisq <- function(n, rep.cnt) {     	# (4) 통계량 구하는 함수
  chisq <- numeric(rep.cnt)
  for (i in 1:rep.cnt) {
    rnd <- rnorm(n, mu, sigma)
    chisq [i] <- (n-1)*var(rnd)/sigma^2		# (5) (n-1)*S^2/sigma^2
  }
  chisq
}

chisq <- mapply(simul.chisq, n, rep.cnt)	# (6) 통계량 구하기

simul.plot <- function(chisq, n) {		# (7) 그래프 출력 함수
  ymax <- max(dchisq(0:(max(chisq)*100)/100, df=n-1))
  plot(density(chisq), xlim=c(0, max(chisq)+5), 
    ylim=c(0, ymax+ymax*0.1), xlab="",
    main=paste("Sample size =", n, ",df =", n-1))
  title(xlab=expression(over((n-1)*S^2, sigma^2)), line=4)
  f <- function(x) dchisq(x, df=n-1)
  curve(f, add=T, col=2, lty=2)
}

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2), oma=c(1,1,1,1))
for (i in 1:length(n)) 				# (8) 그래프출력함수 호출
  simul.plot(chisq[,i], n[i])
par(op)


#################################
# 예제 4-4
#################################
mu <- 3; sigma <- 2				# X~N(3, 2^2)
n <- 10						# sample size
df.chisq  <- n-1				# df
pchisq((n-1)*2/sigma^2, df=df.chisq)-
pchisq((n-1)*1.5/sigma^2, df=df.chisq) 		# 1.P(1.5<S^2<2)
sigma^2/(n-1)*qchisq(0.05, df=df.chisq)		# 2.P(S^2<c)=0.05
sigma^2/(n-1)*
  qchisq(0.05, df=df.chisq, lower.tail=F)	# 3.P(c<S^2)=0.05
sigma^2/(n-1)*
  qchisq(0.95, df=df.chisq)			# 3.P(c<S^2)=0.05

#################################
# 예제 4-5
#################################
mu <- 140; sigma <- sqrt(5)			# X~N(140, 5)
n <- 15						# sample size
df.chisq  <- n-1				# df
pchisq((n-1)*4/sigma^2, df=df.chisq) 		# 1.P(S^2<4)



#################################
# t-분포
#################################

df.t <- 3					# (1) 자유도
x <- -2:2					# (2) x

pdf.t <- function(x, df) {			# (3) pdf 정의
  gamma((df+1)/2)/(sqrt(pi*df)*gamma(df/2))*
    (1+x^2/df)^-((df+1)/2)
}

pdf.t(x, df=df.t) 				# (4) Density using pdf.t
dt(x, df=df.t)					# (5) dt 함수

sapply(x, function(x) {				# (6) F(x):integrate 함수 이용
  f <- function(x) pdf.t(x, df.t);  
  integrate(f, -Inf, x)$value})  	            
pt(x, df=df.t)					# (7) F(x):pt 함수

p <- 1:10/10
qt(p, df.t)					# (8) Quantile:qt 함수

n <- 10^(1:5)					# (9) for Random Sample
rnd.t <- (sapply(n, function(n) 
  {set.seed(1); rnd <- rt(n, df.t);
  rbind(mean(rnd), var(rnd))}))  		# (10) rt 함수
dimnames(rnd.t) <- list(c('E(X)','V(X)'),
  c('n=10','n=100','n=1000','n=10000','n=100000'))
rnd.t						# (11) 난수의 평균,분산


#################################
# t-분포 : 그림 4-9
#################################
df.t <- c(1, 2, 10)		# 자유도

f1 <- function(x) dt(x, df.t[1])
f2 <- function(x) dt(x, df.t[2])
f3 <- function(x) dt(x, df.t[3])
f4 <- function(x) dnorm(x)

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(4, 4, 4, 1))
# 확률밀도함수 그래프
curve(f1, xlim=c(-4,4), ylim=c(0, 0.5),col=1, lty=1, ylab="Density",
  main="t-Density Curve")
curve(f2, add=T, col=2, lty=2)
curve(f3, add=T, col=3, lty=3)
curve(f4, add=T, col=4, lty=4, lwd=2.5)

legend(2, 0.4, col=1:4, lty=1:4, cex=0.8, lwd=c(1,1,1,2.5),
  c("X~t(1)","X~t(5)","X~t(10)","X~N(0,1)"))

f1 <- function(x) pt(x, df.t[1])
f2 <- function(x) pt(x, df.t[2])
f3 <- function(x) pt(x, df.t[3])
f4 <- function(x) pnorm(x)

# 누적분포함수 그래프
curve(f1, xlim=c(-4,4), ylim=c(0, 1),col=1, lty=1, ylab="Probability",
  main="Cumulative t-Distribution")
curve(f2, add=T, col=2, lty=2)
curve(f3, add=T, col=3, lty=3)
curve(f4, add=T, col=4, lty=4, lwd=2.5)

legend(2, 0.6, col=1:4, lty=1:4, cex=0.8, lwd=c(1,1,1,2.5),
  c("X~t(1)","X~t(5)","X~t(10)","X~N(0,1)"))
par(op)


#################################
# t-분포 : 그림 4-10
#################################
mu <- 3; sigma <- 2			# (1) X~N(3, 2^2)
n <- c(3, 5, 10, 20)			# (2) 표본의 수
rep.cnt <- 100				# (3) 실험 회수

simul.t <- function(n, rep.cnt) {     	# (4) t-값을 구하는 함수
  t.val <- numeric(rep.cnt)
  for (i in 1:rep.cnt) {
    rnd <- rnorm(n, mu, sigma)
    t.val[i] <- (mean(rnd)-mu)/
      (sqrt(var(rnd))/sqrt(n))		# (5) (X_bar-μ)/sqrt(S^2/n)
  }
  t.val
}

t.val <- mapply(simul.t, n, rep.cnt)	# (6) t-값 구하기

simul.plot <- function(t.val, n) {	# (7) 그래프 출력 함수
  plot(density(t.val), xlim=c(-5,5), ylim=c(0, 0.45),
    main=paste("Sample size =", n, ",df =", n-1))
  f <- function(x) dt(x, df=n-1)
  curve(f, add=T, col=2, lty=2)
}

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2), oma=c(1,1,1,1))
for (i in 1:length(n)) 			# (8) 그래프출력함수 호출
  simul.plot(t.val[,i], n[i])
par(op)


#################################
# 예제 4-6
#################################
mu <- 3; sigma <- 2			# X~N(3, 2^2)
n <- 10					# sample cnt
df.t  <- n-1				# df

qt(0.05, df=df.t)/sqrt(n)		# 1.P(X_Bar<mu+cS)=0.05
qt(0.05, df=df.t, lower.tail=F)/sqrt(n)	# 2.P(mu+cS<X_Bar)=0.05
qt(0.95, df=df.t)/sqrt(n)		# 2.P(mu+cS<X_Bar)=0.05





#################################
# F-분포
#################################
df.F1 <- 3					# (1) 자유도1
df.F2 <- 5					# (1) 자유도2

x <- 0:3					# (2) x

pdf.F <- function(x, df1, df2) {		# (3) pdf 정의
  1/beta(df1/2, df2/2) * (df1/df2)^(df1/2) *
    x^(df1/2-1) * (1+df1/df2*x)^-((df1+df2)/2)
}

pdf.F(x, df1=df.F1, df2=df.F2)			# (4) pdf using pdf.F
df(x, df1=df.F1, df2=df.F2)			# (5) df 함수

sapply(x, function(x) {				# (6) F(x):integrate 함수 이용
  f <- function(x) pdf.F(x, df.F1, df.F2);  
  integrate(f, 0, x)$value})  	            
pf(x, df=df.F1, df2=df.F2)			# (7) F(x):pf 함수

p <- 1:10/10
qf(p, df.F1, df.F2)				# (8) Quantile:qf 함수

n <- 10^(1:5)					# (9) Sample size for Random Sample
rnd.F <- (sapply(n, function(n) 
  {set.seed(1); rnd <- rf(n, df.F1, df.F2);
  rbind(mean(rnd), var(rnd))}))  		# (10) rf 함수
dimnames(rnd.F) <- list(c('E(X)','V(X)'),
  c('n=10','n=100','n=1000','n=10000','n=100000'))
rnd.F						# (11) 난수의 평균, 분산
df.F2/(df.F2-2)					# (12) E(X)
(2*df.F2^2)*(df.F1+df.F2-2)/
  (df.F1*((df.F2-2)^2)*(df.F2-4))		# (13) V(X)


#################################
# F-분포 - t^2 : F
#################################
df <- 10
(p <- seq(1/2, .99, length=10))		# (1) 0.5≤p<1
(tsqure.q <- qt(p, df)^2)		# (2) T의 분위수의 제곱
(f.q <- qf(2*p - 1, df1=1, df2=df))	# (3) F의 2p-1 분위수
all.equal(tsqure.q, f.q)		# (4) same ?

(x <- seq(0.001, 5, len=10))
(f.p <- df(x^2, 1, 5))			# (5) density of F
(tsqure.p <- dt(x, 5)/x)		# (6) density of t²
all.equal(f.p, tsqure.p)		# (7) same


.Machine$double.eps
.Machine$double.eps ^ 0.5


######################################################
# F-분포 - F^-1(p; df1, df2) = 1/F^-1(1-p, df2, df1)
######################################################
df1 <- 5
df2 <- 7

p <- c(0.05, 0.1, 0.95)
qf(p, df1, df2)
1/qf(1-p, df2, df1)



#################################
# F-분포 : 그림 4-11
#################################
df1 <- c(3, 3, 20, 30)		# 자유도
df2 <- c(5, 10, 20, 30)

f1 <- function(x) df(x, df1[1], df2[1])
f2 <- function(x) df(x, df1[2], df2[2])
f3 <- function(x) df(x, df1[3], df2[3])
f4 <- function(x) df(x, df1[4], df2[4])

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(4, 4, 4, 1))
# 확률밀도함수 그래프
curve(f1, xlim=c(0,2), ylim=c(0, 1.3),col=1, lty=1, 
  ylab="Density",
  main="F-Density Curve")
curve(f2, add=T, col=2, lty=2)
curve(f3, add=T, col=3, lty=3)
curve(f4, add=T, col=4, lty=4)

legend(1.5, 1.2, col=1:4, lty=1:4, cex=0.8,
  c("X~F(3,5)","X~F(3,10)","X~F(20,20)","X~F(30,30)"))

f1 <- function(x) pf(x, df1[1], df2[1])
f2 <- function(x) pf(x, df1[2], df2[2])
f3 <- function(x) pf(x, df1[3], df2[3])
f4 <- function(x) pf(x, df1[4], df2[4])

# 누적분포함수 그래프
curve(f1, xlim=c(0,2), ylim=c(0,1),col=1, lty=1, ylab="P(X<=x)",
  main="Cumulative F-Distribution")
curve(f2, add=T, col=2, lty=2)
curve(f3, add=T, col=3, lty=3)
curve(f4, add=T, col=4, lty=4)

legend(1.5, 0.6, col=1:4, lty=1:4, cex=0.8,
  c("X~F(3,5)","X~F(3,10)","X~F(20,20)","X~F(30,30)"))
par(op)



#################################
# F-분포 : 그림 4-11
#################################
mu1 <- 3; sigma1 <- 2			# (1) X1~N(3, 2^2)
mu2 <- 2; sigma2 <- 3			# (2) X2~N(2, 3^2)

n1 <- c(4, 5, 10, 15)			# (3) 표본의 수 n1
n2 <- c(5, 10, 15, 30)			# (4) 표본의 수 n2
rep.cnt <- 100				# (5) 실험 회수

simul.F <- function(n1, n2, rep.cnt) {	# (6) 통계량 구하는 함수
  F.val <- numeric(rep.cnt)
  for (i in 1:rep.cnt) {
    rnd <- rnorm(n1, mu1, sigma1)
    chisq1 <- (n1-1)*var(rnd)/sigma1^2	# (7) V1=(n1-1)*S1^2/sigma1^2
    rnd <- rnorm(n2, mu2, sigma2)
    chisq2 <- (n2-1)*var(rnd)/sigma2^2	# (8) V2=(n2-1)*S2^2/sigma2^2
    F.val[i] <- (chisq1/(n1-1))/(chisq2/(n2-1))	# (9) (V1/v1)/(V2/v2)	
  }
  F.val
}

F.val <- mapply(simul.F, n1, n2, rep.cnt)	# (10) 통계량 구하기

simul.plot <- function(F.val, n1, n2) {	# (11) 그래프 출력 함수
  plot(density(F.val), xlim=c(0,4), ylim=c(0, 1),
    main=paste("Sample size =",n1,",",n2,",df =", n1-1,",",n2-1))
  f <- function(x) df(x, df1=n1-1, df2=n2-1)
  curve(f, add=T, col=2, lty=2)
}

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2), oma=c(1,1,1,1))
for (i in 1:length(n1)) 			# (12) 그래프출력함수 호출
  simul.plot(F.val[,i], n1[i], n2[i])
par(op)


#################################
# 예제 4-7
#################################
n <- 10
df1 <- df2 <- n-1
qf(0.95, df1, df2) * 4/9