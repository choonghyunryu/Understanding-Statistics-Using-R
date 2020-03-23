##########################################
# Binomial Distributions
##########################################

# Mass Function을 이용한 확률 계산
p <- 1/3; n <- 3; x <- 1
choose(n, x) * (p)^x * (1-p)^(n-x)         		# (1) Mass Function-1

p <- 1/3; n <- 3; x <- 0:3
choose(n, x) * (p)^x * (1-p)^(n-x)         		# (2) Mass Function-2

# dbinom 함수를 이용한 확률 계산
dbinom(1, size=n, prob=p)                  		# (3) dbinom 함수-1
(binom.pdf <- dbinom(0:n, size=n, prob=p)) 		# (4) dbinom 함수-2

# dbinom 함수와 cumsum 함수를 이용한 누적확률 계산
cumsum(binom.pdf)                          		# (5)  cumsum 함수 이용 

# pbinom 함수를 이용한 누적확률 계산
(binom.cdf <- pbinom(0:n, size=n, prob=p)) 		# (6) pbinom 함수

# 분위수 
(binom.quantile <- qbinom(1:10/10, size=n, prob=p)) 	# (7) qbinom 함수 

# 이항난수 발생
set.seed(1)
(binom.rnd <- rbinom(n=30, size=n, prob=p)) 		# (8) rbinom 함수
table(binom.rnd)

# 기대치와 분산
n * p                                       		# (9) E(X) 
mean(binom.rnd)                             		# (10) 이항난수 평균
n * p * (1-p)                               		# (11) V(X)
var(binom.rnd)                              		# (12) 이항난수 분산
cnt <- c(30, 3000, 3000000)                 		# (13) 난수 추출 개수
sapply(cnt, FUN=function(cnt)               		# (14) 추출 개수별 분산
   var(rbinom(n=cnt, size=n, prob=p)))


# 그림 3-2.  이항분포와 관련된 그림
# Binomial Distribution Chart
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2))
plot(0:n, binom.pdf, type="h", xlab="X",    		# (15) plot Probability
  ylab="Probability", main="Binomial(n=3, p=1/3)")
plot(c(0:n, 3.1), c(0,binom.cdf), type="S", lty=2,	# (16) CDF plot
  xlab="X", ylab="CDF", main="Binomial(n=3, p=1/3)")	# 파선으로 계단 그림
points(c(0:n), c(binom.cdf), pch=16, cex=1.2)		# 폐구간 (시작점)
points(c(0:n, 3.1), c(0, binom.cdf), cex=1.2)		# 개구간 (종료점)
x <- c(0, rep(1:3, each=2), 3.1)				
y <- rep(binom.cdf, each=2)
for (i in 1:length(x))					# 선분(개구간,폐구간)
  lines(x[(i*2-1):(i*2)], y[(i*2-1):(i*2)])
plot(c(0:n, 3.1), c(0,binom.cdf), type="n", 		# (17) Quantile plot
  xlab="X", ylab="CDF", main="Quantile")
arrows(-0.1, 1:10/10, binom.quantile, 1:10/10, length=0.1)  
hist(binom.rnd, main="Histogram of Binomial Sample") 	# (18) Histogram
par(op)


# 예제 3-2
n <- 15; p.1 <- 1/6; p.2 <- 3/6; p.3 <- 4/6 		# Parameter
n * p.1  			                      	# Solve 1
pbinom(5, n, p.2)		                       	# Solve 2
pbinom(10, n, p.3)-pbinom(9, n, p.3)        		# Solve 3-1
dbinom(10, n, p.3)		        		# Solve 3-2


# 그림 3-3.  확률 p에 따른 이항분포의 확률분포
binom.p1 <- dbinom(0:n, n, p.1)
binom.p2 <- dbinom(0:n, n, p.2)
binom.p3 <- dbinom(0:n, n, p.3)
prameter <- rep(c("p=1/6", "p=1/2", "p=2/3"), 
  each=n+1)
prameter <- ordered(prameter, 
  levels=c("p=2/3", "p=1/2", "p=1/6"))
X <- as.factor(rep(0:n, times=3))
pdata <- data.frame(prameter, X=X, prob=c(binom.p1, binom.p2, binom.p3))
library(lattice)
barchart(prob ~ X | prameter, data = pdata,
         main="Binomial Probability Chart by p",
         layout = c(1,3), xlab="X",
         ylab = "Probability; P(X=x)")


##########################################
# Poisson Distributions
##########################################
# poisson distribution with lambda=2
lambda <- 2
x <- 0:5
pois1 <- lambda^x/factorial(x)*exp(-lambda) 		# (1) Mass Function-1
pois2 <- dpois(x, lambda)                   		# (2) dpois 함수
pois1
pois2
cumsum(pois1)                               		# (3) cumsum 함수 이용
ppois(x, lambda)                            		# (4) ppois 함수
qpois(1:10/10, lambda)                      		# (5) qpois 함수 
sapply(10^(0:5), FUN=function(x)            		# (6) 난수 발생
  {set.seed(0); mean(rpois(x, lambda))})


# 그림 3-4. lambda에 따른 포아송분포의 확률분포
x <- 0:20
lambda <- c(1, 4, 10)
mat.pois <- sapply(lambda, 
  FUN=function(lambda) dpois(x, lambda))    		# (7) 확률 계산 
prameter <- rep(c("λ=1", "λ=4", "λ=10"),      
  each=NROW(mat.pois))                      		# (8) 모수 벡터
prameter <- ordered(prameter,               
  levels=c("λ=10" ,"λ=4", "λ=1"))             		# (9) ordered factor 변환
X <- as.factor(rep(x, times=3))             		# (10) 확률변수 값
pdata <- data.frame(prameter, X=X,         
  prob=as.vector(mat.pois))                 		# (11) 최종 데이터 생성
library(lattice)                            		# (12) Lattice 패키지 로드
barchart(prob ~ X | prameter, data = pdata, layout = c(1, 3),
         main=expression("Poisson Probability Chart by"~~lambda),
         xlab="X", ylab="Probability; P(X=x)") 		# (13) Poisson 확률 그래프

# 예제 3-3
n <- 20 * 650
p <- 0.00035
lambda <- n * p
ppois(5, lambda)

# 예제 3-4
lambda <- 6
1 - ppois(2, lambda)                            # 1                       
dpois(7, lambda)                                # 2


##########################################
# Hyper-Geometric Distributions
##########################################
N <- 100; m <- 10; n <- 5
x <- 0:min(m, n)
hg1 <- choose(m, x)*choose(N-m, n-x)/choose(N, n) 	# (1) Mass Function-1
hg2 <- dhyper(x, m, N-m, n)                       	# (2) dhyper() 함수
hg1
format(hg1, scientific=F, digit=3)         		# (3) format() 함수 
format(hg2, scientific=F, digit=3)                 
cumsum(hg1)                                		# (4) cumsum() 함수 이용
phyper(x, m, N-m, n)                       		# (5) hyper() 함수
qhyper(1:10/10, m, N-m, n)                 		# (6) qhyper() 함수
p <- m/N
n * p                                      		# (7) E(X)
n * p * (1-p) * (N-n)/(N-1)                		# (8) V(X)
sapply(10^(0:5), FUN=function(x)           		# (9) 난수 발생
  {set.seed(1); rnd <- rhyper(x, m, N-m, n); 
   data.frame(E=mean(rnd), V=var(rnd))})


# 예제 3-5
N <- 40; m <- 25; n <- 15
x <- 6
phyper(x, m, N-m, n)


##########################################
# 그림 3-5
##########################################
op <- par(no.readonly=TRUE)
par(mar=c(0, 0, 0, 0))
plot(1:10, type="n", xlab="", ylab="", xlim=c(2, 5), ylim=c(0, 10), axes=F)
rect(2, 8, 4, 10)
text(3,9, "초기하분포"~~"X~HG(N,m,n)")
arrows(3, 8, 3, 6,length=0.15)
text(4.5, 7, expression(list("N" %->% infinity,~~(over(m,N)==p))), cex=1.2)
rect(2, 4, 4, 6)
text(3, 5, "이항분포"~~"X~B(n,p)")
arrows(3, 4, 3, 2,length=0.15)
text(4.5, 3, expression(list("n" %->% infinity,~~(np==lambda))), cex=1.2)
rect(2, 0, 4, 2)
text(3, 1, expression(paste("포아송분포"~~"X~Poi",(lambda))))
par(op)


##########################################
# Hyper-Geometrix-->Binomial-->Poisson
##########################################
N <- 100; m <- 30; n <- 5
x <- 0:n

eps <- 5

round(p.hyper <- dhyper(x, m, (N-m), n), eps)    	# (1) 초기하분포 
round(p.binom <- dbinom(x, n, m/N), eps)         	# (2) 이항분포
round(p.pois  <- dpois(x, n*m/N), eps)           	# (3) 포아송분포

N.times <- n.times <- 10^(0:5)                   

p.hyper <- sapply(N.times, FUN=function(N.times) 	# (4) 초기하분포-1        
  dhyper(x, m*N.times, (N-m)*N.times, n))
dimnames(p.hyper)=list(paste("X=", x, sep=""),    # (5) 차원이름 할당
  paste("N*", N.times, sep=""))
round(p.binom, eps) - round(p.hyper, eps)        	# (6) (2)-(4)


p.binom <- sapply(n.times, FUN=function(n.times)         
  dbinom(x, n*n.times, m/N/n.times))             	# (7) 이항분포-1
dimnames(p.binom)=list(paste("X=", x, sep=""), 
  paste("n*", n.times, sep=""))                   # (8) 차원이름 할당
round(p.pois, eps) - round(p.binom, eps)         	# (9) (3)-(7)

##########################################
# 그림 3-5
##########################################
np <- 5
x <- 0:15
plot(x, dbinom(x,10,np/10), type="o", 
  xlab="X", ylab="P(X=x)", pch=15,
  main="이항분포의 포아송 근사; B(n,p), np=5")
points(x, dbinom(x, 20, np/20), pch=16, col=2)
lines(x, dbinom(x, 20, np/20), lty=2, col=2)
points(x, dbinom(x, 50, np/50), pch=17, col=3)
lines(x, dbinom(x, 50, np/50), lty=3, col=3)
points(x, dbinom(x, 100, np/100), pch=18, col=4)
lines(x, dbinom(x, 100, np/100), lty=4, col=4)
points(x, dpois(x, np), pch=10, col=5)
lines(x, dpois(x, np), lty=5, col=5)
legend(9, 0.2, lty=1:5, col=1:5, pch=c(15:18, 10), 
  legend=c("X~B(n=10,p=0.5)", "X~B(n=20,p=0.25)", 
  "X~B(n=50,p=0.1)", "X~B(n=100,p=0.05)",
  expression("X~Poi("*lambda*"=5)")))


##########################################
# Uniform Distributions
##########################################
a <- -3; b <- 4  			          	# (1) X~U(-3,4)
x <- c(-5, 2.5, 7)			         	# (2) X={-5, 2.5, 7}

ifelse(b>=x & x>=a, 1/(b-a), 0)		# (3) Density Function
dunif(x, a, b)				           	# (4) dunif 함수

ifelse(b>x & x>=a, 1/(b-a)*(x-a), 
  ifelse(x<a, 0, 1))			       	# (5) Distribution Function
punif(x, a, b)				           	# (6) punif 함수

1-ifelse(b>x & x>=a, 1/(b-a)*(x-a), 
  ifelse(x<a, 0, 1))			       	# (7) 1-F(a)
1-punif(x, a, b)				     	    # (8) punif 함수

p <- 0:10/10				             	# (9) p point
p*(b-a)+a					       	        # (10) p*(b-a)+a
qunif(p, a, b)				           	# (11) qunif 함수

n <- 10^(1:5)
rnd.uf <- sapply(n, FUN=function(n)# (12) 일양난수의 평균과 분산         
  {set.seed(0); rnd <- runif(n, a, b); 
   rbind(mean(rnd), var(rnd))})
dimnames(rnd.uf) <- list(c('E(X)', 'V(X)'),
  c('n=10', 'n=100', 'n=1000', 'n=10000', 'n=100000'))
rnd.uf

(b+a)/2					                 	# (13) E(X)
(b-a)^2/12					       	      # (14) V(X)

##########################################
# Uniform Distributions - 예제 3-6
##########################################
a <- 0; b <- 7  			          	# X~U(0, 7)
1/(b-a) * (5-2)				         	  # 1.P(2<=X<=5)
punif(5, a, b)-punif(2, a, b)		  # 1.P(2<=X<=5)
1/(b-a) * (b-6)				         	  # 2.P(X>=6)
1-punif(6, a, b)				          # 2.P(X>=6)
1/(b-a) * (2-a)				         	  # 3.P(X<=2)
punif(2, a, b)				            # 3.P(X<=2)



##########################################
# Exponential Distributions - Chart 3-9
##########################################
x <- 0:50/10
lambda <- 1:3/2

op <- par(no.readonly=TRUE)
par(mfrow=c(1,2), mar=c(4, 4, 4, 1))
pdf.exp <- sapply(lambda, FUN=function(lambda)         
  dexp(x, lambda))
matplot(x, pdf.exp, type="l", xlab="x", ylab="f(x)",
  main="Probability Density Function", lwd=2)
legend(2.5, 1.2, legend=c(expression(lambda==0.5),expression(lambda==1),
  expression(lambda==1.5)), lty=1:3, col=1:3, lwd=2)

cdf.exp <- sapply(lambda, FUN=function(lambda)         
  pexp(x, lambda))
matplot(x, cdf.exp, type="l", xlab="x", ylab=expression(P(X<=x)),
  main="Cumulative Distribution Function", lwd=2)
legend(2.5, 0.5, legend=c(expression(lambda==0.5),expression(lambda==1),
  expression(lambda==1.5)), lty=1:3, col=1:3, lwd=2)
par(op)


##########################################
# Exponential Distributions 
##########################################
lambda <- 3  					    	# (1) X~Exp(3)
x <- -1:3						# (2) X={-1,0,1,2,3}
p <- 1:10/10					  	# (3) for Quantiles
n <- 10^(1:5)					      	# (4) for Random Sample

ifelse(x<0, 0, lambda*exp(-lambda*x))	           	# (5) Density Function
dexp(x, lambda)					     	# (6) dexp 함수
ifelse(x<0, 0, 1-exp(-lambda*x))		       	# (7) Distribution Function
pexp(x, lambda)					       	# (8)	pexp 함수
-log(1-p)/lambda					# (9) 분포함수의 역함수
qexp(p, lambda)						# (10) qexp 함수
rnd.exp <- (sapply(n, function(n) 
  {set.seed(1); rnd <- rexp(n, lambda);
  rbind(mean(rnd), var(rnd))}))  	               	# (11) rexp 함수
dimnames(rnd.exp) <- list(c('E(X)','V(X)'),
  c('n=10','n=100','n=1000','n=10000','n=100000'))
rnd.exp						   	# (12) 난수의 평균,분산

1/lambda						# (13) E(X)
1/lambda^2						# (14) V(X)


##########################################
# Exponential Distributions - 예제 3-7
##########################################
lambda <- 1/1000   		                   	# X~Exp(1/1000)
1-(1-exp(-lambda*1200))		                       	# 1.P(X>=1200)
1-pexp(1200, lambda)		                     	# 1.P(X>=1200)
pexp(1200, lower.tail=F, lambda)                  	# 1.P(X>=1200)
(1-exp(-lambda*800))			          	# 2.P(X<=800)
pexp(800, lambda)				       	# 2.P(X<=800)
(1-exp(-lambda*900))-(1-exp(-lambda*500))	       	# 3.P(500<=X<=900)
pexp(900, lambda)-pexp(500, lambda)	             	# 3.P(500<=X<=900)


##########################################
# Exponential-Poisson Distributions 예제 3-8
##########################################
# Exponential Distribution
lambda <- 1/(5/2)                                 	# lambda(평균회수)
x <- 30/60  			                   	# 경과시간 x=30초=0.5분
pexp(x, lambda)                                   	# P(X<=x)

# Poisson Distribution
lambda <- (2/5)*(30/60)		                       	# lambda(단위시간당 발생수)
ppois(0, lambda, lower.tail=F)                    	# P(X>x)
1-ppois(0, lambda)                                	# P(X>x)

##########################################
# Normal Distributions
##########################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2,1))
# Chart 1 ==> 그림 3-10
# X1~N(2,1^2), X2~N(2, 2^2) 
mu1 <- mu2 <- 2						# μ1 == μ2
sigma1 <- 1; sigma2 <-2					# σ1 <> σ2

f1 <- function(x)
   dnorm(x, mean=mu1, sd=sigma1)			# f(x; μ1, σ1)
f2 <- function(x)
   dnorm(x, mean=mu2, sd=sigma2)			# f(x; μ2, σ2)

curve(f1, xlim=c(mu2-3*sigma2, mu2+3*sigma2),
  ylab="Density", main=expression(list(mu[1]==mu[2], 
  sigma[1]!=sigma[2])))					# X1 Density Curve
curve(f2, add=T, col=2, lty=2)				# X2 Density Curve
abline(v=mu1, col=4, lty=3, lwd=1)			# Mean(μ1 == μ2)
legend(4, 0.35, legend=c(expression(X[1]*"~N(2,1)"),
  expression(X[2]*"~N(2,4)")), lty=1:2, col=1:2)	# Legend

# Chart 2 ==> 그림 3-10
# X1~N(0,2^2), X2~N(2, 2^2) 
mu1 <- 0; mu2 <- 2					# μ1 <> μ2
sigma1 <- sigma2 <-2					# σ1 == σ2

curve(f1, xlim=c(mu1-3*sigma2, mu2+3*sigma2),
  ylab="Density", main=expression(list(mu[1]!=mu[2], 
  sigma[1]==sigma[2]))) 				# X1 Density Curve
curve(f2, add=T, col=2, lty=2)				# X2 Density Curve
abline(v=mu1, col=1, lty=3, lwd=1)			# Mean(μ1)
abline(v=mu2, col=2, lty=3, lwd=1)			# Mean(μ2)
legend(4.5, 0.16, legend=c(expression(X[1]*"~N(0,4)"), 
  expression(X[2]*"~N(2,4)")), lty=1:2, col=1:2)	# Legend
par(op)

# Chart 3 ==> 그림 3-11
# X~N(0,1^2)
x.mu <- 0; x.sigma <- 1					# X~N(0, 1^2)
set.seed(0)
x <- rnorm(500, x.mu, x.sigma)				# 표준정규분포 난수발생
a <- 1; b <- 2						# a, b
y <- a + b*x						# Y=a+bX
hist(x, probability=T, xlim=c(-5,8), col="lightblue",
  main=expression(Y==a+bX*"~"*list(N(a+b*mu,b^2*sigma^2)))) # 히스토그램 X
f <- function(x)								
   dnorm(x, a+b*x.mu, b*x.sigma)			# f(x; a+bμ, b^2σ2^2)	
hist(y, probability=T, add=T, col="mistyrose",
    main="Histogram of Y=a+bX")				# 히스토그램 Y
curve(dnorm, add=T, col=4, lwd=1.5)			# N(0,1) Density Curve
curve(f, add=T, col=2, lwd=1.5)				# N(a+bμ,b^2σ2^2) Density Curve
arrows(3.4, 0.1, 4, 0.1, 
  col="blue", code=2, length=0.1, lwd=1.8)
text(4, 0.1, pos=4,
  expression(list(N(a+b*mu,b^2*sigma^2))))



# Chart 4 ==> 그림 3-12
# X1~N(20,3^2), X2~N(60, 4^2) 
mu1 <- 20; sigma1 <- 3					# X1~N(20, 3^2)
mu2 <- 40; sigma2 <- 4					# X2~N(40, 4^2)
set.seed(0)
x1 <- rnorm(500, mu1, sigma1)
set.seed(0)
x2 <- rnorm(500, mu2, sigma2)
y <- x1+x2
fy <- function(x)								
   dnorm(x, mu1+mu2, sigma1+sigma2)			# f(x;20+40,3^2+4^2)
hist(x1, probability=T, xlim=c(0,90), col="lightblue",
  main=expression(X[1]+X[2]*"~"*
  N(list(mu[1]+mu[2],sigma[1]^2+sigma[2]^2))))		# 히스토그램 X1
hist(x2, probability=T, add=T, col="mistyrose")		# 히스토그램 X2
hist(y, probability=T, add=T, col="lavender")		# 히스토그램 Y
curve(fy, add=T, lwd=1.5)				# Curve N(20+40,3^2+4^2)


mu <- 3; sigma <- 2  				     	# (1) X~N(3, 2^2)
x <- -4:4						# (2) x={-4:4}
pdf.norm <- function(x, mu=0, sigma=1)            	# (3) pdf
  1/sqrt(2*pi*sigma^2)*exp(-(x-mu)^2/(2*sigma^2))
pdf.norm(x, mu, sigma) 	                         	# (4) pdf 공식 이용
dnorm(x, mu, sigma)					# (5) dnorm 함수 
sapply(x, function(x) {
  f <- function(x) pdf.norm(x, 3, 2);  
  integrate(f, -Inf, x)$value})  	               	# (6) cdf 계산에 integrate 함수 이용
pnorm(x, mu, sigma)					# (7) cdf 계산에  pnorm 함수 이용
p <- 1:10/10
qnorm(p, mu, sigma)					# (8) Quantile 계산에 qnorm 함수 이용
n <- 10^(1:5)					     	# (9) Sample Size for Random Sample
rnd.norm <- (sapply(n, function(n) 
  {set.seed(1); rnd <- rnorm(n, mu, sigma);
  rbind(mean(rnd), var(rnd))}))  	              	# (10) rnorm 함수
dimnames(rnd.norm) <- list(c('E(X)','V(X)'),
  c('n=10','n=100','n=1000','n=10000','n=100000'))
rnd.norm						# (11) 난수의 평균,분산

z <- (x-mu)/sigma   				    	# (12) 표준화
dnorm(z, 0, 1)						# (13) 밀도-1
dnorm(x, mu, sigma)  					# (14) 밀도-2

pnorm(z)						# (15) 누적확률-1
pnorm(x, mu, sigma)					# (16) 누적확률-2
sapply(z, function(x) 
  integrate(dnorm, -Inf, x)$value)			# (17) 누적확률-3
sapply(z, function(x) 
  integrate(pdf.norm, -Inf, x)$value)		     	# (18) 누적확률-4

qnorm(p)*sigma+mu					# (19) 분위수-1
qnorm(p, mu, sigma)					# (20) 분위수-2

# Normal Distributions - Chart 3-13
###################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 1))
pdf <- function(x)           
  1/sqrt(2*pi*sigma^2)*exp(-(x-mu)^2/(2*sigma^2))

curve(pdf, xlim=c(mu-3.5*sigma, mu+3.5*sigma), ylim=c(0, 0.4),
  ylab="Density", xlab="x or z",
  main="Density Curve")
curve(dnorm, add=T, col=2, lty=2)
points(x, dnorm(x, mu, sigma), pch=16, col=1)
points(z, dnorm(z), pch=17, col=2)

legend(6, 0.35, lty=1:2, col=1:2, pch=16:17, 
  legend=c("X~N(3,4)", "Z~N(0,1)"))

cdf <- function(x)           
  pnorm(x, mu, sigma)

curve(cdf, xlim=c(mu-3.5*sigma, mu+3.5*sigma), ylim=c(0, 1), 
  ylab="Cumulative Probability", xlab="x or z",
  main="Cumulative Probability Curve")
curve(pnorm, add=T, col=2, lty=2)
points(x, pnorm(x, mu, sigma), pch=16, col=1)
points(z, pnorm(z), pch=17, col=2)

legend(6, 0.75, lty=1:2, col=1:2, pch=16:17, 
  legend=c("X~N(3,4)", "Z~N(0,1)"))
par(op)




# Cumulate Probability & Quantile
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1     	# (1) erf 함수
1/2*(1+erf(z/sqrt(2)))					# (2) φ(X)
1/2*(1+erf((x-mu)/(sigma*sqrt(2))))			# (3) F(X)

erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)   	# (4) erf 역함수
sqrt(2)*erfinv(2*p-1)					# (5) z_p
sqrt(2)*erfinv(2*p-1)*sigma+mu			      	# (6) X_p


###################################
# Normal Distributions - Chart 3-14
###################################
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
# Chart 3-15
# Chart 1
norm.pdf <- dnorm(x, mu, sigma)
f <- function(x) dnorm(x, mu, sigma)
curve(f, type="l", xlim=c(mu-3.5*sigma, mu+3.5*sigma), lty=2,
  xlab="X", ylab="Density", 
  main=expression("N("~~mu~~ "= 3," ~~ sigma^2 ~~ "= 4 )"))
curve(f, add=T, type="l", xlim=c(-4, 4), col=4, lwd=1.5)
points(x, norm.pdf, col=4, pch=16, cex=1.2)
# Chart 2
norm.cdf <- pnorm(x, mu, sigma)
f <- function(x) pnorm(x, mu, sigma)
curve(f, type="l", xlim=c(mu-3.5*sigma, mu+3.5*sigma), lty=2,
  xlab="X", ylab="CDF", main="pnorm")
curve(f, add=T, type="l", xlim=c(-4, 4), col=4, lwd=1.5)
points(x, norm.cdf, col=4, pch=16, cex=1.2)
# Chart 3
p <- 1:30/30
norm.quantile <- qnorm(p, mu, sigma)
plot(norm.quantile, p, xlab="quantile", ylab="CDF", 
  main="qnorm", pch=16)
# Chart 4
set.seed(1)
norm.sample <- rnorm(30, mu, sigma)
qqnorm(norm.sample)
qqline(norm.sample, col=2)
par(op)


# 예제 3-9)
mu <- 175; sigma <- 3					# (1) X~N(175, 3^2)
x <- 177						# No.1 Argument
p <- 0.2				      		# No.2 Argument

(1- pnorm(x, mu, sigma))				# (2) 1. 1-P(x) 
(qx <- pnorm(x, mu, sigma, 
  lower.tail=F))	                  		# (3) 1. Q(x)
(qp <- qnorm(p, mu, sigma))				# (4) 2. P(x)=0.2, x=?

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1),mar=c(5, 4, 1, 1))
## (5) No.1 Chart
f <- function(x)
  dnorm(x, mean=mu, sd=sigma)
curve(f, xlim=c(mu-3*sigma,mu+3*sigma),
  main="Cumulative Probability")
for (i in seq(x,mu+3*sigma,length=500))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")
abline(v=x, col=2, lty=3, lwd=1)
y <- dnorm(x, mu, sigma)
arrows(x, y, mu+3*sigma, y, 
  col="red", code=2, length=0.1, lwd=1.8)
text(x+3, y+0.01, "Q(x)=P{X>177}", cex=0.8)
text(x+3, y-0.01, round(qx, 5), cex=0.8)

## (6) No.2 Chart
curve(f, xlim=c(mu-3*sigma,mu+3*sigma),
  main="Quantile Point")
for (i in seq(mu-3*sigma, qp,length=500))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")
abline(v=qp, col=2, lty=3, lwd=1)
y <- dnorm(qp, mu, sigma)
arrows(qp, y, mu-3*sigma, y, 
  col="red", code=2, length=0.1, lwd=1.8)
text(qp-3, y+0.01, "P{X<x}=20%", cex=0.8)
text(qp-3, y-0.01, round(qp,2), cex=0.8)
par(op)
 

# 예제 3-10)
# No.1 P(-3<Z<3)
2 * pnorm(3) - 1					# A(x) = 2*P(x)-1
1 - (pnorm(-3) + pnorm(3, lower.tail=F))  		# P(a<X<b)=전확률-(P(a)+Q(b))
1 - (pnorm(-3) + pnorm(-3))  				# P(a<X<b)=전확률-(P(a)+P(-b))
pnorm(3) - pnorm(-3) 					# P(a<X<b)=P(b)-P(a)

# No.2 P(Z<-1.64)+P(Z>1.64)
pnorm(-1.64)+pnorm(1.64, lower.tail=F)

# No.3 P(|Z|<z)=0.9 
qnorm(0.95)						# P(X)=P(Z<z)=0.95				
qnorm(0.05,lower.tail=F)				# Q(X)=P(Z>z)=0.05

# No.4 X~N(3, 2^2), P{2<X<5} 
mu <- 3; sigma <- 2					# X~N(3, 2^2)
a <- 2; b <- 5						# a, b
z.a <- (a-mu)/sigma					# 표준화
z.b <- (b-mu)/sigma					# 표준화
pnorm(z.b) - pnorm(z.a)					# P(a.z<Z<b.z)
pnorm(b, mu, sigma) - pnorm(a, mu, sigma) 		# P(a<X<b)


###################################
# Normal Distributions - Chart 3-16
###################################
p <- 0.3
n <- c(5, 10, 20, 30)
x.p <- sapply(n, FUN=function(n) dbinom(0:40, n, p))

matplot(0:40, x.p, xlim=c(0, 20), type="p", pch=15:18, xlab="X", 
  ylab="Probability", main="Binomial Approximation to Normal by n")
legend(10, 0.35, col=1:4, pch=15:18, 
   legend=c("X~B(n=5, p=0.3)", "X~B(n=10, p=0.3)", 
   "X~B(n=20, p=0.3)", "X~B(n=30, p=0.3)"))

f5 <- function(x) dnorm(x, 5*p, sqrt(5*p*(1-p)))
f10 <- function(x) dnorm(x, 10*p, sqrt(10*p*(1-p)))
f20 <- function(x) dnorm(x, 20*p, sqrt(20*p*(1-p)))
f30 <- function(x) dnorm(x, 30*p, sqrt(30*p*(1-p)))

curve(f5, add=T, lty=1, col=1)
curve(f10, add=T, lty=2, col=2)
curve(f20, add=T, lty=3, col=3)
curve(f30, add=T, lty=4, col=4)

legend(10, 0.25, lty=1:4, col=1:4, legend=c("X~N(5*0.3, 5*0.3*0.7)",
  "X~N(10*0.3, 10*0.3*0.7)", "X~N(20*0.3, 20*0.3*0.7)", 
  "X~N(30*0.3, 30*0.3*0.7)"))