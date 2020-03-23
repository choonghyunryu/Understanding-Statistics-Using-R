#################################
# 예제 8-1
#################################
lamp <- c(2600, 4000, 5600, 7000, 7700, 4500, 8100, 3500, 5000, 5630)
lamp <- lamp[lamp != 5000]
lamp
(n <- length(lamp))
(S <- length(lamp[lamp > 5000 ]))          # (1) S_0
(pvalue <- sum(dbinom(S:n, n, prob=0.5)))  # (2) P(S >= S_0)
(pvalue <- 1 - pbinom(S-1, n, prob=0.5))   # (3) 1 - P(S <= S_0)

#################################
# 예제 8-2
#################################
sw <- iris$Sepal.Width
theta <- 3
ifelse(sw==theta, "0", ifelse(sw>theta, "+", "-"))
(sw <- sw[sw != theta])
(n <- length(sw))
(S <- length(sw[sw > theta]))
(z0 <- (S-n/2)/sqrt(n/4))               # (1) Z_0
z <- abs(z0)                            # (2) |Z_0|
pnorm(-z) + pnorm(z, lower.tail=F)       # (3) P(Z<-z)+P(Z>z)
  2 * pnorm(-z)                         # (4) 2 * P(Z<-z)
2 * pnorm(z, lower.tail=F)              # (5) 2 * P(Z>z)
2 * ifelse(z0<=0, pnorm(z0), 
             pnorm(z0, lower.tail=F))   # (6) 2 * (P(Z<-z) 또는 P(Z>z))


#################################
# 예제 8-3
#################################
lamp <- c(2600, 4000, 5600, 7000, 7700, 4500, 8100, 3500, 5000, 5630)
lamp <- lamp[lamp != 5000]
n <- length(lamp)
(Z <- lamp - 5000)
(Rplus <- rank(abs(Z)))                # (1) R^+
(indicator <- (ifelse(Z>0, 1, 0)))     # (2) indicator
(Wplus <- sum(Rplus * indicator))      # (3) 검정통계량
1 - psignrank(Wplus-1, n=n)            # (4) 1-P(W^+ < =w) 
psignrank(Wplus-1, n=n, lower.tail=F)  # (5) P(W^+ >= w)

# 그림 8-1
dsignrank(0:45, 9) * 2^9
plot(0:45, dsignrank(0:45, 9), xlab="x", ylab="density", type="h")
title("Distribution of Signed-Rank Statistic when n = 9") 


#################################
# 예제 8-4
#################################
n <- 9
Wplus <- 28 - 1/2                     # (1) 연속항 수정   
(mu <- n*(n+1)/4 )                    # (2) E(W^+) 
(V <- n*(n+1)*(2*n+1)/24)             # (3) V(W^+)
(z <- (Wplus - mu)/sqrt(V))           # (4) z
pnorm(z, lower.tail=F)                # (5) p-value

#################################
# 예제 8-5
#################################
lamp <- c(2600, 4000, 5600, 7000, 7700, 4500, 8100, 3500, 5000, 5630)
lamp[lamp==4500] <- 4400               # (1) 6번째 값 변경
(lamp <- lamp[lamp != 5000])
n <- length(lamp)
(Z <- lamp - 5000)
(Rplus <- rank(abs(Z)))                # (2) R^+
(indicator <- (ifelse(Z>0, 1, 0)))     # (3) indicator
(Wplus <- sum(Rplus * indicator))      # (4) 검정통계량
1 - psignrank(Wplus, n=n)              # (5) 1-P(W^+ <=w_0) 
psignrank(Wplus, n=n, lower.tail=F)    # (6) P(W^+ >= w_0)

#################################
# 예제 8-6
#################################
lamp <- c(2600, 4000, 5600, 7000, 7700, 4400, 8100, 3500, 5000, 5630)
lamp <- lamp[lamp != 5000]
n <- length(lamp)
abs.Z <- abs(lamp - 5000)
(tie <- table(abs.Z))                  # (1) 동점 개수 구하기
tie*(tie-1)*(tie+1)
(sum.tie <- sum(tie*(tie-1)*(tie+1)))  # (2) Σti*(ti-1)*(ti+1)
(mu <- n*(n+1)/4)                      # (3) E(W^+)
(V <- n*(n+1)*(2*n+1)/24 - sum.tie/48) # (4) V(W^+)
(z <- (27.5 - mu)/sqrt(V))             # (5) z
pnorm(z, lower.tail=F)                 # (6) p-value
(z1 = (27 - mu)/sqrt(V))               # (7) z (연속항 수정)
pnorm(z1, lower.tail=F)                # (8) p-value


lamp <-c(2600, 4000, 5600, 7000, 7700, 4400, 8100, 3500, 5000, 5630)
lamp <- lamp[lamp != 5000]
Rplus <- rank(abs(lamp-5000))          # (1) R^+

c1 <- c(0, Rplus[1])                   
for(i in 2:9){
    c1 <- c(c1, c1+Rplus[i])
}

(tab1 = table(c1))                     # (2) 도수분포
tab1[as.numeric(names(tab1)) >= 27.5]  # (3) 27.5 이상인 경우
sum(tab1[as.numeric(names(tab1)) >= 27.5]) / 2^9 # (4) 27.5 이상일 확률 

# (5) 27.5 이상일 확률의 두 번째 방법
psi <- expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
Rpsi <- t(psi) * Rplus
sum.Rpsi <- apply(Rpsi, 2, sum)
tab2 <- table(sum.Rpsi)
sum(tab2[as.numeric(names(tab2)) >= 27.5]) / 2^9

######################################
# 그림 8-2
######################################
x <- as.numeric(names(tab1))
y <- as.numeric(tab1)/ 2^9
plot( x, y, xlab = "x", ylab = "probability", type="h")
title(" Distribution of Wplus when ties do exist")

# 예제 8-1 데이터 이용
dat1 <- c(2600, 4000, 5600, 7000, 7700, 4500, 8100, 3500, 5000, 5630)
wilcox.test(dat1, alternative="greater", mu=5000)
dat1 <- dat1[dat1 != 5000]
wilcox.test(dat1, alternative="greater", mu=5000)

# 예제 8-3 데이터 이용
dat2 <- c(2600, 4000, 5600, 7000, 7700, 4400, 8100, 3500, 5000, 5630)
wilcox.test(dat2, alternative="greater", mu=5000)
wilcox.test(dat2, alternative="greater", mu=5000, correct=F)
(dat2 <- dat2[dat2 != 5000])
wilcox.test(dat2, alternative="greater", mu=5000)

#############################
# 8.2. 이표본 위치문제
#############################

# 8.2.1 윌콕슨 순위합 검정
dwilcox(3, m=6, n=5)
3/ choose(11, 6)

######################################
# 그림 8-3 
######################################
x <- 0:30
y <- dwilcox(x, m = 6, n = 5)
x <- x + 21
plot(x, y, ylab = "probability", type = "h")
title("P(W = x) for Wilcoxon rank sum statistic W ")


#################################
# 예제 8-7
#################################
set.seed(1)
(x <- round(rexp(5, 1/5000)))
set.seed(1)
(y <- round(rexp(6, 1/7000)))

n <- length(y)
m <- length(x)
(data <- c(x=x, y=y))                      # (1) 혼합표본   
idx.y <- grep("^y", names(rank(data)))     # (2) Y 표본 위치 
(W <- sum(rank(data)[idx.y]))              # (3) W 통계량
(U <- W - n*(n+1)/2)                       # (4) U 통계량
(p.value <- (1 - pwilcox(U-1, m, n))*2)    # (5) p-value 
(p.value <- sum(dwilcox((U):51 , m, n))*2) # (6) p-value


#################################
# 그림 8-4
#################################
dexp5000 <- function(x) { dexp(x, rate=1/5000)}
dexp7000 <- function(x) { dexp(x, rate=1/7000)}
curve(dexp5000, from=0, to=20000, ylab="density")
curve(dexp7000, from=0, to=20000, add=T, lty=2)
legend(15000, 1/5000, legend=c("Exp(5,000)", "Exp(7,000)"), lty=1:2)
title("Densities of two Exponential Distribution")
lines(c(5000,5000), c(0, dexp5000(5000)))
lines(c(7000,7000), c(0, dexp7000(7000)), lty=2)
text(4200, 0.00002, adj=0, labels=expression(mu[1]==5000))
text(6200, 0.00002, adj=0, labels=expression(mu[2]==7000))


(z1 <- (42-36)/sqrt(30))
(p.value <- pnorm(z1, lower.tail=F) * 2)
(z2 <- (41.5-36)/sqrt(30) )
(p.value <- pnorm(z2, lower.tail=F) * 2)


#################################
# 예제 8-8
#################################
set.seed(1)
(x1 <- round(rexp(5, 1/5000)))
set.seed(1)
(y1 <- round(rexp(6, 1/7000)))
x1[3] <- y1[3]

(z3 <- c(x1, y1))
rank(z3)
(W <- sum(rank(z3)[6:11]))
(p.value <- (1 - pwilcox(floor(W)-21, 6, 5))*2 )
(p.value <- sum( dwilcox((ceiling(W)-21):51, 6, 5))*2 )

2 / choose(11, 6)

(c0 <- rank(z3))
m <- length(x1)
n <- length(y1)
N <- m + n
W <- NULL

for(i in 0:2^N) {
  c1 <- i
  c3 <- rep(0, N)
  
  for(j in 1:N) {  
    c2 <- floor(c1/2)
    c3[j] <- c1 - c2*2
    c1 <- c2 
  }
  
  if (sum(c3) != n) next
  W <- c(W, sum(c0*c3)) 
}

(tab <- table(W))
x <- as.numeric(names(tab))
y <- as.numeric(tab)/choose(N, n)
plot(x, y, xlab="x", ylab="probability", type="h")
title("Distribution of W when ties do exist")

tab[as.numeric(names(tab)) >= 40.5]
sum(tab[as.numeric(names(tab)) >= 40.5])
p.value <- sum(tab[as.numeric(names(tab)) >= 40.5])/choose(11, 6)*2
p.value


#################################
# 예제 8-9
#################################
W <- 40.5
m <- length(x1)
n <- length(y1)
t1 <- 2
(E.W <- n*(m+n+1)/2)
(V.W <- m*n*(m+n+1)/12 - m*n/(12*(m+n)*(m+n-1))*t1*(t1^2-1))
(Z <- (W - E.W)/sqrt(V.W))
pnorm(Z, lower.tail=F)*2
(Z1 <- (W-0.5-E.W)/sqrt(V.W))
pnorm(Z1, lower.tail=F)*2


#################################
# wilcox.test() 함수
#################################
x; y
wilcox.test(y, x)
wilcox.test(y, x, correct=F)
wilcox.test(y, x, exact=T)

x1; y1
wilcox.test(y1, x1)
wilcox.test(y1, x1, correct=F)
wilcox.test(y1, x1, exact=T)


#################################
# 예제 8-10
#################################
before <- c(90, 78, 105, 95, 89, 120, 101, 99, 75, 83)
after <- c(75, 75, 90, 80, 77, 105, 70, 60, 56, 70)
(dif <- before - after)
#예제(1)
idx <- rep(1, 10)
(S <- sum(idx[dif > 10]))
(p.value <- 1 - pbinom(S-1, size=10, prob=0.5))
#예제(2)
wilcox.test(dif, mu=20, alternative="greater")
#예제(3)
mean(dif)    # 3-1
median(dif)  # 3-2
# 3-3
(s <- sort(dif))
n <- length(s)
Wij <- outer(s, s, FUN=function(x, y) (x+y)/2)
dimnames(Wij) <- list(s, s)
Wij
(j.ge.i <- upper.tri(Wij, diag=T))
(w <- Wij[j.ge.i])
median(w)

#################################
# 예제 8-11
#################################
counts <- c(40, 34, 39, 45, 39, 43)
n <- length(counts)
alpha <- 0.05
(chisq <- sum((counts-40)^2/40))      # (1) chi^2
qchisq(1-alpha, df=n-1)               # (2) chi_0.95;9
PVAL <- pchisq(chisq, df=n-1)
1 - PVAL                              # (3) p-value
chisq.test(counts)                    # (4) chisq.test() 함수


#################################
# 예제 8-12
#################################
menus <- c(14, 21, 19, 18, 21, 12, 14, 19, 13, 20, 17, 12)
dim(menus) <- c(3, 4)
dimnames(menus) <- list(c("rice", "topping", "snack"), 
                        paste(1:4, "year"))
menus
(sum.i <- apply(menus, 1, sum))          # (1) n_i.
(sum.j <- apply(menus, 2, sum))          # (2) n_.j
(eij <- outer(sum.i, sum.j)/sum(menus))  # (3) E_ij
(chisq <- sum((menus-eij)^2/eij))        # (4) chi^2
alpha <- 0.05
(ni <- length(sum.i))                    # (5) R
(nj <- length(sum.j))                    # (6) C   
qchisq(1-alpha, df=(ni-1)*(nj-1))        # (7) chi_0.95;6
PVAL <- pchisq(chisq, df=(ni-1)*(nj-1))
1 - PVAL                                 # (8) p-value
chisq.test(menus)                        # (9) chisq.test() 함수


#################################
# 예제 8-13
#################################
eggs <- c(29, 30, 29, 27, 26, 27, 29, 28, 26, 27, 
          25, 27, 28, 26, 25, 30, 28, 26, 28, 26)
dim(eggs) <- c(5, 4)
dimnames(eggs) <- list(NULL, paste("feed", 1:4))
eggs                                    # (1) data        
ranks <- rank(eggs)                     
dim(ranks) <- c(5, 4)
dimnames(ranks) <- list(NULL, paste("feed", 1:4))
ranks                                   # (2) rank data
(n <- length(eggs))                     # (3) block counts 
(k <- NCOL(eggs))                       # (4) treatment counts
(Ri <- apply(ranks, 2, sum))            # (5) R_i 
(sumsq <- sum(Ri^2/ni))
(K <- 12/(n*(n+1))*sumsq-3*(n+1))       # (6) K 통계량
(ties <- table(eggs))                   # (7) 순위의 그룹
(K <- K/(1-sum(ties^3-ties)/(n^3-n)))   # (8) 보정 K 통계량
alpha <- 0.05
qchisq(1-alpha, df=(k-1))               # (9) chi_0.95;3
pchisq(K, df=(k-1), lower.tail=F)       # (10) p-value
(feeds <- as.factor(rep(paste("feed", 1:4), each=5)))
kruskal.test(as.vector(eggs) ~ feeds)   # (11) kruskal.test() 함수


#################################
# 예제 8-14
#################################
times <- c(4, 3, 3, 5, 4, 6, 4, 3, 4, 2, 3, 5, 3, 4, 3, 2, 2, 3, 3, 4, 3)
dim(times) <- c(7, 3)
dimnames(times) <- list(paste("rabbit", 1:7), paste("anodynia", 1:3))
times                                   # (1) data        
(ranks <- apply(times, 1, rank))
(ranks <- t(ranks))                     # (2) rank data
(n <- NROW(times))                      # (3) data counts 
(k <- NCOL(times))                      # (4) group counts
(Ri <- apply(ranks, 2, sum))            # (5) R_i 
(ni <- apply(ranks, 2, length))         # (6) n_i
(sumsq <- sum((Ri-n*(k+1)/2)^2))
(FS <- 12/(n*k*(k+1))*sumsq)            # (7) Friedman 통계량           
(A <- sum(ranks^2))
(C <- n*k*(k+1)^2/4)
(FS <- (k-1)*sumsq/(A-C))               # (8) 수정 Friedman 통계량
alpha <- 0.05
qchisq(1-alpha, df=(k-1))               # (9) chi_0.95;2
pchisq(FS, df=(k-1), lower.tail=F)      # (10) p-value
(groups <- rep(paste("anodynia", 1:3), each=7))
(blocks <- rep(paste("rabbit", 1:7), times=3))
friedman.test(times, groups, blocks)    # (11) kruskal.test() 함수



#################################
# 예제 8-15
#################################

# (1) 런의 수 r의 확률질량 함수
drun <- function(r, n1, n2)
{
  k <- r %/% 2
  
  ifelse(r %% 2,
    (choose(n1-1, k)*choose(n2-1, k-1) + 
     choose(n2-1, k)*choose(n1-1, k-1)) /
    choose(n1+n2, n1)  ,
    2*choose(n1-1, k-1)*choose(n2-1, k-1)/
      choose(n1+n2, n1))
}

# (2) 런의 수 r의 확률분포 함수
prun <- function(r, n1, n2, lower.tail=TRUE) 
{
  ifelse(lower.tail, sum(drun(2:r, n1, n2)), 1-sum(drun(2:r, n1, n2)))
}  
  
x <- factor(c("H", "T", "H", "H", "T", "T", "H", "H", "T", "H",
              "T", "H", "H", "T", "H", "T", "T", "H", "T", "T"))

n <- table(x)
n1 <- n[1]                            # (3) n1
n2 <- n[2]                            # (4) n2

r <- sum(diff(as.numeric(x))!=0) + 1  # (5) 런의 개수

prun(14, 10, 10)                      # (6) P(R>=14)
prun(14, 10, 10, lower.tail=F)        # (7) P(R<=14)


#################################
# 예제 8-16
#################################
n <- table(x)
n <- as.vector(n)
(n1 <- n[1])                                         # (1) n1
(n2 <- n[2])                                         # (2) n2
(r <- sum(diff(as.numeric(x))!=0) + 1)               # (3) 런의 개수

(E <- 2*n1*n2/(n1+n2)+1)                             # (4) E(R)
(V <- 2*n1*n2*(2*n1*n2-n1-n2)/((n1+n2)^2*(n1+n2-1))) # (5) V(R)
(Z <- (r-E)/sqrt(V))                                 # (6) Z
alpha <- 0.05
qnorm(1-alpha/2)                                     # (7) N_0.975
2*pnorm(Z, lower.tail=F)                             # (8) p-value
if (!length(grep("tseries", installed.packages()[, "Package"])))
  install.packages("tseries")                        # (9) 패키지설치
library(tseries)                                     # (10) tseries 패키지
runs.test(x)                                         # (11) runs.test() 함수




#################################
# 예제 8-17
#################################
X <- c(86, 97, 99, 100, 101, 103, 106, 110, 112, 113)
Y <- c(0, 20, 28, 27, 50, 29, 7, 17, 6, 12)
(n <- length(X))
(S <- rank(X))
(R <- rank(Y))
(d <- S-R)
(ds <- d^2)
(sum.ds <- sum(ds))
(rs <- 1 - 6*sum.ds/(n*(n^2-1)))          # (1) 스피어만 순위상관계수 
alpha <- 0.05
if (!length(grep("SuppDists", installed.packages()[, "Package"])))
  install.packages("SuppDists")           # (2) SuppDists 패키지설치
library(SuppDists)                        # (3) SuppDists 로드
qSpearman(1-alpha/2, r=n)                 # (4) Spearman_0.975
2*pSpearman(rs, r=n, lower.tail=TRUE)     # (5) p-value
cor.test(X, Y, method="spearman")         # (6) cor.test() 함수


#################################
# 예제 8-18
#################################
X <- c(12, 17, 39, 22, 34, 23, 15, 19, 25, 33, 37, 25, 35, 46, 30,
       46, 41, 41, 21, 20, 27, 32, 42, 29, 20, 25, 29, 13, 43, 36)
Y <- c(3.3, 4.3, 8.4, 3.1, 7.6, 4.6, 4.1, 4.4, 3.9, 8.3, 
       8.5, 4.3, 8.9, 9.8, 8.0, 9.2, 8.1, 7.2, 3.2, 3.3,
       6.1, 8.1, 8.4, 6.6, 2.6, 4.2, 4.6, 5.0, 8.6, 8.9)
(n <- length(X))
(S <- rank(X))
(R <- rank(Y))
(d <- S-R)
(ds <- d^2)
(sum.ds <- sum(ds))
(r.pearson <- cor(X, Y))                  # (1) 피어슨 상관계수
(TIES <- (min(length(unique(x)), length(unique(y))) < n)) # (2) 동점여부
# (3) 스피어만 순위상관계수
(rs <- ifelse(TIES,  
  {                                       # (3-1) 동점이 있을 경우
    ties.X <- table(X)                            
    ties.Y <- table(Y)
    st.X <- sum(ties.X^3-ties.X)
    st.Y <- sum(ties.Y^3-ties.Y)
    sum.fk <- 1-st.X/(n^3-n)
    sum.gm <- 1-st.Y/(n^3-n)
    (1-6/(n^3-n)*(sum.ds+1/12* st.X+1/12* st.Y))/
       (sqrt(sum.fk)*sqrt(sum.gm))
  },
  1 - 6*sum.ds/(n*(n^2-1))))              # (3-2) 동점이 없을 경우

plot(X, Y, pch=16, main="scatter plot")   # (4) 산점도
(lms <- lm(Y~X))
abline(lms)
text(14, 9, adj=0, substitute(r[p]==rp, list(rp=round(r.pearson,7))))
text(14, 8.5, adj=0, substitute(r[s]==rs, list(rs=round(rs,7))))
text(14, 8, adj=0, sprintf("Y = %fX + %f", lms$coef[2], lms$coef[1]))

(T <- rs*sqrt(n-2)/sqrt(1-rs^2))          # (5) 통계량
alpha <- 0.05
qt(1-alpha/2, df=n-2)                     # (6) t_0.975
2*pt(T, df=n-2, lower.tail=FALSE)         # (7) p-value
cor.test(X, Y, method="spearman")         # (8) cor.test() 함수

if (!length(grep("pspearman", installed.packages()[, "Package"])))
  install.packages("pspearman")           # (9) pspearman 패키지설치
library(pspearman)                        # (10) pspearman 로드
spearman.test(X, Y, approximation="exact")# (11) spearman.test()
spearman.test(X, Y, approximation="AS89") # (12) spearman.test()
spearman.test(X, Y, approximation="t-distribution") # (13) spearman.test()

