##########################################
# 예제 2-1
##########################################
x <- c(156, 165, 178, 176, 175, 166, 173, 166, 179, 180, 
       173, 166, 177, 182, 186, 165, 169, 180, 182, 164,
       177, 172, 170, 169, 168, 170, 172, 164, 177, 178,
       159, 163, 177, 182, 178, 173, 177, 170, 165, 168,
       160, 158, 182, 177, 170, 174, 173, 172, 168, 172)

(n <- length(x))                              # (1) n
(x1 <- min(x))                                # (2) min
(xn <- max(x))                                # (3) max
(nclass <- round(1 + log(n)/log(2)))          # (4) 계급 개수
unit <- function(x) {                         # (5) 최소단위함수 정의
  unit <- 1
  
  if (all(x==floor(x))) {                     # 정수일 경우
    while(all(x%%10==0)) {
      x <- x / 10
      unit <- unit * 10 
    }
  } 
  else {                                      # 소수일 경우
    unit <- unit /10
    while(all(x!=floor(x)) & any(x%%1>0)) {
      x <- x * 10
      unit <- unit / 10 
    }
  }
  
  unit
}

(u <- unit(x))                                # (6) 최소단위
(M <- (xn-x1)/u +1)                           # (7) 가능한 자료의 최대수
(m <- M/nclass)                               # (8) 계급에 포함될 자료 최대수
(interval <- ceiling(m)*u)                    # (9) 계급 간격
(minlimit <- ifelse(u==x1, u/2,
      x1-(nclass*interval-diff(range(x)))/2)) # (10) 첫계급의 하한
(breaks <- seq(from=minlimit, 
               length=nclass+1, by=interval)) # (11) 계급 경계
(x <- cut(x, breaks, dig.lab=5))              # (12) 계급구하기(범주형화)
(cval <- seq(from=minlimit+interval/2, 
             length=nclass, by=interval))     # (13) 계급값
(freq <- table(x))                            # (14) 계급도수 
(cfreq <- cumsum(freq))                       # (15) 누적도수  
(freq.rel <- freq/n)                          # (16) 상대도수
(cfreq.rel <- cumsum(freq.rel))               # (17) 누적상대도수

data.frame(Freq=as.vector(freq),              # (18) 도수분포표
           Cum.Freq=cfreq,
           Rel.Freq=as.vector(freq.rel),
           Cum.Rel=cfreq.rel,
           Class.Val=cval)

(freq.rel <- prop.table(freq))                # (19) 상대도수 함수
nclass.Sturges(x)                             # (20) 계급개수 구하기

##########################################
# 예제 2-2
##########################################
head(state.region)
(n <- length(state.region))
freq <- table(state.region)                   # (1) 계급도수 
cfreq <- cumsum(freq)                         # (2) 누적도수 
freq.rel <- prop.table(freq)                  # (3) 상대도수
cfreq.rel <- cumsum(freq.rel)                 # (4) 누적상대도수
data.frame(Freq=as.vector(freq),              # (5) 도수분포표
           Cum.Freq=cfreq,
           Rel.Freq=as.vector(freq.rel),
           Cum.Rel=cfreq.rel)


##########################################
# 예제 2-3
##########################################
x <- c(
  2, 3, 4, 5, 3, 4, 6, 7, 6, 4, 5, 6, 9, 8, 7, 6, 5, 4, 5, 3,
  2, 4, 3, 5, 6, 7, 5, 6, 7, 8, 4, 3, 5, 6, 7, 8, 9, 9, 7, 5,
  2, 3, 4, 5, 3, 6, 7, 8, 5, 4, 6, 4, 3, 6, 8, 9, 6, 4, 6, 5,
  3, 5, 7, 4, 6, 8, 5, 7, 9, 6, 4, 3, 6, 9, 8, 6, 4, 5, 3, 3,
  4, 6, 7, 5, 4, 6, 8, 6, 4, 6, 8, 9, 7, 5, 6, 7, 4, 3, 6, 8,
  4, 6, 8, 5, 4, 2, 3, 6, 4, 6, 8, 9, 6, 7, 8, 6, 5, 4, 6, 6,
  3, 4, 6, 5, 7, 8, 7, 6, 5, 4, 5, 7, 9, 7, 8, 7, 6, 5, 4, 3,
  5, 3, 3, 5, 7, 8, 9, 6, 5, 4, 3, 2, 2, 4, 6, 4, 3, 5, 7, 7,
  4, 5, 7, 6, 5, 3, 5, 6, 7, 8, 7, 5, 6, 8, 7, 6, 4, 6, 7, 3,
  2, 3, 4, 5, 6, 5, 7, 8, 5, 4, 3, 6, 7, 9, 7, 5, 5, 6, 7, 5)
(n <- length(x))
(nclass <- round(1 + log(n)/log(2)))          # (1) 계급 개수
unique(x)
length(unique(x))                             # (2) 데이터 종류
freq <- table(x)                              # (3) 계급도수 
cfreq <- cumsum(freq)                         # (4) 누적도수 
freq.rel <- prop.table(freq)                  # (5) 상대도수
cfreq.rel <- cumsum(freq.rel)                 # (6) 누적상대도수
data.frame(Freq=as.vector(freq),              # (7) 도수분포표
           Cum.Freq=cfreq,
           Rel.Freq=as.vector(freq.rel),
           Cum.Rel=cfreq.rel)


##########################################
# 분할표
##########################################
set.seed(1)
x <- sample(1:4, 200, replace=T)
set.seed(2)
y <- sample(LETTERS[1:5], 200, replace=T)
(ct <- table(x, y))                         # 2차원 분할표

addmargins(ct)                              # 주변합 추가
ct/n                                        # 상대도수(결합분포)
prop.table(ct, margin=1)                    # 상대도수(행기준)
prop.table(ct, margin=2)                    # 상대도수(열기준)

addmargins(prop.table(ct, 1))               # 상대도수(행기준)
addmargins(prop.table(ct, 2))               # 상대도수(열기준)

set.seed(1)
x <- data.frame(                            # (1) 가상의 데이터 생성
  larges=sample(LETTERS[1:2], 100, replace=T),
  smalls=sample(letters[1:3], 100, replace=T),
  numbers=sample(1:2, 100, replace=T),
  symbols=sample(c("#","$"), 100, replace=T))
head(x)                                     # (2) 데이터의 구조
ftable(x)                                   # (3) 4차원 분할표
ftable(x, row.vars = 1:2)                   # (4) 행의 변수를 2개로 지정

xtabs(~ larges, data=x)                     # (5) 1차원 분할표
xtabs(~ larges + smalls, data=x)            # (6) 2차원 분할표
xtabs(numbers ~ larges + smalls, data=x)    # (7) 2차원 집계표(numbers집계)

is(ct)
summary(ct)                                 # (8) ct 분할표 독립성 검정
large.small <- xtabs(~ larges + smalls, data=x)
is(large.small)
summary(large.small)                        # (9) xtabs 분할표 독립성 검정


##########################################
# 예제 2-4
##########################################
x <- c(156, 165, 178, 176, 175, 166, 173, 166, 179, 180, 
       173, 166, 177, 182, 186, 165, 169, 180, 182, 164,
       177, 172, 170, 169, 168, 170, 172, 164, 177, 178,
       159, 163, 177, 182, 178, 173, 177, 170, 165, 168,
       160, 158, 182, 177, 170, 174, 173, 172, 168, 172)

par(mfrow=c(1, 2))                           # 그림 2-1의 틀 지정
hist(x, col="lightblue", main="hist breaks") # (1) 히스토그램
(obj <- hist(x, plot= FALSE))                # (2) 도수분포정보
data.frame(class=levels(cut(x, obj$breaks)), # (3) 도수분포표
           Freq=obj$counts,              
           Cum.Freq=cumsum(obj$counts),
           Rel.Freq=obj$density,
           Cum.Rel=cumsum(obj$density),
           Class.Val=obj$mids)
bks <- c(153.5, 158.5, 163.5, 168.5, 173.5,  # (4) 예제 2-1 breaks
         178.5, 183.5, 188.5)
hist(x, breaks=bks, col="lightblue", 
     main="User define breaks")              # (5) 예제 2-1 도수분포표 적용 

cumhist <- function(data, col="cyan",        # (6) 사용자정의함수
        xlab=deparse(substitute(data)), 
                    main="Cumulative Histogram") 
{
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  par(mar=c(4, 4, 2, 4))
  obj <- hist(data, plot=FALSE)
  
  bins <- matrix(levels(cut(x, obj$breaks)))
  bins <- apply(bins, 1, function(x) 
    as.numeric(gsub("[[:punct:]]", "", 
    unlist(strsplit(x, ",")))))
  
  counts <- cumsum(obj$counts)
  breaks <- obj$breaks
  
  ylab <- "Frequency"
  plot(breaks, c(0, counts), type="n", bty = "n",
       col=col, xlab=xlab, ylab=ylab, main=main)
  axis(4, pretty(counts, 5), pretty(0:1, 5))
  mtext("Probability", side=4, line=2)
  n <- length(breaks)
  rect(breaks[-n], 0, breaks[-1L], counts, col=col)
}

par(mfrow=c(1, 1))                           # 그림 2-2 
cumhist(x)                                   # (7) 누적히스토그램

x <- c(                                      # (8) 예제 2-3 데이터   
  2, 3, 4, 5, 3, 4, 6, 7, 6, 4, 5, 6, 9, 8, 7, 6, 5, 4, 5, 3,
  2, 4, 3, 5, 6, 7, 5, 6, 7, 8, 4, 3, 5, 6, 7, 8, 9, 9, 7, 5,
  2, 3, 4, 5, 3, 6, 7, 8, 5, 4, 6, 4, 3, 6, 8, 9, 6, 4, 6, 5,
  3, 5, 7, 4, 6, 8, 5, 7, 9, 6, 4, 3, 6, 9, 8, 6, 4, 5, 3, 3,
  4, 6, 7, 5, 4, 6, 8, 6, 4, 6, 8, 9, 7, 5, 6, 7, 4, 3, 6, 8,
  4, 6, 8, 5, 4, 2, 3, 6, 4, 6, 8, 9, 6, 7, 8, 6, 5, 4, 6, 6,
  3, 4, 6, 5, 7, 8, 7, 6, 5, 4, 5, 7, 9, 7, 8, 7, 6, 5, 4, 3,
  5, 3, 3, 5, 7, 8, 9, 6, 5, 4, 3, 2, 2, 4, 6, 4, 3, 5, 7, 7,
  4, 5, 7, 6, 5, 3, 5, 6, 7, 8, 7, 5, 6, 8, 7, 6, 4, 6, 7, 3,
  2, 3, 4, 5, 6, 5, 7, 8, 5, 4, 3, 6, 7, 9, 7, 5, 5, 6, 7, 5)
                            
par(mfrow=c(1, 2))                          # 그림 2-3
hist(x, main="graphics::hist")              # (9) graphics 패키지 hist() 
library(MASS)   
truehist(x, prob=F, main="MASS::truehist")  # (10) MASS 패키지 truehist()



##########################################
# 분할표 그래프
##########################################
set.seed(1)
x <- sample(1:4, 200, replace=T)
set.seed(2)
y <- sample(LETTERS[1:5], 200, replace=T)
(ct <- table(x, y))                         

table(x)
table(y)
(sumx <- apply(ct, 1, sum))
(sumy <- apply(ct, 2, sum))

cols <- c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")
op <- par(no.readonly = TRUE)
par(mfrow=c(2, 2), mar=c(2.5, 3, 2, 3))           # 그림 2-4, 파이차트
pie(sumx, main="Pie Chart (x)", radius=1)         # (1) 파이차트 x
pie(sumy, main="Pie Chart (y) ", radius=1)        # (2) 파이차트 y
barplot(ct, col=cols, main="Stacked Bar Plots")    # (3) 누적막대그래프 
legend(4, 45, fill=cols, legend=rownames(ct))      # (4) 범례
barplot(ct, beside=T, col=cols, main="Parallel Bar Plots")  # (5) 막대그래프 
legend(13, 20, fill=cols, legend=rownames(ct))     # (6) 범례
par(op)


op <- par(no.readonly = TRUE)
par(mfrow=c(2, 2), mar=c(2.5, 3, 2, 3))               # 그림 2-5
apply(Titanic,c(4, 1), sum)
barplot(apply(Titanic, c(4, 1), sum), col=c("lightblue", "mistyrose"),
  main="Survived over Economic status (class)", ylim=c(0, 1300),
  legend=rownames(apply(Titanic, c(4, 1), sum)))
apply(Titanic, c(4, 2),sum)
barplot(apply(Titanic, c(4, 2), sum), col=c("lightblue", "mistyrose"),
  main="Survived over Sex",
  legend=rownames(apply(Titanic, c(4, 2), sum)))
apply(Titanic, c(4, 3), sum)
barplot(apply(Titanic, c(4, 3), sum), col=c("lightblue", "mistyrose"),
  main="Survived over Age", ylim=c(0, 3000),
  legend=rownames(apply(Titanic, c(4, 3), sum)))
apply(Titanic, 4, sum)
barplot(apply(Titanic, 4, sum), col=c("lightblue", "mistyrose"),
  main="Survived", legend=dimnames(Titanic)$Survived)
par(op)

ftable(Titanic)

temp <- c(apply(Titanic[dimnames(Titanic)[[1]]!="Crew",,,], 2:4, sum),
       Titanic[dimnames(Titanic)[[1]]=="Crew",,,])
dim(temp) <- c(2, 2, 2, 2)
dimnames(temp)=list(Sex=c("Male", "Female"),
                  Age=c("Child", "Adult"),
                  Survived=c("No", "Yes"),
                  Class=c("Passenger", "Crew"))
mosaicplot(apply(temp, c(1, 4, 3), sum),               # 그림 2-6
           main="Titanic Survival over Class & Sex",
           col=hcl(c(240, 120)),
           off=c(5, 5, 5))



##########################################
# 예제 2-5
##########################################
x <- c(85, 90, 90, 95, 80, 85, 80, 80, 90, 20)
(n <- length(x))
(tot <- sum(x))
(arthmean <- tot/n)
x-arthmean
mean(x)
mean(x[-10])


##########################################
# 예제 2-6
##########################################
base <- 70
score <- c(72, 75, 79, 80, 88)
(n <- length(score))
(score.bef <- c(base, score)[-(n+1)])
(score.rise <- diff(c(base, score)))
(rate.rise <- score.rise/score.bef*100)  # (1) 월별 상승률 (%)
(tot <- sum(rate.rise))
tot/n                                    # (2) 산술평균
(prd <- prod(rate.rise))
prd^(1/n)                                # (3) 기하평균 
exp((1/n)*sum(log(rate.rise)))           # (4) 기하평균_간편식  
if (!length(grep("psych", installed.packages()[, "Package"])))
  install.packages("psych")              # (5) psych 패키지설치
library(psych)
geometric.mean(rate.rise)                # (6) geometric.mean()


##########################################
# 예제 2-7
##########################################
velocity <- c(65, 70, 54, 59, 40)
mean(velocity)                          # (1) 산술평균 
library(psych)
geometric.mean(velocity)                # (2) 기하평균 
(n <- length(velocity))
(sum.rev <- sum(1/velocity))
n/sum.rev                               # (3) 조화평균
harmonic.mean(velocity)                 # (4) harmonic.mean()


##########################################
# 그림 2-7
##########################################
xs <- 5
ys <- seq(10, 5, length= 100)
mean.arth <- apply(t(ys), 2, 
                  function(x) mean(c(x, xs)))
mean.geo <- apply(t(ys), 2, 
                   function(x) geometric.mean(c(x, xs)))
mean.harm <- apply(t(ys), 2, 
                    function(x) harmonic.mean(c(x, xs)))
means <- cbind(mean.arth, mean.geo, mean.harm)
difs <- abs(xs-ys)
matplot(difs, means, type="l", main="두 값 a, b에 대한 평균의 비교",
        xlab="두 값의 차이", ylab="평균")
legend.str <- c(expression(산술평균 == over(a+b, 2)), 
                expression(기하평균 == sqrt(a%*%b)), 
                expression(조화평균 == over(2, over(1, a) + over (1, b))))
legend(0.3, 7.6, legend.str, col=1:3, lty=1:3, bty="n")


xs <- 1:9
ys <- 10:100
mean.arth <- apply(t(ys), 2, 
                   function(x) mean(c(x, xs)))
mean.geo <- apply(t(ys), 2, 
                  function(x) geometric.mean(c(x, xs)))
mean.harm <- apply(t(ys), 2, 
                   function(x) harmonic.mean(c(x, xs)))
means <- cbind(mean.arth, mean.geo, mean.harm)
main.str <- expression(paste(list(x[1]==1, x[2]==2, 
                                  cdots, x[9]==9, x[10]==X), 의~평균))
matplot(ys, means, type="l", main=main.str,
        xlab=expression(x[10]==X), ylab="평균")
legend.str <- c(expression(산술평균 == sum(x[i], i==1, 10)/ 10), 
                expression(기하평균 == sqrt(prod(x[i], i==1, 10), 10)), 
                expression(조화평균 == 10/ sum(over (1, x[i]), i==1, 10)))
legend(10, 15, legend.str, col=1:3, lty=1:3, bty="n")


##########################################
# 예제 2-8
##########################################
x <- c(85, 90, 90, 95, 80, 85, 80, 80, 90, 20)
(n <- length(x))
mean(x)                                       # (1) 산술평균 
mean(x[-10])                                  # (2) 극단값제거 산술평균
(x.sort <- sort(x))                           # (3) 오름차순 정렬
ifelse(n%%2, x.sort[(n+1)/2],                 # (4) 중위수
    (x.sort[n/2]+x.sort[n/2+1])/2)
median(x)                                     # (5) median()


##########################################
# 예제 2-9
##########################################
x <- c(                                      # (1) 예제 2-3 데이터   
  2, 3, 4, 5, 3, 4, 6, 7, 6, 4, 5, 6, 9, 8, 7, 6, 5, 4, 5, 3,
  2, 4, 3, 5, 6, 7, 5, 6, 7, 8, 4, 3, 5, 6, 7, 8, 9, 9, 7, 5,
  2, 3, 4, 5, 3, 6, 7, 8, 5, 4, 6, 4, 3, 6, 8, 9, 6, 4, 6, 5,
  3, 5, 7, 4, 6, 8, 5, 7, 9, 6, 4, 3, 6, 9, 8, 6, 4, 5, 3, 3,
  4, 6, 7, 5, 4, 6, 8, 6, 4, 6, 8, 9, 7, 5, 6, 7, 4, 3, 6, 8,
  4, 6, 8, 5, 4, 2, 3, 6, 4, 6, 8, 9, 6, 7, 8, 6, 5, 4, 6, 6,
  3, 4, 6, 5, 7, 8, 7, 6, 5, 4, 5, 7, 9, 7, 8, 7, 6, 5, 4, 3,
  5, 3, 3, 5, 7, 8, 9, 6, 5, 4, 3, 2, 2, 4, 6, 4, 3, 5, 7, 7,
  4, 5, 7, 6, 5, 3, 5, 6, 7, 8, 7, 5, 6, 8, 7, 6, 4, 6, 7, 3,
  2, 3, 4, 5, 6, 5, 7, 8, 5, 4, 3, 6, 7, 9, 7, 5, 5, 6, 7, 5)
table(x)                                     # (2) 도수분포
get.mode <- function(x) {                    # (3) 최빈수 함수
  tbl <- table(x)
  as.numeric(names(tbl[which(tbl==max(tbl))]))
}  
get.mode(x)                                  # (4) 2-3 최빈수
x <- c(                                      # (5) 예제 2-1 데이터   
  156, 165, 178, 176, 175, 166, 173, 166, 179, 180, 
  173, 166, 177, 182, 186, 165, 169, 180, 182, 164,
  177, 172, 170, 169, 168, 170, 172, 164, 177, 178,
  159, 163, 177, 182, 178, 173, 177, 170, 165, 168,
  160, 158, 182, 177, 170, 174, 173, 172, 168, 172)
obj <- hist(x, plot= FALSE)                  # (6) 도수분포정보
(freq <- obj$counts)                         # (7) 도수
(pos.mode <- which(freq==max(freq)))         # (8) 최빈수 계급 위치
(f.plus <- freq[pos.mode+1])                 # (9) f+
(f.minus <- freq[pos.mode-1])                # (10) f-
(C.lower <- obj$breaks[pos.mode])            # (11) C_lower
(I.class <- diff(obj$mids)[1])               # (12) I_class                      
C.lower + f.plus/(f.minus+f.plus)*I.class    # (13) 최빈수
get.mode(x)                                  # (14) get.mode()
  

##########################################
# quantile() 함수
##########################################
x <- c(10, 30, 70, 20, 100)
quantile(x, probs=c(0.25, 0.5, 0.75))
quantile(x, probs=0.3)


##########################################
# fivenum() 함수
##########################################
x12 <- 1:12
x13 <- 1:13
x14 <- 1:14
x15 <- 1:15
fivenum(x12)
fivenum(x13)
fivenum(x14)
fivenum(x15)


##########################################
# 예제 2-10
##########################################
set.seed(1)
x <- rnorm(500)
myquant <-                                   # (1) 백분위수 함수
  function(x, ps=c(0.25, 0.5, 0.75)) {  
  x.sort <- sort(x)
  n <- length(x)
  qs <- ifelse(rep(n,length(ps))%%2,
               x.sort[ps*n+1],
               (x.sort[ps*n]+x.sort[ps*n+1])/2)  
  names(qs) <- paste(ps*100, "%", sep="")
  qs
}
myquant(x)                                  # (2) 사분위수 계산
quantile(x)                                 # (3) quantile()
fivenum(x)                                  # (4) fivenum()
boxplot.stats(x)$stats                      # (5) boxplot.stats()
myquant(x, c(0.3, 0.6))                     # (6) 30, 60백분위수
quantile(x, probs=c(0.3, 0.6))              # (7) quantile()


##########################################
# 예제 2-11
##########################################
box.fivenum <- function(x) {                 # (1) 사용자 정의함수
  q1 <- quantile(x)[2]
  q2 <- quantile(x)[3]
  q3 <- quantile(x)[4]  
  
  iqr <- IQR(x)
  
  upper <- q3 + 1.5 * iqr
  lower <- q1 - 1.5 * iqr

  l.extreme <- ifelse(lower > min(x), min(x[x > lower]), min(x))
  u.extreme <- ifelse(upper < max(x), max(x[x < upper]), max(x))

  five <- c(l.extreme, q1, q2, q3, u.extreme)
  names(five) <- c("lower extreme", "Q1", "Q2", "Q3", "upper extreme")
  five
}
box.fivenum(x)                               # (2) 수염상자그림 5개 통계량

myboxplot <- function(x, col=NA, main=NA) {  # (3) 상자그림 함수
  five <- box.fivenum(x)
  outliers <- x[x<five[1] | x>five[5]]
  
  plot(sort(x), seq(-1, 1, length=length(x)), type="n", 
       axes=F, xlab=NA, ylab=NA, main=main)
  box()
  axis(1)
  rect(five[2], -0.25, five[4], 0.25, col=col)      # box
  lines(c(five[3], five[3]), c(-0.25, 0.25), lwd=3) # median
  lines(c(five[1], five[2]), c(0, 0), lty=2)        # lower whiskers
  lines(c(five[4], five[5]), c(0, 0), lty=2)        # upper whiskers
  lines(c(five[1], five[1]), c(-0.1, 0.1))          # lower extreme
  lines(c(five[5], five[5]), c(-0.1, 0.1))          # upper extreme
  points(outliers, rep(0, length(outliers)))        # outliers
}  

op <- par(no.readonly = TRUE)
par(mfrow=c(2, 1))                         # 그림 2-10
myboxplot(x, col="lightblue",              # (4) 사용자정의 함수
          main="my boxplot function")
boxplot(x, horizontal=T, col="lightblue",  # (5) boxplot() 함수
        main="boxplot function")
par(op)

##########################################
# 예제 2-12
##########################################
x <- iris$Sepal.Length
mean(x)                             # (1) 원 데이터 산술평균
x[150] <- 200
mean(x)                             # (2) Outlier 추가 산술평균

# (3) 절사평균의 사용자 정의 함수
trimmean <- function(x, trim=0.05) {
  n <- length(x)
  
  lower <- floor(n * trim) + 1
  upper <- n + 1 - lower
  
  x.sort <- sort(x)
  
  mean(x.sort[lower:upper])
}
trimmean(x)                         # (4) 사용자정의함수 사용
mean(x, trim=0.05)                  # (5) mean(), trim 인수

# (6) 윈저화 평균의 사용자 정의 함수
winsormean <- function(x, trim=0.05) {
  n <- length(x)
  
  lower <- floor(n * trim) + 1
  upper <- n + 1 - lower
  
  x.sort <- sort(x)
  x.sort[1:lower] <- quantile(x, probs=trim)
  x.sort[upper:n] <- quantile(x, probs=1-trim)
  mean(x.sort)
}
winsormean(x)                       # (7) 사용자정의함수 사용
library(psych)
winsor.mean(x, trim=0.05)           # (8) winsor.mean() 


##########################################
# 예제 2-13
##########################################
x <- c(324, 327, 332, 423, 325, 432, 353, 412, 433, 
       508, 442, 654, 420, 397, 372, 378, 298, 500,
       298, 361, 465, 299, 368, 364, 278)
(n <- length(x))                                
(Range <- max(x) - min(x))                      # (1) 범위공식
range(x)
diff(range(x))                                  # (2) 범위
(Iqr <- quantile(x)[4] - quantile(x)[2])        # (3) IQR 공식
IQR(x)                                          # (4) IQR()
diff(quantile(x, probs=c(0.1, 0.9)))            # (5) 10분위수범위    
mean(x)
sum((x-mean(x))^2)                              # (6) 제곱합
sum(abs(x-mean(x))/n)                           # (7) 평균편차
sum((x-mean(x))^2)/n                            # (8) 분산
var(x)                                          # (9) var()
sum((x-mean(x))^2)/(n-1)                        # (10) 표본분산
sqrt(sum((x-mean(x))^2)/n)                      # (11) 표준편차
sqrt(sum((x-mean(x))^2)/(n-1))                  # (12) 표본표준편차
sd(x)                                           # (13) sd()
sqrt(sum((x-mean(x))^2)/(n-1))/sqrt(n)          # (14) 표준오차
IQR(x)/2                                        # (15) 사분위편차  



##########################################
# 예제 2-14
##########################################
m <- 7
n <- 10
(from <- 2^(0:m) + c(0, rep(1, m)))
(to <- 2^(4:(m+3)))
mat <- matrix(1:m, nrow=m, ncol=n)
(mat <- apply(mat, 1, function(x) {           # (1) 7개 데이터
  i <- mean(x)
  set.seed(0)
  sample(from[i]:to[i], size=n, replace=T)
}))

(means <- apply(mat, 2, mean))                # (2) 평균
(sds <- apply(mat, 2, sd))                    # (3) 표본표준편차 
(cvs <- sds/means*100)                        # (4) 변동계수(%)

if (!length(grep("raster", installed.packages()[, "Package"])))
  install.packages("raster")                  # (5) raster 패키지설치
library(raster)
apply(mat, 2, cv)                             # (6) cv() 함수

op <- par(no.readonly = TRUE)
par(mfrow=c(3, 1), mar=c(2, 5, 2, 2))         # 그림 2-11
plot(means, type="l", ylab=expression(bar(X)), main=expression(bar(X)))
plot(sds, type="l", ylab="S", main="S")
plot(cvs, type="l", ylim=c(0,100), ylab="CV", main="CV")
par(op)

apply(mat, 2, function(x) 
  sum(abs(x-mean(x))/n)/median(x))            # (7) 평균편차계수1
apply(mat, 2, function(x) 
  sum(abs(x-mean(x))/n)/mean(x))              # (8) 평균편차계수2
apply(mat, 2, function(x) IQR(x)/sum(quantile(x, c(0.25, 0.75)))) # (9) 사분위편차계수


####################################
# 왜도, 그림 2-12
####################################
# (1) 최빈수를 구하는 함수
get.mode <- function(x) {
  tbl=table(x)
  as.numeric(names(tbl[which(tbl==max(tbl))]))
}

# (2) 왜도를 구하는 함수 (모멘트 공식)
get.skewness <- function(x) {
  mu <- mean(x)
  mu3 <- sum((x - mu)^3)/length(x)
  s3 <- (sum((x - mu)^2)/length(x))^(3/2)
  mu3 / s3
}

# (3) 왜도가 0에 근사한 데이터를 생성
set.seed(1)
x <- floor(rnorm(mean=10.5, sd=3, n=1500))
(x.mean <- mean(x))          # 평균
(x.median <- median(x))      # 중위수
(x.mode <- get.mode(x))      # 최빈수
get.skewness(x)              # 왜도(moment)

if (!length(grep("moments", installed.packages()[, "Package"])))
  install.packages("moments") # (4) moments 패키지설치
library(moments)
skewness(x)                   # (5)

# (6) 왜도가 음수인 데이터를 생성
y <- numeric(0)   
for(i in 1:15) {
  set.seed(1)
  y <<- c(y,sample(i:20, 100, replace=T))
}
(y.mean <- mean(y))
(y.median <- median(y))
(y.mode <- get.mode(y))
get.skewness(y)

# (7) 왜도가 양수인 데이터를 생성
z <- numeric(0)
for(i in 6:20) {
  set.seed(1)
  z <<- c(z,sample(1:i, 100, replace=T))
}
(z.mean <- mean(z))
(z.median <- median(z))
(z.mode <- get.mode(z))
get.skewness(z)

# (8) 이하 그래프 출력
op <- par(no.readonly = TRUE)
par(mfrow=c(3,2))
plot(density(x), main="Like Normal")
qqnorm(x)
qqline(x)
plot(density(y), main="skewed to the left")
abline(v=y.mean, col=1, lty=1)
abline(v=y.median, col=2, lty=2)
abline(v=y.mode, col=4, lty=3)
legend.str <- c("mean", "median", "mode")
legend(-2, 0.12, bty="n", legend=legend.str, col=c(1, 2, 4), lty=1:3)
qqnorm(y)
qqline(y)
plot(density(z), main="skewed to the right")
abline(v=z.mean, col=1, lty=1)
abline(v=z.median, col=2, lty=2)
abline(v=z.mode, col=4, lty=3)
legend(14, 0.11, bty="n", legend=legend.str, col=c(1, 2, 4), lty=1:3)
qqnorm(z)
qqline(z)
par(op)


####################################
# 첨도, 그림 2-13
####################################
# (1) 첨도를 구하는 함수
get.kurtosis<- function(x) {
  mu <- mean(x)
  mu4 <- sum((x - mu)^4)/length(x)
  s4 <- (sum((x - mu)^2)/length(x))^(4/2)
  mu4 / s4 - 3
}

# (2) 첨도가 0에 근사하는 데이터 생성 함수
makezero <- function(n=1500, seed.n=100000) {
  fun <- rnorm
  
  seed <- 0
  esp <- 10
  data <- numeric(n)
  
  for (i in 0:seed.n) {
    set.seed(i)
    x <- fun(n)
    k <- get.kurtosis(x)
    
    if (abs(0 - k) < esp) {
      esp <- abs(0 - k)
      seed <- i
      data <- x
    }
  }
  
  return(list(seed=seed, data=data))
}

# (3) 첨도가 음수인 데이터 생성 함수
makeminus <- function(n=1500, seed.n=100000) {
  fun <- function(n) {
    runif(n, min=-1.5, max=1.5) + rnorm(n, mean=0, sd=2)
  }
  
  seed <- 0
  esp <- 10
  data <- numeric(n)
  
  for (i in 0:seed.n) {
    set.seed(i)
    x <- fun(n)
    k <- get.kurtosis(x)
    
    if (esp > k) {
      esp <- k
      seed <- i
      data <- x
    }
  }
  
  return(list(seed=seed, data=data))
}

# (4) 첨도가 양수인 데이터 생성 함수
makeplus <- function(n=1500, seed.n=100000) {
  fun <- function(n) {
    runif(n, min=-0.5, max=0.5) + rnorm(n, mean=0, sd=0.5)
  }
  
  seed <- 0
  esp <- -10
  data <- numeric(n)
  
  for (i in 0:seed.n) {
    set.seed(i)
    x <- fun(n)
    k <- get.kurtosis(x)
    
    if (esp < k) {
      esp <- k
      seed <- i
      data <- x
    }
  }
  
  return(list(seed=seed, data=data))
}

# (5) 첨도가 0에 근사하는 데이터 생성
zero <- makezero()
zero$seed
x <- zero$data

# (6) 첨도가 음수인 데이터 생성
minus <- makeminus()
minus$seed
y <- minus$data

# (7) 첨도가 양수인 데이터 생성
plus <- makeplus()
plus$seed
z <- plus$data

# (8) 첨도 계산
get.kurtosis(x)
get.kurtosis(y)
get.kurtosis(z)

# (9) moments 패키지의 kurtosis 함수로 첨도 계산
library(moments)
kurtosis(x)  
kurtosis(y)
kurtosis(z)

# (10) 첨도에 따른 분포모양 그래프, 그림 2-13
plot(density(x), main="Kurtosis compare", ylim=c(0, 0.7), lwd=2)
lines(density(y), lty=2, col=2, lwd=2)
lines(density(z), lty=3, col=4, lwd=2)
lgd.str <- c("K = 0", "K < 0", "K > 0")
legend(-4, 0.5, lgd.str, lty=1:3, bty="n", col=c(1, 2, 4), lwd=2)
