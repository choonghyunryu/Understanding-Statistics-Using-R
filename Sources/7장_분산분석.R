(edu.method <- LETTERS[1:5])
t(combn(edu.method, 2))

#################################
# 예제 7-1
#################################
#(1) 데이터 읽기 (c:/datas 디렉토리에 있다고 가정)
oneway <- read.table("c:/datas/oneway.txt", header=T)	  
oneway
y <- oneway$ozone	
(mean.y <- mean(oneway$ozone))				# (2) 전체평균
(mean.A <- mean(oneway[oneway$garden=="A", "ozone"]))	# (3) A garden 평균
(mean.B <- mean(oneway[oneway$garden=="B", "ozone"]))	# (4) B garden 평균

(SST <- sum((oneway$ozone-mean.y)^2))			# (5) SST
n <- length(y)							# (6) 데이터 개수
sum(y^2)- n*mean(y)^2						# (7) SST(간편식)

(SSE_A <- sum((y[which(oneway$garden=="A")]-mean.A)^2)) # (8) SSE_A
(SSE_B <- sum((y[which(oneway$garden=="B")]-mean.B)^2)) # (9) SSE_B

SSE <- SSE_A + SSE_B						# (10) SSE

SSR <- 0
for (grp in levels(oneway$garden)) {				# (11)SSR 구하기
  ni <- sum(oneway$garden==grp) 
  SSR <- SSR + ni *(get(paste("mean", grp, sep="."))-mean.y)^2
}
SSR									# (12) SSR

(SSR <- SST - SSE)							# (13) SSR


#################################
# 예제 7-2, 그림 7-1, 2
#################################
# (1) SST의 표현
plot(y, ylim=c(0, 8), xlim=c(0, 24), pch=16,
  main=paste("Total Sum of Squares", SST, sep=" = "))
abline(h=mean.y)
for (i in 1:NROW(oneway)) {
  lines(x=c(i, i), y=c(y[i], mean.y))
  text(i, 0, (y[i]-mean.y)^2)
}
text(21, 0, paste("SST", SST, sep="="), adj=0)


# (2) SSE의 표현
plot(oneway$ozone, ylim=c(0, 8), xlim=c(0, 27), type="n",
  main=paste("Error Sum of Squares", SSE_A+SSE_B, sep=" = "))
abline(h=mean.A)
abline(h=mean.B)
for (i in 1:NROW(oneway)) {
  grp <- as.character(oneway$garden[i])
  lines(x=c(i, i), y=c(y[i], 
    get(paste("mean", grp, sep="."))))
  text(i, ifelse(grp=="A", 0, 8), 
    (y[i]-get(paste("mean", grp, sep=".")))^2)
  points(i, y[i], col=ifelse(grp=="A", 2, 4),
     pch=ifelse(grp=="A", 15, 16))
}
text(22, 0, paste("SSE_A", SSE_A, sep="="), adj=0)
text(22, 8, paste("SSE_B", SSE_B, sep="="), adj=0)


#################################
# 예제 7-3, 그림 7-3
#################################
plot(oneway$ozone, ylim=c(0, 8), xlim=c(0,27), type="n",
  main=paste("Treatment Sum of Squares", SSR, sep=" = "))
abline(h=mean.y)
abline(h=mean.A, lty=2)
abline(h=mean.B, lty=2)

SSRi <- numeric(0)
for (i in 1:NROW(oneway)) {
  arrows(i+0.3, mean.y, i+0.3, y[i], length=0.1, angle=20)

  grp <- as.character(oneway$garden[i])
  arrows(i, get(paste("mean", grp, sep=".")), 
    i, y[i], length=0.1, code=1, angle=20, lty=2)
  points(i, y[i], col=ifelse(grp=="A", 2, 4),
     pch=ifelse(grp=="A", 15, 16))
  text(i, ifelse(grp=="A", 0, 8), 
    (y[i]-mean.y)-(y[i]-get(paste("mean", grp, sep="."))))

  SSRi <- c(SSRi, (y[i]-mean.y)-(y[i]-get(paste("mean", grp, sep="."))))
}
SSR_A <- sum(SSRi[which(oneway$garden=="A")]^2)
SSR_B <- sum(SSRi[which(oneway$garden=="B")]^2)

text(22, 0, paste("SSR_A", SSR_A, sep="="), adj=0)
text(22, 8, paste("SSR_B", SSR_B, sep="="), adj=0)


#################################
# 예제 7-4
#################################
y1 <- c(110, 120, 80, 95, 115)
y2 <- c(220, 238, 250, 500)
y3 <- c(430, 450, 445, 460, 465)
y4 <- c(995, 1000, 1015, 1005, 997)
n1 <-  length(y1)
n2 <- length(y2)
n3 <- length(y3)
n4 <- length(y4)
(SSE = (n1-1)* var(y1) + (n2-1)*var(y2) + 	# (1) SEE
   (n3-1)*var(y3) + (n4-1)*var(y4) ) 
y <- c(y1, y2, y3, y4)
(SST  = sum( (y-mean(y))^2 ))  			# (2) SST
(CT <- sum(y)^2/length(y))   			# (3) CT
(SST <- sum(y^2) - CT)      			# (4) SST 구하기
(SStr <- n1*(mean(y1)-mean(y))^2 + n2*(mean(y2)-mean(y))^2 +
  n3*(mean(y3)-mean(y))^2 + n4*(mean(y4)-mean(y))^2)# (5) SStr
(SStr <- sum(y1)^2/n1 + sum(y2)^2/n2 + 
  sum(y3)^2/n3 + sum(y4)^2/n4 - CT) 		# (6) SStr 간편식
(SStr <- SST - SSE)            			# (7) SStr=SST-SSE 이용
K <- 4
(df.tr <- K-1)						# (8) 처리 자유도
(df.e <- length(y)-K)				# (9) 오차 자유도
(MStr <- SStr/df.tr)					# (10) MStr			
(MSE <- SSE/df.e)					# (11) MSE
(F0 <- MStr/MSE)					# (12) F0
pf(F0, df.tr, df.e, lower.tail=F)			# (13) p-value


y <- c(y1, y2, y3, y4)
treat <- rep(1:4, c(n1, n2, n3, n4))
(treat <- factor(paste("Treatment", treat)))	# (1) 요인생성
datas <- data.frame(y, treat)			# (2) 데이터프레임 생성
head(datas)								
lm.fit <- lm(y ~ treat, data = datas)		# (3) lm 객체생성
anova(lm.fit)						# (4) 분산분석표-anova()  함수
aov.treat <- aov(y ~ treat)				# (5) aov()  함수
summary(aov.treat)					# (6) 분산분석표
is(aov.treat)						# (7) aov체 객체
is(lm.fit)						# (8) lm 객체
methods(anova)					# (9) anova methods
is(anova.nls)						# (10) 찾을 수 없음 
is(stats:::anova.nls)					# (11) stats 패키지에

var(y1); var(y2) ; var(y3) ; var(y4)

#################################
# 그림 7-4
#################################
plot(x=c(0, 6), y=c(0, 1200), xlab ="", ylab ="", type ="n")
points(1:n1, y1, pch=15, col=1)
points(1:n2, y2, pch=16, col=2)
points(1:n3, y3, pch=17, col=3)
points(1:n4, y4, pch=18, col=4)
abline(h=mean(y1), lty=2)
abline(h=mean(y2), lty=2)
abline(h=mean(y3), lty=2)
abline(h=mean(y4), lty=2)

text(0, mean(y1)+40, labels=expression(bar(Y)[1]))
text(0, mean(y2)+40, labels=expression(bar(Y)[2]))
text(0, mean(y3)+40, labels=expression(bar(Y)[3]))
text(0, mean(y4)+40, labels=expression(bar(Y)[4]))
title("A Representation of Data")

#################################
# 예제 7-5
#################################
n <- sum(n1, n2, n3, n4)
K <- 4
df <- n-K
alpha <- 0.05
(reject <- qt(alpha/2, df, lower.tail=F))			# (1) 기각치
(LSD_45 <- sqrt(3653.547 * (1/4+1/5) ) * reject)	# (2) LSD
(LSD_55 <- sqrt(3653.547 * (1/5+1/5) ) * reject)	# (3) LSD
(diff1 <- c(mean(y1), mean(y2), mean(y3), mean(y4)))	# (4) 평균들
(mat1 <- matrix(diff1, nrow = 4, ncol 렬= 4))		# (5) 평균행렬
mat2 <- t(mat1)						# (6) 전치행렬
abs((mat1 - mat2)*upper.tri(matrix(0, ncol=4, nrow=4)))   # (7) 평균들의 차

(n <- sum(n1, n2, n3, n4))
K <- 4
(df <- n-K)
alpha <- 0.05; MSE <- 3653.547
(reject <- qt(alpha/2, df, lower.tail=F))			# (1) 기각치
(case <- t(combn(1:4, 2)))					# (2) 비교의 조합

apply(case, 1, function(x) {					# (3) 결과계산
  n.left <- get(paste("n", x[1], sep=""))
  mean.left <- mean(get(paste("y", x[1], sep="")))
  n.right <- get(paste("n", x[2], sep=""))
  mean.right <- mean(get(paste("y", x[2], sep="")))
  LSD <- sqrt(MSE*(1/n.left + 1/n.right)) * reject
  diff.mean <- abs(mean.left-mean.right)
  sprintf("|mean(y%s) - mean(y%s)| = %5.1f %s %s (H0:μ%s = μ%s is %s)", 
    x[1], x[2], round(diff.mean, 2), 
    ifelse(diff.mean>LSD, '>', '<=')  , round(LSD, 3), 
    x[1], x[2], ifelse(diff.mean>LSD, 'reject', 'accept'))
})


#################################
# 예제 7-6
#################################
K <- 4
df <- n-K
(reject <- qt(alpha/(K*(K-1)), df, lower.tail=F))		# (1) 기각치

apply(case, 1, function(x) {					# (2) 결과계산
  n.left <- get(paste("n", x[1], sep=""))
  mean.left <- mean(get(paste("y", x[1], sep="")))
  n.right <- get(paste("n", x[2], sep=""))
  mean.right <- mean(get(paste("y", x[2], sep="")))
  bonferroni <- sqrt(MSE*(1/n.left + 1/n.right)) * reject
  diff.mean <- abs(mean.left-mean.right)
  sprintf("|mean(y%s) - mean(y%s)| = %5.1f %s %s (H0:μ%s = μ%s is %s)", 
    x[1], x[2], round(diff.mean, 2), 
    ifelse(diff.mean>bonferroni, '>', '<=')  , round(bonferroni, 3), 
    x[1], x[2], ifelse(diff.mean>bonferroni, 'reject', 'accept'))
})


#################################
# 예제 7-7
#################################
K <- 4
(df <- n-K)
alpha <- 0.05
(reject <- qtukey(1-alpha, nmeans=K, df=df))        # (1) 기각치

apply(case, 1, function(x) {					              # (2) 결과계산
  n.left <- get(paste("n", x[1], sep=""))
  mean.left <- mean(get(paste("y", x[1], sep="")))
  n.right <- get(paste("n", x[2], sep=""))
  mean.right <- mean(get(paste("y", x[2], sep="")))
  
  tukey <- 
    if(n.left==n.right) sqrt(MSE/n.right) * reject
    else {
      harmomic.mean <- 2/(1/n.left + 1/n.right)
      sqrt(MSE/harmomic.mean) * reject
    }
  diff.mean <- abs(mean.left-mean.right)
  sprintf("|mean(y%s) - mean(y%s)| = %5.1f %s %s (H0:μ%s = μ%s is %s)", 
    x[1], x[2], round(diff.mean, 3), 
    ifelse(diff.mean>tukey, '>', '<=')  , round(tukey, 4), 
    x[1], x[2], ifelse(diff.mean>tukey, 'reject', 'accept'))
})


#################################
# 예제 7-8
#################################
K <- 4
alpha <- 0.05
(reject <- qf(1-alpha, df1=K-1, df2=n-K))  # (1) 기각치

ci <- matrix(0, ncol=4, nrow=6)            # (2) 상수 c 생성
for (i in 1:NROW(case)) {                  # (3) Σc_i=0 적용
  ci[i, case[i, 1]] <- 1
  ci[i, case[i, 2]] <- -1 
}
ci                                         # (4) 생성된 c 상수                                       

(ci.sum <- apply(ci, 1, function(x) {      # (5) Σ(c_i)^2/n_i
  sum(x^2 / apply(t(paste("n", 1:length(x), sep="")), 2, get)) 
}))  

case <- t(combn(1:4, 2))
(case <- cbind(case, 1:NROW(case)))        # (6) 계산을 위한 case 변경

apply(case, 1, function(x) {  				     # (7) 결과계산
  n.left <- get(paste("n", x[1], sep=""))
  mean.left <- mean(get(paste("y", x[1], sep="")))
  n.right <- get(paste("n", x[2], sep=""))
  mean.right <- mean(get(paste("y", x[2], sep="")))
    
  scheffe <- sqrt((K-1) * reject * MSE * ci.sum[x[3]])
  diff.mean <- abs(mean.left-mean.right)
  sprintf("|mean(y%s) - mean(y%s)| = %5.1f %s %s (H0:μ%s = μ%s is %s)", 
    x[1], x[2], round(diff.mean, 3), 
    ifelse(diff.mean>scheffe, '>', '<=')  , round(scheffe, 3), 
    x[1], x[2], ifelse(diff.mean>scheffe, 'reject', 'accept'))
})  


#################################
# 예제 7-9
#################################
alpha <- 0.05
groups <- 1:4
y.means <-                                       # (1) 그룹별 평균    
  apply(t(groups), 2, function(x) {
    mean(get(paste("y", x, sep="")))
  })
names(y.means) <- groups
(s.menas <- sort(y.means, decreasing=T))         # (2) 내림차순 정렬

diffs <- 4:2

for (dif in diffs) {                             # (3) Newman-Keuls 계산
  for (pos in groups) {
    if (pos+dif-1 <= length(s.menas)) {
      diff.mean <- s.menas[pos]-s.menas[pos+dif-1]
      
      reject <- 
        qtukey(1-alpha, nmeans=dif, df=n-K)
      
      n.left <- get(paste("n", names(s.menas[pos]), sep=""))
      n.right <- get(paste("n", names(s.menas[pos+dif-1]), sep=""))      
      nk <- if(n.left==n.right) reject*sqrt(MSE/n.left)
      else {
        harmomic.mean <- 2/(1/n.left + 1/n.right)
        reject*sqrt(MSE/harmomic.mean)
      }
      print(
      sprintf("|mean(y%s) - mean(y%s)| = %5.1f %s %7.3f (H0:μ%s=μ%s is %s)", 
        names(s.menas[pos]), names(s.menas[pos+dif-1]), round(diff.mean,3), 
        ifelse(diff.mean>nk, '>', '<='), round(nk, 3), 
        names(s.menas[pos]), names(s.menas[pos+dif-1]), 
              ifelse(diff.mean>nk, 'reject', 'accept')))   
    }
  }
}


#################################
# 예제 7-10
#################################
dunnett.cv <- 2.36                   # (1) Dunnett의 Critical Value
nc <- n1 

for (i in 2:4) {                     # (2) Dunnett 계산                 
  ni <- get(paste("n", i, sep=""))
  r <- if (ni==nc) ni
       else 2/(1/nc + 1/ni)
  dunnett <- dunnett.cv * sqrt(2*MSE/r)
  
  mean.left <- mean(get(paste("y", i, sep="")))
  mean.right <- mean(y1)
  diff.mean <- abs(mean.left-mean.right)
  
  print(
  sprintf("|mean(y%s) - mean(y%s)| = %5.1f %s %s (H0:μ%s = μ%s is %s)", 
    i, 1, round(diff.mean,3), 
    ifelse(diff.mean > dunnett, '>', '<=')  , round(dunnett, 3), 
    i, 1, ifelse(diff.mean > dunnett, 'reject', 'accept')))  
}


#################################
# 7.3 다중비교
#################################
install.packages("multcomp")                          # (1) multcomp 설치
library(multcomp)                                     # (2) multcomp 로드     

treatment <- rep(c("Y1","Y2","Y3","Y4"), c(5, 4, 5, 5))  # (3) 처리 그룹
concentration <- c(y1, y2, y3, y4)                    # (4) 혈청자료   

datas <- data.frame(treatment, concentration)         # (5) 데이터프레임 
head(datas, n=7)                                        

conc <- aov(concentration ~ treatment, data=datas)    # (6) aov 객체 
(conc.glht <- glht(conc, linfct=mcp(treatment="Tukey"))) # (7) glht 객체
summary(conc.glht)                                    # (8) 결과보기
plot(conc.glht)                                       # (9) 그래프


#################################
# 7.4.1 반복이 없는 이원분산분석 모형
#################################
#################################
# 예제 7-11
#################################
egg <- c(32, 38, 35, 42, 33, 37, 34, 41, 29, 40, 36, 39)
feed <- LETTERS[1:4]
weight <- c("180g", "190g", "200g")
(eggs <- matrix(egg, byrow=T, ncol=length(feed), dimnames=list(weight, feed)))

a <- ncol(eggs)					# (1) a
b <- nrow(eggs)					# (2) b

CT <- sum(eggs)^2 / (a*b)				# (3) CT

(SST <- sum(eggs^2) - CT) 			# (4) SST
(SSA <- sum(apply(eggs, 2, sum)^2/b) - CT)	# (5) SSA
(SSB <- sum(apply(eggs, 1, sum)^2/a) - CT)	# (6) SSB
(SSE <- SST - SSA - SSB)				# (7) SSE	

(df.A <- a-1)						# (8) A의 자유도
(df.B <- b-1)						# (9) B의 자유도
(df.E <- (a-1)*(b-1))				# (10) 오차항 자유도

(MSA <- SSA/df.A)					# (11) MSA
(MSB <- SSB/df.B)					# (12) MSB
(MSE <- SSE/df.E)					# (13) MSE

(F.A <- MSA/MSE)					# (14) A의 F0
(F.B <- MSB/MSE)					# (15) B의 F0

pf(F.A, df.A, df.E, lower.tail=F)    			# (16) A의 유의확률
pf(F.B, df.B, df.E, lower.tail=F)			# (17)	B의 유의확률

#################################
# 예제 7-11, aov 함수를 이용한 방법
#################################
egg <- c(32, 38, 35, 42, 33, 37, 34, 41, 29, 40, 36, 39)
feed <- rep(LETTERS[1:4], 3)
weight <- rep(c("180g", "190g", "200g"), each=4)
(eggs <- data.frame(egg, feed, weight))
summary(aov(egg~feed+weight, data=eggs))


#################################
# 7.4.2 반복이 있는 이원분산분석 모형
#################################
#################################
# 예제 7-12
#################################
output <- c(40, 35, 37, 43, 42, 38, 38, 41, 42, 37, 37, 41)
fertilizer <- c("15Kg", "20Kg")
water <- c("100l", "200l")
repetition <- paste("R", 1:3, sep=".")

(tomato <- array(output, dim=c(2, 2, 3), dimnames=list(water, fertilizer, repetition)))

a <- length(fertilizer)				# (1) a
b <- length(water)					# (2) b
c <- length(repetition)
  
CT <- sum(tomato)^2 / (a*b*c)			# (3) CT

(SST <- sum(tomato^2) - CT) 			# (4) SST
(SSA <- sum(apply(tomato, 2, sum)^2/(b*c)) - CT)	# (5) SSA
(SSB <- sum(apply(tomato, 1, sum)^2/(a*c)) - CT)	# (6) SSB
(SSAB <- sum(apply(tomato, c(1, 2), sum)^2/c) 
   - CT - SSA - SSB)                              	# (7) SSAB
(SSE <- SST - SSA - SSB - SSAB)		# (8) SSE	

(df.A <- a-1)						# (9) A의 자유도
(df.B <- b-1)						# (10) B의 자유도
(df.AB <- (a-1)*(b-1))  	                        	# (11) AB의 자유도
(df.E <- a*b*(c-1))				          # (12) 오차항 자유도

(MSA <- SSA/df.A)					# (13) MSA
(MSB <- SSB/df.B)					# (14) MSB
(MSAB <- SSAB/df.AB)  			          # (15) MSAB
(MSE <- SSE/df.E)					# (16) MSE

(F.A <- MSA/MSE)					# (17) A의 F0
(F.B <- MSB/MSE)					# (18) B의 F0
(F.AB <- MSAB/MSE)  			          # (19) AB의 F0

pf(F.A, df.A, df.E, lower.tail=F)    		        	# (20) A의 유의확률
pf(F.B, df.B, df.E, lower.tail=F)		       	# (21) B의 유의확률
pf(F.AB, df.AB, df.E, lower.tail=F)             	# (21) AB의 유의확률

means <- apply(tomato, c(1, 2), mean)            	# (22) 각 수준 조합의 평균
plot(x=rep(c(100, 200), 2), y=means, pch=16,      # (23) 그래프 그리기
     xlim=c(70, 230), ylim=c(32, 43), xlab="water(l)", 
     ylab="tomato outpot(kg)", xaxp=c(100, 200, 1))
lines(c(100, 200), means[1:2])                    	# (24) A1 line
lines(c(100, 200), means[3:4])                    	# (25) A2 line
text(100-15, means[1, 1], colnames(means)[1])       # (26) 15Kg 
text(100-15, means[1, 2], colnames(means)[2])       # (27) 20Kg
text(200+15, means[2, 1], colnames(means)[1])       # (28) 15Kg
text(200+15, means[2, 2], colnames(means)[2])       # (29) 20Kg


#################################
# 예제 7-12, aov 함수를 이용한 방법
#################################
output <- c(40, 35, 37, 43, 42, 38, 38, 41, 42, 37, 37, 41)
fertilizer <- rep(c("15Kg", "20Kg"), each=2, times=3)
water <- rep(c("100l", "200l"), 6)
(tomato <- data.frame(output, fertilizer, water))
summary(aov(output~fertilizer*water, data=tomato))

interaction.plot(tomato$fertilizer, tomato$water, tomato$output)


#################################
# 예제 7-13, aov 함수를 이용한 방법
#################################
output <- c(35, 38, 37, 43, 41, 41, 40, 42, 42, 37, 38, 37)
concentration <- rep(c("0.075", "0.1", "0.15", "0.2"), each=3)
(tomato <- data.frame(output, concentration))
summary(aov(output~concentration, data=tomato))



#################################
# 7.5. 난괴법
#################################

mu <- 500
alpha <- c(-70, -20, 40, 50)
beta <- c(-90, -10, 100)
n.alpha <- length(alpha)
n.beta <- length(beta)

Y <- matrix(0, nrow=n.alpha, ncol=n.beta)
set.seed(1)
errors <- rnorm(length(Y), mean = 0, sd = 5)

for(i in 1:n.alpha) {
  for(j in 1:n.beta) {
    Y[i,j] <- mu + alpha[i] + beta[j] + errors[(i-1)*n.beta+j]
  }
}

(Y <- round(Y))

(trt <- paste("trt", 1:4, sep=""))
(block <- paste("block", 1:3, sep=""))
dimnames(Y) = list(trt, block)
Y
(exam <- data.frame(Y=as.vector(Y), trt=rep(trt, 3), 
                    block=rep(block, each=4)))
anova(lm(Y ~ trt, data=exam))
qf(0.95, 3, 8)

anova(lm(Y ~ trt + block, data=exam))
qf(0.95, 3, 6)
qf(0.95, 2, 6)


feature <- c(13.1, 12.9, 13.4, 12.4, 12.7, 12.5, 12.3, 12.0, 12.2)      
temp <- c("240C", "250C", "260C")
block <- c("B1", "B2", "B3")
(fish.line <- data.frame(temp=rep(temp, each=3), block=rep(block, 3),
                        feature))
summary(aov(feature ~ temp + block, data=fish.line))




