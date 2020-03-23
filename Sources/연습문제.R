#####################################
# 1.1
#####################################
# (1)
pass <- c("합격", "불합격", "불합격", "합격", "불합격")
pass <- factor(pass)
pass
# (2)
grade <- c("1등급", "2등급", "3등급", "2등급", "5등급", "3등급", "4등급", "4등급", "3등급", "3등급")
grade <- ordered(grade)
grade
# (3)
distance <- c(12, 23, 32, 22, 19, 21, 20) 
distance

#####################################
# 1.2
#####################################
# (1)
women
write.csv(women, "women.csv")
# (2)
women2 <- read.csv("women.csv")
women2

#####################################
# 2.1
#####################################
(data <- seq(-50, 50, 10))
(outlier <- seq(-300, 300, 100))

x <- apply(t(1:length(outlier)), 2, 
            function(x) c(data, outlier[x]))
dimnames(x) <- list(1:12, outlier)
x

# (1)
round(apply(x, 2, mean), 2)
apply(x, 2, median)

# (2)
boxplot(x, pch=16, main="mean vs median", xlab="outlier")
points(apply(x, 2, mean), pch=17, col=2)
legend(1, 250, c("outlier", "mean"), pch=16:17, col=1:2)


#####################################
# 2.2 (2)
#####################################
set.seed(2)
(x <- sample(10))
(n <- length(x))
(xbar <- mean(x))
(dev <- x-xbar)
(dev.sum <- cumsum(dev))

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(4, 4, 4, 1))
plot(x, pch=16, main="data and deviation")
abline(h=xbar, lty=1)
for (i in 1:n) {
  lines(c(i, i), c(xbar, x[i]), lty=2)
} 

plot(dev, pch=16,  ylim=range(c(dev, dev.sum)), 
     main="cumulative deviation")
abline(h=0, lty=1)

for (i in 1:n) {
  arrow.col <- sign(dev[i]) + 3
  if (i==1) {
    arrows(i, 0, i, dev.sum[i], length=0.15, col=arrow.col, lwd=2)
  }
  else {
    arrows(i, dev.sum[i-1], i, dev.sum[i], length=0.15, col=arrow.col, lwd=2)
    lines(c(i-1, i), c(dev.sum[i-1], dev.sum[i-1]), lty=2)
  }
} 
par(op)





#####################################
# 3.4
#####################################
curve(dnorm, xlim=c(-3,3), ylim=c(0, 0.45), 
  main="N(0,1) Quantile points", ylab="Density", xlab="z")
p <- 1:3/4
x <- qnorm(p)
y <- dnorm(x)
points(x, y, pch=16)
arrows(x, y, x, 0.006, col="red", code=2, length=0.1, lwd=1.8)
cardinal <- 1:3
ordinal <- paste(cardinal,ifelse(cardinal==1,'st',
  ifelse(cardinal==2, 'nd', 'rd')), sep="")
text(x,y, paste(ordinal,"Quartile"), pos=3)
text(x,0, round(x,2))

#####################################
# 3.5
#####################################
lambda <- 2
f1 <- function(x) dexp(x, rate=lambda)
f2 <- function(x) pexp(x, rate=lambda)
curve(f1, xlim=c(0,3), ylab="Density", 
  main=expression(X*"~"*Exp(lambda==2)))
curve(f2, add=T, col=2, lty=2)
legend(2, 1.8, c("pdf curve","cdf curve"), col=1:2, lty=1:2)

#####################################
# 3.6
#####################################
N <- 12; m <- 7; n <- 3		# female = m, male = N-m
x <- 2				# female count = x
dhyper(x, m, N-m, n)  		#(1)

N <- 50; m <- 25; n <- 10	# OK = m, Not OK = N-m
x <- 6				# OK count = x
dhyper(6,25,25,10)  		#(2)





#####################################
# 4.1
#####################################
df.chisq <- 9
(c1 <- qchisq(0.95, df=df.chisq))	# (1) F^-1(0.95; 9)
(c2 <- qchisq(0.05, df=df.chisq))	# (2) F^-1(0.05; 9)
pchisq(c1, df=df.chisq)-		# (3) P{c2<V<c1}
pchisq(c2, df=df.chisq)


#####################################
# 4.2
#####################################
# (1)
diff.abs <- function(df)
{
  f <- function(x) abs(dnorm(x)-dt(x, df=df))
  integrate(f, -3, 3)$val
}

mapply(diff.abs, 1:30)


# (2),(3)
x <- seq(-3, 3, by=0.1)
df.t <- c(1, 3, 5)

op <- par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(4, 4, 4, 1))
p.t <- sapply(FUN=function(df) dt(x, df), df.t)
matplot(x, p.t, type="l", ylim=c(0, 0.4), ylab="f(x)",
  main="t and Normal pdf")
p.norm <-  dnorm(x)
lines(x, p.norm, lty=4, col=4, lwd=2)
legend(2, 0.4, lty=1:4, lwd=c(1,1,1,2), col=1:4,
  c("t(1)","t(3)","t(5)","Z"), cex=0.8)

diff.p <- abs(p.t-p.norm)
matplot(x, diff.p, type="l", ylab="|pdf of z - pdf of t|",
  main="Difference between t and Normal pdf by df=1,3,5")
legend(2, 0.08, lty=1:3, col=1:3,
  c("t(1)","t(3)","t(5)"), cex=0.8)
par(op)



#####################################
# 4.3
#####################################
df1 <- 3
df2 <- 4

qf(0.95, df1, df2)			# (1) 

1/qf(1-0.95, df2, df1)			# (2)

f <- function(x) df(x, df1, df2)
integrate(f, 4, 7)$value		# (3)


#####################################
# 5.1
#####################################

#######
# (1)
#######
alpha <- 0.05
x <- USArrests$Murder			# ??????
(xbar <- mean(x))					# ǥ??????
n <- length(x)						# ?????? ??
(S <- sd(x))						# ǥ??ǥ??????
(CI <- xbar + qnorm(c(alpha/2,1-alpha/2))*S/sqrt(n))	# ?ŷڱ???
S.xbar <- S/sqrt(n) 						

f <- function(x) dnorm(x, mean=xbar, sd=S.xbar)		# pdf
fill <- function (f, interval, ...) {			# ???? ä???? ?Լ?
  x <- seq(interval[1], interval[2], length=100)
  y <- f(x)
  polygon(c(x, rev(x)), c(y, rep(0, length(y))), ...)
}

curve(f, xlim=c(xbar-3*S.xbar, xbar+3*S.xbar),		# pdf plot
  main=paste((1-alpha)*100, "% Confidence Interval Represented by Normal pdf", sep=""))

fill(f, CI, col="green") 					# ?ŷڼ??? ä????
text(xbar+2*S.xbar, dnorm(xbar, xbar, S.xbar),		
  paste("Confidence Level =", 1-alpha))
abline(v=xbar)
abline(h=0)
arrows(CI[1], dnorm(CI[1], xbar, S.xbar),			# ?ŷڱ??? ȭ??ǥ
  CI[2], dnorm(CI[2], xbar, S.xbar), lwd=2.5, length=0.1, code=3)
text(CI[1]-S.xbar/2, dnorm(CI[1], xbar, S.xbar), round(CI[1],5))
text(CI[2]+S.xbar/2, dnorm(CI[2], xbar, S.xbar), round(CI[2],5))
text(xbar, dnorm(xbar, xbar, S.xbar)/2, substitute(bar(X)==xbar, list(xbar=xbar)))

#######
# (2)
#######

mu <- 7							# ??=7	
(z <- (xbar - mu)/S.xbar)				# ??��???跮
(reject <- qnorm(alpha, lower.tail=F))			# ?Ⱒ??
(p.val <- pnorm(z, lower.tail=F))			# p-value
curve(dnorm, xlim=c(-3.5,3.5), 				# density plot
  main=expression("Rejection Region - "~list(H[0]:mu==7,H[1]:mu>7)))
abline(h=0)

fill(dnorm, c(reject, 4), col="green") 			# ?Ⱒ?? ä???? ?Լ?
text(-2, 0.35,"Significance Level = 0.05")	
arrows(reject, 0, reject, dnorm(reject), lwd=3, 	# z_0.05
  col=2, code=1, length=0.1)
text(reject+0.1, dnorm(reject), 			# z_0.05 ????
  substitute(z[0.05]==val, list(val=reject)), adj=0)

text(reject+0.5 , dnorm(reject)/2,			# Rejection Region
  adj=0, "Rejection Region")
arrows(z, 0, z, dnorm(z), lwd=3, col=4,
  length=0.1, code=1, lty=4)				# Z_0
text(z+0.1, dnorm(z), substitute(Z[0]==z, list(z=z)), adj=0)



#####################################
# 5.2
#####################################
alpha <- 0.05
sigma1.sq <- 8 
sigma2.sq <- 10
d <- 0.5
z.alpha <- qnorm(1-alpha/2)
ceiling((z.alpha^2*(sigma1.sq+sigma2.sq))/d^2)



#####################################
# 5.3
#####################################
boxplot(len ~ supp, data=ToothGrowth, horizontal=T,
  xlab="Tooth length", ylab="Supplement type")

diff.mean <- NULL
(diff.mean <- data.frame(
  method=
  c(t.test(len ~ supp, data=ToothGrowth)$method, 
    t.test(len ~ supp, data=ToothGrowth, var.equal=T)$method,
    wilcox.test(len ~ supp, data=ToothGrowth, exact=FALSE)$method,
    "Analysis of Variance",
    kruskal.test(len ~ supp, data=ToothGrowth)$method),
  p.value=
  c(diff.mean$p.value, t.test(len ~ supp, data=ToothGrowth)$p.value,
    t.test(len ~ supp, data=ToothGrowth, var.equal=T)$p.value,
    wilcox.test(len ~ supp, data=ToothGrowth, exact=FALSE)$p.value,
    anova(lm(len ~ supp, data=ToothGrowth))$"Pr(>F)"[1],
    kruskal.test(len ~ supp, data=ToothGrowth)$p.value)))



#####################################
# 5.4
#####################################
# 1)
N <- 1000
n <- 100
a <- t(c(0, 0.05, 0.1, 0.15, 0.2))

p <- c()
get.pval <- function (x) {
  for (i in 1:N) {
    rnd <- rnorm(n)+x
    p <- append(p, t.test(rnd)$p.value)
  }
  sort(p)
}

pval <- apply(a, 2 ,get.pval)

x <- 100*(1:N)/N
plot(x~I(x/100), type="n", ylab="percent", xlab="p-value",
  main="Empirical cdf of p-value")
i <- 0
apply(pval, 2, 
  function(p) {
    i <<- i + 1
    lines(x~p, col=i, lty=i, lwd=2)
  }
)
legend("bottomright", paste("rnorm", a, sep="+"),
  col=1:length(a), lty=1:length(a))
abline(v=0.05, lty=2)
abline(0, 100, lty=2)
text(0.05, 0 , "0.05", adj=0)

# 2)
p <- c()
n <- t(c(10, 100, 150, 500, 1000))
get.pval <- function (x) {
  for (i in 1:N) {
    rnd <- rnorm(x)+0.1
    p <- append(p, t.test(rnd)$p.value)
  }
  sort(p)
}

pval <- apply(n, 2 ,get.pval)

plot(x~I(x/100), type="n", ylab="percent", xlab="p-value",
  main="Empirical cdf of p-value")
i <- 0
apply(pval, 2, 
  function(p) {
    i <<- i + 1
    lines(x~p, col=i, lty=i, lwd=2)
  }
)
legend("bottomright", paste("n", n, sep="="),
  col=1:length(n), lty=1:length(n))
abline(v=0.05, lty=2)
abline(0, 100, lty=2)
text(0.05, 0 , "0.05", adj=0)


#####################################
# 6.1
#####################################
# 1)
pairs(cars, panel=panel.smooth, main="cars data",
  col = 2 + ifelse(cars$speed <= 10, 1, 
      ifelse(cars$speed < 20, 2, 3)), 
  pch = 14 + ifelse(cars$speed <= 10, 1, 
      ifelse(cars$speed < 20, 2, 3)))

# 2)
cor(x=cars$speed, y=cars$dist)
grp <- as.factor(ifelse(cars$speed <= 10, 1, 
         ifelse(cars$speed < 20, 2, 3)))
by(cars, grp, cor)


#####################################
# 6.2
#####################################
# 1)
lm.stack <- lm(stack.loss ~ stack.x)
summary(lm.stack)
step(lm.stack, direction="forward")

# 2)
esr.fit <- rstudent(lm.stack)                      
alpha <- 0.05             
n <- length(stack.loss)
k <- NCOL(stack.x)                         
(cirt <- qt(1-alpha/2, df=n-k-1, lower.tail=TRUE))
(idx <- which(abs(esr.fit) > cirt))               
stack.x[idx,]                                      

influence.measures(lm.stack)


#####################################
# 7.1
#####################################
# 1)
install.packages("HSAUR")
data("weightgain", package = "HSAUR")
interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain)

# 2)
summary(aov(weightgain ~ source + type + source:type, data=weightgain))
aov.weightgain <- aov(weightgain ~ source + type, data=weightgain)
summary(aov.weightgain)

#####################################
# 7.2
#####################################
# 1)
feature <- c(13.1, 12.9, 13.4, 12.4, 12.7, 12.5, 12.3, 12.0, 12.2)      
temp <- c("240C", "250C", "260C")
block <- c("B1", "B2", "B3")
(fish.line <- data.frame(temp=rep(temp, each=3), block=rep(block, 3),
                        feature))
fish.line.aov <- aov(feature ~ temp + block, data=fish.line)
summary(fish.line.aov)

# 2)
library(multcomp)   
(fish.glht <- glht(fish.line.aov, linfct=mcp(temp="Tukey"))) 
summary(fish.glht)
plot(fish.glht)

#####################################
# 8.1
#####################################
(VC <- ToothGrowth[ToothGrowth$supp=="VC", "len"])
(OJ <- ToothGrowth[ToothGrowth$supp=="OJ", "len"])
wilcox.test(VC, OJ, alternative = "less")


#####################################
# 8.2
#####################################
# 1)
with(sleep,extra[group==1])
with(sleep,extra[group==2])
(dif <- with(sleep, extra[group==2]-extra[group==1]))
wilcox.test(dif, mu=1.5, alternative="greater")

#2)
kruskal.test(extra~group, data=sleep) 


#####################################
# 8.3
#####################################
prun.table <- function(n1, n2) {
  arrangement <- function(n, n1) {
    (case <- choose(n, n1))
    (sets <- matrix(0, ncol=case, nrow=n1+n2))
    (m <- combn(n, n1))
    
    i <- 0
    combination <- function(x) {
      i <<- i+1
      vec <- rep(0, n1+n2)
      vec[m[,i]] <- 1
      vec
    }
    t(apply(sets, 2, combination))
  }
  
  arr <- arrangement(n1+n2, n1)
  
  run <- function(x) {
    sum(diff(x)!=0) + 1
  }
  
  runs <- apply(arr, 1, run)
  cumsum(table(runs)/length(runs))
}

prun.table(2, 3)
prun.table(5, 5)
