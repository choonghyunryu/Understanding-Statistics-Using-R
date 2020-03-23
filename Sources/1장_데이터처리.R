##########################################
# validity and reliability
##########################################
circle <- function (shoot=10, mean=0, sd=1, main="") {
  n <- 100
  band <- 5:1
  angle <- (0:n)/n*2*pi
  x <-  band %o% cos(angle)
  y <-  band %o% sin(angle)
  
  cols <- c("white", "black", "blue", "red", "yellow")
  
  par(pty='s', mar = rep(0.7, 4))
  plot(c(-5, 5),c(-5, 5), type='n', axes=F, xlab='', ylab='') 
  title(main)
  
  for (i in rev(band)) {
    polygon(x[i, ], y[i, ], col=cols[i])
  }
  
  pos.x <- rnorm(shoot, mean, sd)
  pos.y <- rnorm(shoot, mean, sd)
  points(pos.x, pos.y, col="white", pch=16, cex=1.5)
}  

op <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
set.seed(0)
circle(20, 0, 1, "high validity and low reliability")
set.seed(1)
circle(20, 0, 0.2, "high validity and high reliability")
set.seed(3)
circle(20, 2, 1, "low validity and low reliability")
set.seed(2)
circle(20, 2, 0.2, "low validity and high reliability")
par(op)


##########################################
# 예제 1-1
##########################################
# (1) nominal scale
fruits <- c("apple", "pear", "pear", "apple", "banana")
fruits
fruits <- factor(fruits)
fruits
table(fruits)                               # 도수분포
table(fruits) / sum(table(fruits)) * 100    # 빈도 (백분율)
as.numeric(fruits)                          # 수준 인덱스

# (2) ordinal scale
grades <- c("middle", "high", "high", "high", "low", "low", "high")
grades
grades <- ordered(grades, levels=c("low", "middle", "high"))
grades
table(grades)                               # 도수분포
table(grades) / sum(table(grades)) * 100    # 빈도 (백분율)
median(as.numeric(grades))                  # 중위수 (수준 인덱스) 
levels(grades)                              # 수준의 이름
levels(grades)[median(as.numeric(grades))]  # 중위수 (수준이름)

# (3) interval scale
celsius <- c(23, 32, 18, 23, 22, 20)
celsius 
mean(celsius)                               # 산술평균
median(celsius)                             # 중위수
diff(celsius)                               # 두 값들의 차이(X_i - X_i-1)
celsius[2:6]/celsius[1:5]                   # 두 값들의 비율(X_i / X_i-1)

# (4) ratio scale
K <- celsius + 273.15                       # 절대 0 부여
K 
mean(K)                                     # 산술평균
median(K)                                   # 중위수
diff(K)                                     # 두 값들의 차이(X_i - X_i-1)
K[2:6]/K[1:5]                               # 두 값들의 비율(X_i / X_i-1)


typeof(T)                                   # (1) 논리 값
mode(TRUE)
storage.mode(FALSE) 
typeof(3.141592)                            # (2) 수치 값
mode(pi)
storage.mode(3e+07)
typeof(3L)                                   # (3) 정수 값
mode(3L)
storage.mode(-2)
typeof('star')                              # (4) 문자 값
mode('A')
storage.mode("R world")
typeof(4+2i)                                # (5) 복소수 값
mode(3i)
storage.mode(1-3i)


# vector
(vec <- 1:5)                                # 5개의 정수를 갖는 벡터
mode(vec)                                   # mode
length(vec)                                 # length
names(vec) <- paste("pos", vec, sep=".")    # names 지정
attributes(vec)                             # attributes


# matrix
(vec <- 1:12)                               # 12개의 정수를 갖는 벡터
(mat <- matrix(vec, nrow=3, ncol=4))        # 행렬 생성
mode(mat)                                   # mode
length(mat)                                 # length
nrow(mat)                                   # nrow
ncol(mat)                                   # ncol
dimnames(mat) <- 
  list(rowname=paste("row", 1:3, sep="."), 
       colname=paste("col", 1:4, sep=".")) # dimnames 지정
mat
dim(mat)                                   # dim
attributes(mat)                            # attributes


# array
(vec <- 1:24)                               # 12개의 정수를 갖는 벡터
(ary <- array(vec, dim=c(3,4,2)))           # 배열 생성
mode(ary)                                   # mode
length(ary)                                 # length
dimnames(ary) <- 
  list(rowname=paste("row", 1:3, sep="."),
       colname=paste("col", 1:4, sep="."), 
       matname=paste("mat", 1:2, sep=".")) # dimnames 지정
ary
dim(ary)                                   # dim
attributes(ary)                            # attributes


# list
child.cnt <- 2                             # vector
child.name <- c("Tom", "Jane")             # vector
child.age <- c(12, 9)                      # vector
(children <- list(child.cnt=child.cnt, child.name=child.name, 
                  child.age=child.age))    # list 생성
mode(children)                             # mode
length(children)                           # length


# factor
fruits <- c("apple", "pear", "pear", "apple", "banana")
(fruits <- factor(fruits))                  # factor 생성
mode(fruits)                                # mode
length(fruits)                              # length
attributes(fruits)                          # attributes
nlevels(fruits)                             # levels 개수
levels(fruits)                              # levels
grades <- c("middle", "high", "high", "high", "low", "low", "high")
(grades <- ordered(grades, levels=c("low", "middle", "high")))
mode(grades)                                # mode 
length(grades)                              # length
attributes(grades)                          # attributes
nlevels(grades)                             # levels 개수
levels(grades)                              # levels


# data frame
L3 <- LETTERS[1:4]                          # vector
(d <- data.frame(cbind(x=1, y=1:4), fac=L3))# data frame 생성
mode(d)                                     # mode
dim(d)                                      # dim
length(d)                                   # length
attributes(d)                               # attributes


# 외부 데이터 읽기 
path <- "Z:/05.R을 이용한 통계학이해_집필중/02.소스/datas"
ifile.name <- paste(path, "family_info.csv", sep="/") # 파일 이름 만들기
family <- read.csv(ifile.name)                        # csv 파일 읽어들이기       
family                                                # 결과 
dim(family)                                           # dim 
is.data.frame(family)                                 # 데이터 프레임 여부
is.factor(family$gender)                              # facotr 여부
is.ordered(family$age)                                # ordered factor 여부 
family$age <- ordered(family$age)                     # ordered facot로 변환
family$age                                            # 결과
is.ordered(family$age)                                # ordered factor 여부
is.numeric(family$freq)                               # numeric vector 여부


# R 객체 내보내기 
family.table <- xtabs(freq~gender+age, data=family)    # 분할표 만들기
family.table                                           # 결과
ofile.name <- paste(path, "family_table.csv", sep="/") # 파일 이름 만들기
write.csv(family.table, file=ofile.name)               # csv 파일로 출력  




