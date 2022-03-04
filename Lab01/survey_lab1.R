## ------------------------------------------------
sample(5)


## ------------------------------------------------
set.seed(0)
sample(5)


## ------------------------------------------------
sample(c('A','B','C','D','A'))


## ------------------------------------------------
sample(c('A','B','C','D','A'), 3)
sample(10, 3)


## ------------------------------------------------
sample(10, replace=TRUE)


## ------------------------------------------------
sample(10, 3, replace=TRUE)


## ------------------------------------------------
sample(45, 6)


## ------------------------------------------------
subway <- read.csv("./lab1/subway_snu.csv", header = TRUE)
dim(subway) 
N <- 184
head(subway, 7)
mu <- mean(subway$number) # population mean
sigma.sq <- var(subway$number)*(N-1)/N # population variance
c(mean=mu, sd=sqrt(sigma.sq))


## ------------------------------------------------
set.seed(0)
n <- 30
sample.idx <- sample(N,n)
sample.subway <- subway[sample.idx,]
y.bar <- mean(sample.subway$number)
y.bar # estimate of the population mean


## ------------------------------------------------
s.sq <- var(sample.subway$number)
est.var <- (N-n)/(N*n)*s.sq
est.var


## ------------------------------------------------
alpha <- 0.05
lower <- y.bar + qnorm(alpha/2)*sqrt(est.var)
upper <- y.bar - qnorm(alpha/2)*sqrt(est.var) 
c(lower=lower, upper=upper) # 95% confidence interval


## ------------------------------------------------
count <- 0
for(i in 1:1000){
  sample.idx <- sample(N,n)
  sample.subway <- subway[sample.idx,]
  y.bar <- mean(sample.subway$number)
  s.sq <- var(sample.subway$number)
  est.var <- (N-n)/(N*n)*s.sq
  lower <- y.bar + qnorm(alpha/2)*sqrt(est.var)
  upper <- y.bar - qnorm(alpha/2)*sqrt(est.var)
  if((lower < mu) & (mu < upper)) count <- count + 1
}
count / 1000


## ------------------------------------------------
plot(1:31, subway$number[1:31], type="b", xlab="March", ylab="Number of passengers")


## ------------------------------------------------
subway$group <- ifelse(subway$day=="SAT",2,ifelse(subway$day=="SUN",3,1))
N.vector <- table(subway$group)
N.vector


## ------------------------------------------------
n1 <- 10
n2 <- 10
n3 <- 10
n.vector <- c(n1, n2, n3)

# install.packages("sampling")
library(sampling)

set.seed(0)
strata.subway <- sampling::strata(subway, "group", size=n.vector, method="srswor")
strata.subway2 <- getdata(subway, strata.subway)


## ------------------------------------------------
# install.packages("survey")
library(survey)
mydesign <- svydesign(ids=~1, strata=~group, data=strata.subway2, fpc=~rep(N.vector,each=10))
res <- svymean(~number, design=mydesign)
res
confint(res)


## ------------------------------------------------
y.bar.vec <- tapply(strata.subway2$number, strata.subway2$group, mean)
y.bar.vec
w.j <- N.vector/N
y.bar.st <- sum(y.bar.vec*w.j)
y.bar.st


## ------------------------------------------------
s.sq.vec <- tapply(strata.subway2$number, strata.subway2$group, var)
est.var.st <- sum(w.j^2*(N.vector-n.vector)/(N.vector*n.vector)*s.sq.vec)
est.var.st


## ------------------------------------------------
alpha <- 0.05
lower.st <- y.bar.st + qnorm(alpha/2)*sqrt(est.var.st)
upper.st <- y.bar.st - qnorm(alpha/2)*sqrt(est.var.st) 
c(lower=lower.st, upper=upper.st) # 95% confidence interval

