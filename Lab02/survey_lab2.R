## -------------------------------------------------------
subway <- read.csv("subway_snu2.csv", header = TRUE)
N <- nrow(subway)
mu <- mean(subway$number)


## -------------------------------------------------------
set.seed(0)
k <- 4   # N = nk, 184 = 46 * 4
n <- N/k # 46

# Method A
j <- sample(k, 1)
index <- seq(j, N ,k)
y.bar.sy <- mean(subway$number[index])
y.bar.sy

# Method B
j <- sample(N, 1)
index <- seq(j %% k, N, k) # does not matter even if the remainder is 0
y.bar.sy <- mean(subway$number[index])
y.bar.sy


## -------------------------------------------------------
# y: vector of length N = nk
# k: number of systematic samples
intra.cor <- function(y, k){
  N <- length(y)
  n <- N/k
  mu <- mean(y)
  class <- rep(1:k, N/k)
  rho <- 0
  for(i in 1:k){
    comb.index <- t(combn(which(class==i),2))
    rho <- rho + sum((y-mu)[comb.index[,1]] * (y-mu)[comb.index[,2]])
  }
  rho*2/(n-1)/(N-1)/var(y)
}

rho <- intra.cor(subway$number, k=4)
rho
intra.cor(subway$number, k=7)


## -------------------------------------------------------
fit <- anova(lm(subway$number ~ as.factor(rep(1:k,n))))
fit$`Sum Sq` # SSB, SSW
SSW <- fit$`Sum Sq`[2]
SST <- sum(fit$`Sum Sq`)
1 - n/(n-1)*SSW/SST # = rho


## -------------------------------------------------------
# Eq (1)
y.bar <- tapply(subway$number, rep(1:k, N/k), mean)
sum((y.bar-mu)^2)/k

# Eq (3)
var(subway$number)/n*(N-1)/N*(1+(n-1)*rho)


## -------------------------------------------------------
est.var.sy <- (N-n)/N*var(subway$number[index])/n
est.var.sy


## -------------------------------------------------------
n <- 30
index <- sample(N, n)
R <- sum(subway$number)/sum(subway$pass_in)
r <- sum(subway$number[index])/sum(subway$pass_in[index])
c(R, r)


## -------------------------------------------------------
var.ratio <- 1/mean(subway$pass_in)^2*(N-n)/(N*n)*
  var(subway$number-R*subway$pass_in)
est.var.ratio <- 1/mean(subway$pass_in[index])^2*(N-n)/(N*n)*
  var(subway$number[index]-r*subway$pass_in[index])
c(var.ratio, est.var.ratio)


## -------------------------------------------------------
r + c(-2,2)*sqrt(est.var.ratio)


## -------------------------------------------------------
library(sampling)
library(survey)
subway.srs <- subway[index,]
subway.srs.des <- svydesign(id = ~1, data = subway.srs, fpc = ~rep(N,n))
res <- svyratio(~number, ~pass_in, design = subway.srs.des)
res
confint(res)


## -------------------------------------------------------
data(iris)
fit <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(fit)


## -------------------------------------------------------
plot(iris$Sepal.Length, iris$Petal.Length)
abline(fit)


## -------------------------------------------------------
plot(fit)


## -------------------------------------------------------
fit2 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
summary(fit2)


## -------------------------------------------------------
mu.x <- mean(subway$pass_in)
mu.x


## -------------------------------------------------------
n <- 30
subway.srs <- subway[sample(N,n),]
fit <- lm(number ~ pass_in, data = subway.srs)
beta1.hat <- coef(fit)[[2]]
beta1.hat

mu.hat.yL <- mean(subway.srs$number) + beta1.hat*(mu.x - mean(subway.srs$pass_in))
mu.hat.yL


## -------------------------------------------------------
MSE <- sum(fit$residuals^2)/(n-2)
MSE
est.var.reg <- (N-n)/(N*n)*MSE
est.var.reg

