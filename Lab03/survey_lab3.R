## --------------------------------------------------------------------
seoul <- read.csv("lab3/seoul_covid.csv", header = TRUE)
head(seoul)


## --------------------------------------------------------------------
N <- length(unique(seoul$cluster))
n <- 11
m_i <- table(seoul$cluster)
M <- sum(table(seoul$cluster))
M_bar <- mean(table(seoul$cluster))


## --------------------------------------------------------------------
seoul$fpc <- rep(N, M) 
set.seed(0)
index <- sample(unique(seoul$cluster),n)
seoul_cluster <- seoul[seoul$cluster %in% index,]


## --------------------------------------------------------------------
library(survey)
onestage <- svydesign(id = ~cluster, data = seoul_cluster, fpc = ~fpc)
summary(onestage)

mu_est <- svymean(~y, design=onestage)
mu_est
confint(mu_est)
mean(seoul$y) # true mean


## --------------------------------------------------------------------
total_est2 <- mu_est[1] * M
total_est2 
confint(mu_est) * M
sum(seoul$y) # true total


## --------------------------------------------------------------------
total_est1 <- svytotal(~y, design=onestage)
total_est1 
confint(total_est1)
sum(seoul$y) # true total


## --------------------------------------------------------------------
set.seed(0)
cluster_size <- m_i[unique(seoul$cluster)]
seoul$pi <- n*cluster_size[seoul$cluster]/M
index <- sample(unique(seoul$cluster),n,prob=cluster_size)
seoul_cluster_pps <- seoul[seoul$cluster %in% index,]


## --------------------------------------------------------------------
onestage_pps <- svydesign(id = ~cluster, data = seoul_cluster_pps, probs = ~pi)
svymean(~y, design = onestage_pps) |> confint()
svytotal(~y, design = onestage_pps) |> confint()


## --------------------------------------------------------------------
colnames(seoul)[5] <- "fpc1"
ssu_length <- tapply(seoul$확진일, seoul$cluster, \(cl) length(unique(cl)))
seoul$fpc2 <- ssu_length[seoul$cluster]
set.seed(1)
#primary sampling
index1 <- sample(unique(seoul$cluster),n)
seoul_ps <- seoul[seoul$cluster %in% index1,]
#secondary sampling
m <- 15 # m_1 = ... = m_11 = 15
index2 <- tapply(seoul_ps$확진일, seoul_ps$cluster, \(cl) sample(unique(cl), m))
seoul_ss <- seoul_ps[seoul_ps$확진일 %in% unlist(index2),]

twostage <- svydesign(id = ~cluster + 확진일, data = seoul_ss, fpc = ~fpc1 + fpc2)
summary(twostage)


## --------------------------------------------------------------------
total_est <- svytotal(~y, design = twostage) 
total_est
confint(total_est)
sum(seoul$y) # true total


## --------------------------------------------------------------------
mu_est1 <- svymean(~y, design = twostage)
mu_est1
confint(mu_est1)
mean(tapply(seoul$y, seoul$확진일, sum)) # true mean 


## --------------------------------------------------------------------
M <- length(unique(seoul$확진일))
mu_est2 <- total_est[[1]]/M
mu_est2
confint(total_est)/M
mean(tapply(seoul$y, seoul$확진일, sum)) # true mean 

