##Simulation of true and observed Culex mean abundance 

##Load packages
library(VGAM)

##Sample size/area of study

M <- 45  ## number of sites
J <- 25 ## number of visits (1 x week)
C <- matrix(NA, nrow = M, ncol = J)


## True abundance = 50
lambda <- 50

## zero inflation = 0.6
N <- rzipois(n = M,lambda = lambda, pstr0 = 0.6)  ## draw samples from a zero-inflated Poisson distribution (rzipois).
N

## TRUE population
table(N) ## true abundance mosquito distribution

sum(N)  ##total number of mosquitoes

sum(N>0)  ## total number of occupied sites

mean(N)  ## true mean mosquito abundance


##generate local abundace (true population)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = 0.1)
}


## SAMPLING OF MOSQUITOES 

table(apply(C,1,max)) ## sampled mosquitoes

sum(apply(C,1,max)) ##total number of sampled mosquitoes

mean(apply(C,1,max)) ##mean of mosquitoes sampled



## True abundance = 100
lambda <- 100

## zero inflation = 0.6
N <- rzipois(n = M,lambda = lambda, pstr0 = 0.6)  ## draw samples from a zero-inflated Poisson distribution (rzipois).
N

## TRUE population
table(N) ## true abundance mosquito distribution

sum(N)  ##total number of mosquitoes

sum(N>0)  ## total number of occupied sites

mean(N)  ## true mean mosquito abundance


##generate local abundace (true population)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = 0.1)
}


## SAMPLING OF MOSQUITOES 

table(apply(C,1,max)) ## sampled mosquitoes

sum(apply(C,1,max)) ##total number of sampled mosquitoes

mean(apply(C,1,max)) ##mean of mosquitoes sampled



## True abundance = 200
lambda <- 200

## zero inflation = 0.6
N <- rzipois(n = M,lambda = lambda, pstr0 = 0.6)  ## draw samples from a zero-inflated Poisson distribution (rzipois).
N

## TRUE population
table(N) ## true abundance mosquito distribution

sum(N)  ##total number of mosquitoes

sum(N>0)  ## total number of occupied sites

mean(N)  ## true mean mosquito abundance


##generate local abundace (true population)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = 0.1)
}


## SAMPLING OF MOSQUITOES 

table(apply(C,1,max)) ## sampled mosquitoes

sum(apply(C,1,max)) ##total number of sampled mosquitoes

mean(apply(C,1,max)) ##mean of mosquitoes sampled



## True abundance = 300
lambda <- 300

## zero inflation = 0.6
N <- rzipois(n = M,lambda = lambda, pstr0 = 0.6)  ## draw samples from a zero-inflated Poisson distribution (rzipois).
N

## TRUE population
table(N) ## true abundance mosquito distribution

sum(N)  ##total number of mosquitoes

sum(N>0)  ## total number of occupied sites

mean(N)  ## true mean mosquito abundance


##generate local abundace (true population)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = 0.1)
}


## SAMPLING OF MOSQUITOES 

table(apply(C,1,max)) ## sampled mosquitoes

sum(apply(C,1,max)) ##total number of sampled mosquitoes

mean(apply(C,1,max)) ##mean of mosquitoes sampled



## True abundance = 400
lambda <- 400

## zero inflation = 0.6
N <- rzipois(n = M,lambda = lambda, pstr0 = 0.6)  ## draw samples from a zero-inflated Poisson distribution (rzipois).
N

## TRUE population
table(N) ## true abundance mosquito distribution

sum(N)  ##total number of mosquitoes

sum(N>0)  ## total number of occupied sites

mean(N)  ## true mean mosquito abundance


##generate local abundace (true population)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = 0.1)
}


## SAMPLING OF MOSQUITOES 

table(apply(C,1,max)) ## sampled mosquitoes

sum(apply(C,1,max)) ##total number of sampled mosquitoes

mean(apply(C,1,max)) ##mean of mosquitoes sampled



