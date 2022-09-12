# sketch of how to approach power analysis for est mosquito control efficacy

# by Helen for Oswaldo et al

library(MASS)
library(tidyverse)
library(glmmTMB)
library(broom.mixed)

# parameter setup in a tibble:
simdata <- expand_grid(nbin_mu = c(1, 10),
                       nbin_theta = 20,
                       zeroinf_pi = .9,
                       nTraps = c(5, 25, 45),
                       nTrapNights = c(5, 8),
                       controlEfficacy = c(.8, .95),
                       sim_rep = seq(1, 100)) # will want sim_rep to be big

# functions for simulating ZINB according to parms:
# update these functions as desired (e.g. could include imperfect detection etc)
sim_pre <- function(nbin_mu, nbin_theta, zeroinf_pi, nTraps, nTrapNights) {
  
  simData <- tibble(Trap = rep(1:nTraps, times = nTrapNights),
                    Night = rep(1:nTrapNights, each = nTraps),
                    MosqCount = rnegbin(nTraps*nTrapNights, 
                                        nbin_mu, 
                                        nbin_theta) * 
                      rbinom(nTraps*nTrapNights, 
                             prob = 1 - zeroinf_pi, 
                             size = 1) *
                      rbinom(nbin_mu, 1, 0.38), # p=0.38 var(p)=0.236 (Rick Camp)
                    Treatment = "pre")
}



sim_post <- function(nbin_mu, nbin_theta, zeroinf_pi, nTraps, nTrapNights,
                     controlEfficacy) {
  
  simData <- tibble(Trap = rep(1:nTraps, times = nTrapNights),
                    Night = rep(1:nTrapNights, each = nTraps),
                    MosqCount = rnegbin(nTraps*nTrapNights, 
                                        nbin_mu * (1 - controlEfficacy), 
                                        nbin_theta) * 
                      rbinom(nTraps*nTrapNights, 
                             prob = 1 - zeroinf_pi, 
                             size = 1) *
                      rbinom(nbin_mu, 1, 0.38),
                    Treatment = "post")
}

##  Run it:
set.seed(7654)

simdata <- simdata %>%
  mutate(preControl = pmap(list(nbin_mu, 
                                nbin_theta,
                                zeroinf_pi,
                                nTraps,
                                nTrapNights), 
                           .f = sim_pre),
         postControl = pmap(list(nbin_mu, 
                                 nbin_theta,
                                 zeroinf_pi,
                                 nTraps,
                                 nTrapNights,
                                 controlEfficacy), 
                            .f = sim_post))

# look at one row:
simdata$preControl[[4]]
simdata$postControl[[4]]

##Pre and Post mosquito counts


plt.pre <- as.data.frame(do.call(rbind, simdata$preControl))
p.pre <- ggplot(plt.pre, aes(x=MosqCount)) +
  geom_histogram(binwidth=1) +
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(0:3, 100, 1000), minor_breaks = NULL)

p.pre

plt.post <- as.data.frame(do.call(rbind, simdata$postControl))
p.post <- ggplot(plt.post, aes(x=MosqCount)) +
  geom_histogram(binwidth=1) +
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(0:3, 100, 1000), minor_breaks = NULL)

p.post

##  Model outcomes: -----------------------------------------------------------

# create model function: replace with model that makes sense for data (this one is overparameterized bc data simulated w/o a trap effect)
control_model <- function(df) {
  m1 <- glmmTMB(MosqCount ~ Treatment + (1 | Trap),
                data = df,
                family = nbinom2,
                zi = ~ 1)
}

# super simple model just so it runs quickly 
plain_glm <- function(df) {
  m <- glm(MosqCount ~ Treatment, 
           data = df,
           family = nbinom2)
}


simModel <- simdata %>%
  # combine pre and post into one tibble
  mutate(PreAndPost = map2(preControl, postControl, bind_rows),
         # estimate a model for each row:
         mod = map(PreAndPost, plain_glm),
         # pull out the estimates
         tidyEst = map(mod, tidy)) %>%
  # make estimates and p values regular columns
  unnest(tidyEst) 


glimpse(simModel)
summary(simModel$mod[[1]])
simModel$tidyEst[[10]]

# example synthesis and plot:
simModel %>%
  filter(term == "Treatmentpre") %>%
  mutate(sig = ifelse(p.value < .05, 1, 0)) %>%
  group_by(nbin_mu, nTraps, nTrapNights, controlEfficacy) %>%
  summarize(nSig = sum(sig),
            n = n()) %>%
  ggplot() +
  geom_line(aes(nTraps, nSig, 
                group = nTrapNights,
                color = factor(nTrapNights))) +
  facet_grid(nbin_mu ~ controlEfficacy)