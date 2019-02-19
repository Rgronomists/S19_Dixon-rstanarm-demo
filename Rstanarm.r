library(rstanarm)
library(lme4)
set.seed(583916502)

# simple linear regression
nobs <- 20
test <- data.frame(x=runif(nobs, 2, 10))
test$y <- 10 - 0.2*(test$x-5)^2 + rnorm(nobs,0,1)
test$x2 <- test$x^2
y.lm <- lm(y ~ x + x2, data=test)
summary(y.lm)$coefficients
confint(y.lm)

y.stan <- stan_glm(
    y ~ x + x2,
    family=gaussian,
    data=test,
    chains = 4,
    cores = 4
  )

library(parallel)
detectCores()

prior_summary(y.stan)

launch_shinystan(y.stan)

summary(y.stan, digits=2)

# slr on centered covariates
test$xc <- test$x - mean(test$x)
test$xc2 <- test$xc^2
yc.lm <- lm(y ~ xc + xc2, data=test)
summary(yc.lm)
confint(yc.lm)

yc.stan <- stan_glm(
    y ~ xc + xc2,
    family=gaussian,
    data=test,
    chains = 4,
    cores = 4
  )
summary(yc.stan, digits=2)

y.post <- as.matrix(yc.stan)
head(y.post)
plot(density(y.post[,3]), xlim=c(-2,0.5))

xmax <- -y.post[,2]/(2*y.post[,3])
plot(density(xmax), xlim=c(-4, 0.5))
summary(xmax)
quantile(xmax, c(0.025, 0.05, 0.5, 0.95, 0.975) )

# logistic regression example
donner <- read.csv('donner.csv')
donner.glm <- glm(survival ~ age + femc, data=donner,   family=binomial)
summary(donner.glm)
confint(donner.glm)
donner.stan <- stan_glm(
    survival ~ age + femc,
    family=binomial,
    data=donner,
    chains = 4,
    cores = 4
    )
summary(donner.stan, digits=2)

# probability that a male more likely to die than a female of same age= P[femc < 0]
donner.post <- as.matrix(donner.stan)
mean(donner.post[,3] < 0)

# incomplete block example
ib <- read.csv('IBtest.csv')
ib.stan <- stan_lmer(
    y ~ trt.f + (1 | block.f),
    data=ib,
    chains = 4,
    cores = 4
    )
ib.stan <- stan_lmer(
    y ~ trt.f + (1 | block.f),
    data=ib,
    chains = 4,
    cores = 4,
    adapt_delta = 0.98
    )
    
prior_summary(ib.stan)
summary(ib.stan, digits=2, 
  pars=c('(Intercept)', 'trt.fb', 'sigma'),
  regex_pars='Sigma*')

# overdispersed counts example
pod <- read.csv('PODtest.csv')
pod.glmm <- glmer(y ~ xc + (1|obs), data=pod, family=poisson)
summary(pod.glmm)
confint(pod.glmm)

pod.stan <- stan_glmer(
    y ~ xc + (1 | obs),
    data=pod,
    family=poisson,
    chains = 4,
    cores = 4
    )
summary(pod.stan, digits=2,
  pars=c('(Intercept)', 'xc'),
  regex_pars='Sigma*')
    
	