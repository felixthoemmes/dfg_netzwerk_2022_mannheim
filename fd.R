#load libraries
library(dagitty) #for easy simulation
library(lavaan)
library(mediation)



#data-generating model for FD example
## Simulate data with pre-defined path coefficients
## true causal effect is exactly -.04

fd1model <- dagitty('dag{
                x -> m [beta=-.2] 
                m -> y [beta=.2] 
                x <-> y [beta=.6]}')
fd1df <- simulateSEM(fd1model,standardized = TRUE, N = 300, empirical = TRUE) 


#using regressions by hand
lm1 <- lm(m ~ x, data=fd1df)
lm2 <- lm(y ~ m + x, data=fd1df)
coef(lm2) * coef(lm1) #precisely recovered

#using mediation package
med1 <- mediate(model.m = lm1, model.y = lm2, treat = "x", mediator = "m")
summary(med1) #correctly recovered mediated effect - total effect incorrect

#using lavaan

fdm <- 'm ~ a*x
        y ~ b*m
        x ~~ y
        ce := a*b'

fdsem <- sem(fdm, data = fd1df)
summary(fdsem) #precisely recovered

