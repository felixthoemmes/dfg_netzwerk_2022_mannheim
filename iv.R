#load libraries
library(dagitty) #for easy simulation
library(AER)
library(fixest)
library(lavaan)



#data-generating model for IV example
## Simulate data with pre-defined path coefficients
## true causal effect is exactly .4

ivm <- dagitty('dag{z -> x [beta=.3] x -> y [beta=.4] x <-> y [beta=.3]}')
df1 <- simulateSEM(ivm,standardized = TRUE, N = 300, empirical = TRUE) 


#using regressions by hand
lm1 <- lm(x ~ z, data=df1)
lm2 <- lm(y ~ z, data=df1)
coef(lm2) / coef(lm1) #precisely recovered


#2SLS by hand using predicted values of lm1 as input to lm2
lm2slsh <- lm(y ~ predict(lm1), data=df1)
summary(lm2slsh) #precisely recovered


#using AER to do the same thing, but with robust standard errors
lmiv1 <- ivreg(y ~ x | z, data=df1)
summary(lmiv1) #precisely recovered with correct standard errors

#using fixest to estimate the same quantity 
lmiv2 <- feols(y ~ 1 | x ~ z,data =  df1)
summary(lmiv2) #precisely recovered with correct standard errors

#lavaan estimation
ivm <- 'x ~ z
        y ~ x
        x ~~ y'

ivsem <- sem(ivm,data=df1)
summary(ivsem)


###Exercise 1
ex1m <- dagitty('dag{
                z -> w [beta=-.2] 
                w -> y [beta=.2] 
                v -> y [beta=.2]
                x -> y [beta=.2]
                z <-> v [beta=.2]
                v <-> x [beta=.2]
                x <-> w [beta=-.2]
                x <-> y [beta=.2]}')
ex1df <- simulateSEM(ex1m,standardized = TRUE, N = 300, empirical = TRUE) 

#using AER
lmex1 <- ivreg(y ~ x + w + v| z + w + v, data=ex1df)
summary(lmex1) #precisely recovered with correct standard errors


#lavaan estimation
semiv1 <- 'x ~ z + w + v
        y ~ x + w + v
        x ~~ y'

semiv1fit <- sem(semiv1,data=ex1df)
summary(semiv1fit)

