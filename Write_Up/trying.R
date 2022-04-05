if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(ggplot2)
library(Rcpp)
library(tseries)
library("dplyr")
install.packages("BVAR")
library(BVAR)
library(dplyr)
data <- read.delim("C:/Users/jesic/OneDrive/Desktop/Studies 2021/Second Semester/Time series/Project Dawie/Real/11_South Africa.txt")

## Cleaning DATA

GDP <- diff(data$y)*100
xa <- data[-1,]
Inflation <- xa$dp
Stock_Price <- diff(data$q)*100
Exports <- diff(data$ex)*100
Imports <- diff(data$im)*100
Oil <- diff(data$po)*100
Exchange_Rate <- diff(data$e)*100

dats <- cbind(GDP, Inflation, Stock_Price ,Exports,Imports,Oil,Exchange_Rate)
dataa <- ts(dats, start=c(1980,3), frequency = 4)
plot(dataa, main=" Representation of Data")

x <- as.data.frame(dataa)


## BVAR ANALYSIS

mn <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),alpha = bv_alpha(mode = 2), var = 1e07)

# Create a sum-of-coefficients prior
add_soc <-function(Y, lags, par) { soc <-if(lags == 1) {diag(Y[1, ]) / par} else { diag(colMeans(Y[1:lags, ])) / par }
Y_soc <-soc
X_soc <-cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))
return(list("Y" = Y_soc, "X" = X_soc)) }
soc <-bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_soc)


# Create a single-unit-root prior
add_sur <-function(Y, lags, par) { sur <-if(lags == 1) {Y[1, ] / par} else { colMeans(Y[1:lags, ]) / par }
Y_sur <-sur
X_sur <-c(1 / par, rep(sur, lags))
return(list("Y" = Y_sur, "X" = X_sur)) }
sur <-bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_sur) # Add the new custom dummy priors
bv_priors(hyper = "auto", soc = soc, sur = sur)


mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)


run <- bvar(x, lags = 10, n_draw = 90000, n_burn = 70000, n_thin = 1, priors = priors, mh = mh, verbose = TRUE)


print(run)

plot(run)
plot(run, type = "dens", vars_response = "Inflation", vars_impulse = "Inflation-lag1")

fitted(run, type = "mean")

plot(residuals(run, type = "mean"), vars = c("GDP", "Exchange_Rate", "Inflation", "Oil"))

opt_irf <- bv_irf(horizon = 16, identification = TRUE)

irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))

plot(irf(run), area = TRUE, vars_impulse = c("GDP", "Exchange_Rate", "Inflation", "Oil", "Exports", "Imports", "Stock_Price"), vars_response = c(1:2, 6))

predict(run) <- predict(run, horizon = 16, conf_bands = c(0.05, 0.16))
plot(predict(run), area = TRUE, t_back = 32, vars = c("Oil", "Exports", "Imports", "Stock_Price"))


library(forecast)
forecast(run)

## Lags
install.packages("vars")
library(vars)
lag <- VARselect(x, type="both")

## Stationary

library(tseries)
library(ggplot2)
library(dplyr)

detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)

adf.test(x$Output)
adf.test(x$Inflation)
adf.test(x$Stock_Price)
adf.test(x$Exports)
adf.test(x$Imports)
adf.test(x$Oil)
adf.test(x$Exchange_Rate)

old.par <- par(mfrow=c(4,2))
plot(acf(x$GDP, lag.max=20, plot=FALSE), ylab="Real GDP", main="")
plot(acf(x$Inflation, lag.max=20, plot=FALSE), ylab="Inflation", main="")
plot(acf(x$Stock_Price, lag.max=20, plot=FALSE), ylab="Real Stock Price Index" ,main="")
plot(acf(x$Exports, lag.max=20, plot=FALSE), ylab="Real Exports" ,main="")
plot(acf(x$Imports, lag.max=20, plot=FALSE), ylab="Real Imports" ,main="")
plot(acf(x$Oil, lag.max=20, plot=FALSE), ylab="Oil Price" ,main="")
plot(acf(x$Exchange_Rate, lag.max=20, plot=FALSE), ylab="Oil Price" ,main="")
par(old.par)


p1 <- pp.test(x$Output)
p3 <- pp.test(x$Inflation)
p4 <- pp.test(x$Stock_Price)
p6 <- pp.test(x$Exports)
p7 <- pp.test(x$Imports)
p8 <- pp.test(x$Oil)
p5 <- pp.test(x$Exchange_Rate)
