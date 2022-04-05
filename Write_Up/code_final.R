######################################################
######## Installing everything ######################
####################################################

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

#######################################################
############ Splitting Data ##########################
#######################################################


## TEST data from 2005:Q1

GDP_test <- diff(test$y)*100
xa_test <- data[-(1:99),]
Inflation_test <- xa_test$dp
Stock_Price_test <- diff(test$q)*100
Exports_test <- diff(test$ex)*100
Imports_test <- diff(test$im)*100
Oil_test <- diff(test$po)*100
Exchange_Rate_test <- diff(test$e)*100

la <- cbind(GDP_test, Inflation_test, Stock_Price_test,Exports_test,Imports_test,Oil_test,Exchange_Rate_test)
dataa <- ts(la, start=c(1980,3), frequency = 4)
plot(dataa, main=" Representation of Data")

test1 <- as.data.frame(dataa)
test1 <- test1[-(11:18),]

## Train data 

GDP <- diff(train$y)*100
xa <- data[-1,]
xb <-data[-(100:117),]
Inflation <- xb$dp
Stock_Price <- diff(train$q)*100
Exports <- diff(train$ex)*100
Imports <- diff(train$im)*100
Oil <- diff(train$po)*100
Exchange_Rate <- diff(train$e)*100

dats1 <- cbind(GDP, Inflation, Stock_Price ,Exports,Imports,Oil,Exchange_Rate)
dataa1 <- ts(dats1, start=c(1980,3), frequency = 4)
plot(dataa1, main=" Representation of Data")

train2 <- as.data.frame(dataa1)

################################################################################
################################## BVAR ########################################
################################################################################

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


run <- bvar(train2, lags = 10, n_draw = 90000, n_burn = 70000, n_thin = 1, priors = priors, mh = mh, verbose = TRUE)


print(run)

plot(run)
plot(run, type = "dens", vars_response = "Inflation", vars_impulse = "Inflation-lag1")

fitted(run, type = "mean")

plot(residuals(run, type = "mean"), vars = c("GDP", "Exchange_Rate", "Inflation", "Oil"))

opt_irf <- bv_irf(horizon = 16, identification = TRUE)

irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))

plot(irf(run), area = TRUE, vars_impulse = c("GDP", "Exchange_Rate", "Inflation", "Oil", "Exports", "Imports", "Stock_Price"), vars_response = c(1:2, 6))

see <- predict(run, horizon = 10)
plot(see, area = TRUE, t_back = 32, vars = c("GDP", "Inflation", "Oil"))

#####################################
############ VAR ####################
#####################################

install.packages("vars")
library(vars)
var1 <- VAR(train2, p=10, type="both")
var2 <- predict(var1, newdata=test1, horizon = 20, conf_bands = c(0.05, 0.16), P = 18)
plot(var2, area = TRUE, t_back = 32, vars = c("GDP", "Inflation", "Exchange_Rate"))


##################################
## Putting results into a table ##
##################################
# This works only for BVAR - not other

install.packages("BVARverse")
library(BVARverse)
library(dplyr)
library(tidyverse)

try <- tidy(see)

try1a <- try %>% spread("variable", value)

try21 <- try1a %>% filter(quantile %in% c("50"))

####################################################
#### Putting results into vectors for analysis ####
###################################################

gdp_bvar <- c(1.7448787, 1.7434149, 1.5251118, 1.2852046, 1.1148588, 0.8768851, 0.8314630, 0.8156417, 0.7431564, 0.7296073) 
gdp_var <- c(- 0.8908903, 0.1065763, -0.3492994, 0.4464542, 1.5528151, 2.1273837, 2.6274903, 2.6859277, 2.3765362, 2.5859514)
gdp_actual <- c(1.3641975, 1.2130095 , 0.9688178, 1.4387614, 1.4957687,  1.1248029, 1.5373958, 1.3361993, 0.8991781, 1.0947265)

infl_actual <- c(0.006368598, 0.005283041, 0.006773210, 0.002397284, 0.005726190, 0.009536616, 0.018747035, 0.010118324, 0.011258538, 0.017610026) 
infl_var <- c(-0.01265742, -0.01726220, -0.03529701, -0.03532820, -0.02663917, -0.03608311, -0.02982121, -0.03457038, -0.05109997, -0.05013784)
infl_bvar <- c( 0.004051628, 0.005142886, 0.008334394, 0.007580780, 0.007872427, 0.008883043, 0.007656673, 0.006849587, 0.007385887, 0.008543302) 

oil_var <- c(9.715215, -5.681085, -31.130308, -34.810443, -8.030314, -33.676059, -48.439894, -35.927783, -51.381779, -58.990796) 
oil_bvar <- c(3.83536202, 8.95983541, -0.01693524, 0.41643603, 5.41052479, 0.85631826, 0.23662992, 2.94611692, 1.18515221, 0.30833741) 
oil_actual <- c(8.0106647, 17.6070462, -7.7909729, 8.3794895, 12.0328698, 0.3763495, -16.0104983, -2.8129879, 16.8641448, 8.7831482)

#########################################################
#######Getting statistics for accuracy #################
#########################################################

library(Metrics)

mae(gdp_actual, gdp_bvar)
mae(gdp_actual, gdp_var)
mae(infl_actual, infl_bvar)
mae(infl_actual, infl_var)
mae(oil_actual, oil_bvar)
mae(oil_actual, oil_var)

sqrt(mean((gdp_actual - gdp_bvar)^2))
sqrt(mean((gdp_actual - gdp_var)^2))
sqrt(mean((infl_actual - infl_bvar)^2))
sqrt(mean((infl_actual - infl_var)^2))
sqrt(mean((oil_actual - oil_bvar)^2))
sqrt(mean((oil_actual - oil_var)^2))

mean((gdp_actual - gdp_bvar)^2)
mean((gdp_actual - gdp_var)^2)
mean((infl_actual - infl_bvar)^2)
mean((infl_actual - infl_var)^2)
mean((oil_actual - oil_bvar)^2)
mean((oil_actual - oil_var)^2)

install.packages("DescTools")
library(DescTools)

## TheilU stats

TheilU(gdp_actual, gdp_bvar, type=c(1))
TheilU(gdp_actual, gdp_var, type =c(1))
TheilU(infl_actual, infl_bvar, type = c(1))
TheilU(infl_actual, infl_var, type = c(1))
TheilU(oil_actual, oil_bvar, type=c(1))
TheilU(oil_actual, oil_var, type = c(1))

TheilU(gdp_actual, gdp_bvar, type=c(2))
TheilU(gdp_actual, gdp_var, type =c(2))
TheilU(infl_actual, infl_bvar, type = c(2))
TheilU(infl_actual, infl_var, type = c(2))
TheilU(oil_actual, oil_bvar, type=c(2))
TheilU(oil_actual, oil_var, type = c(2))

######################################
############ Lags####################
####################################

install.packages("vars")
library(vars)
lag <- VARselect(x, type="both")

#####################################
############ Stationary #############
#####################################

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


