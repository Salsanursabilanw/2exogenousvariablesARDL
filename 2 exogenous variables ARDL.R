print("Hello")
#let's say we have the data under the name datav
library(readr)
data <- read_delim("datav.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)
view(datav)
attach(datav)
str(datav)
plot(datav) #time series plot
#load libraries we need
library(tseries)
library(ARDL)
library(lmtest)
library(car)
library(strucchange)
#Stationarity Test
pp.test(y)
pp.test(x1)
pp.test(x2)
#Stationarity test for first difference
pp.test(diff(y)) #Stationary
pp.test(diff(x1)) #Stationary
pp.test(diff(x2)) #Stationary
#ARDL Model
models = auto_ardl(y ~ x1 + x2, data= datav, max_order =5)
summary(models)
#Optimum Lag
models$top_order
#Building model with the best lag order
modelardl1 = models$best_model
summary(modelardl)
#cointegration test(Long-run levels relationship)
fbounds = bounds_f_test(modelardl, case= 3, alpha = 0.05)
fbounds$tab
uecmardl=uecm(modelardl1)
tbounds = bounds_t_test(uecmardl, case=3, alpha=0.01)
tbounds$tab #there is no cointegration
#assumption check
#autocorrelation test
bgtest(modelardl1)
#multicolinearity test
bptest(modelardl1)
#heteroskedastisity test
vif(modelardl1)
#Model Stability Test
modelt=efp(y ~ x1 + x2)
plot(modelt)
