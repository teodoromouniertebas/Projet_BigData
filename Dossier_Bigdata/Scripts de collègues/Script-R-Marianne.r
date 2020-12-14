### Dossier Big Data ###

 # Library #
library(readxl)
library(tidyverse)
library(lgarch)
library(gets)
library(glmnet)
library(rbridge)
library(doParallel)
registerDoParallel(cores=4)
library(ggplot2)
library(purrr)
library(tseries)
library(forecast)
library(dplyr)
library(gridExtra)
library(dyn)
library(ncvreg)
library(car)

 # Import data #
dlbase <- read_excel("./Données/database-stock returns.xlsx")
dlbase <- data.frame(dlbase)

str(dlbase)

summary(dlbase)

dlbase <- na.omit(dlbase)

summary(dlbase) #979 observations

 # remove column INDEX:
dlbase <- dlbase[,-2]

 # **** EXPLORATORY ANALYSIS *** #

    # 1) Stationatrity

 # ADF Test #
    # H0: Unit Root -> Not stationnary
ndlbase <- dlbase[,-1]
tsdlbase <- ts(ndlbase)

    # Start LOOP #
pvalues <- NULL

for (i in 1:ncol(tsdlbase)){
 pvalues[i] <- adf.test(tsdlbase[,i])$p.value

}
pvalues

adf.pvalues <- data.frame(pvalues)
rownames(adf.pvalues) <- colnames(ndlbase)

status <- NULL
#TRUE : STAT
#FLASE: NOT STAT

for (i in 1:nrow(adf.pvalues)){
  if (adf.pvalues[i, "pvalues"]<0.05)
    status[i] <- "TRUE"
  else
    status[i] <- "FLASE"
}

status

adf.pvalues <- data.frame(adf.pvalues, status)
 # END LOOP #
# Not stationnary: D12, DP, DY, E12, bm, tbl, AAA, BAA, lty, Rfree, Ip & epu

#These variables need to be stationnarized:

# LOOP FOR 1st differenciation #
 # All variables that we have are rates -> stationary: only differenciation for D12, E12,IP

  # create a database with the ones not stationary as per TEST#
ns <- dlbase[,c("D12","E12","IP")]

r <- nrow(ns)
p <- ncol(ns)

diff <- matrix(NA, nrow=(r-1), ncol = p)
res <- matrix(NA, nrow=r, ncol = p)

for (i in 1:ncol(ns)){
  res <- diff(ns[,i], lag=frequency(ns[,i]), differences=1)
  diff[,i] <- cbind(as.matrix(res))
}
diff

diff <- data.frame(diff)
colnames(diff) <- colnames(ns)

# combine with the whole database
newbase <- cbind(dlbase[-1,-c(3,8,22)], diff)

#write.xlsx(newbase, file = "base_Marianne.xlsx", append = FALSE)


   # 2) Graphs
# Function for graphs: Before differenciation  #
scatter_fun = function(x, y) {
  ggplot(dlbase, aes(x = dlbase[[x]], y = dlbase[[y]]) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw() +
    labs(x = x,
         y = y)
}

scatter_fun("date", "DY")

expl = names(dlbase)[1:28]
expl <- set_names(expl)
elev_plots = map(expl, ~scatter_fun("date", .x) )
elev_plots

response = names(dlbase)[2]
response = set_names(response)
all_plots = map(response,
                ~map(expl, scatter_fun, y = .x) )
cowplot::plot_grid(plotlist = all_plots[[1]])

response_plots = map(all_plots, ~cowplot::plot_grid(plotlist = .x))
response_plots

# Function for graphs: After Differenciation #
scatter_fun_n = function(x, y) {
  ggplot(newbase, aes(x = newbase[[x]], y = newbase[[y]]) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw() +
    labs(x = x,
         y = y)
}

scatter_fun_n ("date","D12")
scatter_fun_n ("date","E12")
scatter_fun_n ("date","IP")

   # 3) Correlations

Tableau_Correlation<- round(cor(newbase[2:28]),2)
Tableau_Correlation
png("Tableau_Correlation.png", height = 30*nrow(Tableau_Correlation), width = 80*ncol(Tableau_Correlation))
grid.table(Tableau_Correlation)
dev.off()


corr.matrix<-matrix('',nrow=ncol(Tableau_Correlation),ncol=ncol(Tableau_Correlation))
for (i in 1:nrow(Tableau_Correlation)){
  for (j in 1:ncol(Tableau_Correlation)){
    if (Tableau_Correlation[i,j]==""){
      print(0)}else{
        if (as.numeric(Tableau_Correlation[i,j]>=abs(0.5))) {
          corr.matrix[i,j]=('Correlated')
        }else{
          corr.matrix[i,j]=('')
        }
      }
  }
}
colnames(corr.matrix)<-colnames(Tableau_Correlation)
rownames(corr.matrix)<-rownames(Tableau_Correlation)

write.table(corr.matrix, "corr_matrix.xls", col=NA, sep="\t",dec=".")

# ** VARIABLES SELECTION ** ##
 # Penalized regressions

str(newbase)

 # standardized y and x (centered and standardized)
y <- data.frame(newbase[,-1]) %>%
  select(RI) %>%
  scale(center = T, scale = F) %>%
  as.matrix()

x <- data.frame(newbase[,-1]) %>%
  select(-RI) %>% # on retire la variable ? expliquer y
  as.matrix()


 # I !! ** ------- Ridge Regression -------- ** !! #

# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambdas_to_try

# alpha = 0, implementation of ridge regression
# choose best lambda out of 100
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10)
ridge_cv$nzero

# Figures of lambdas
plot(ridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- ridge_cv$lambda.min
lambda_cv #0.001

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
summary(model_cv)

# Ridge betas
model_cv$beta

  # !! ** ------- LASSO regression ------- ** !!
# 10-folds CV to choose lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10)
lasso_cv$nzero
lasso_cv$lambda.min

# Figures of lambdas
plot(lasso_cv)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- lasso_cv$lambda.1se
lambda_cv #0.001450829

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)
model_cv

# Lasso betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)

  # !! ** ------ Elastic-Net regression ------ ** !! #
# with 0 < alpha < 1
# Choose alpha sequencially
a <- seq(0.1, 0.9, 0.05)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- glmnet::cv.glmnet(x, y,  alpha = i, lambda = lambdas_to_try, standardize = T, nfolds = 10)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

# Figures of lambdas
par(mfrow=c(1,1))
plot(cv)

# Implementation of EN regression
elasticnet_cv <- search[search$cvm == min(search$cvm), ]
elasticnet_cv

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- elasticnet_cv$lambda.1se
lambda_cv

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, lambda = elasticnet_cv$lambda.1se, alpha = elasticnet_cv$alpha)
summary(model_cv)

# EN betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)


  # !! ** ------ Adaptive Lasso regression ------- ** !! #

#LASSO
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10)
lambda_cv <- lasso_cv$lambda.1se

model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)
coef_lasso<-predict(model_cv,type="coef",s=lambda_cv)

# Weighted with gamma = 0.5
gamma=0.5
w0<-1/(abs(coef_lasso) + (1/length(y)))
poids.lasso<-w0^(gamma)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.lasso)
fit_adalasso
fit_cv_adalasso<-cv.glmnet(x, y,penalty.factor=poids.lasso)
fit_cv_adalasso

# Figure of lambdas
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv_adap <- fit_cv_adalasso$lambda.1se
lambda_cv_adap

# Evaluation of the final model with the selected lambda
model_cv_adap <- glmnet(x, y, alpha = 1, lambda = lambda_cv_adap, standardize = T)
model_cv_adap

# Lasso betas
model_cv_adap$beta

# Get the name of relevant variables
which(! coef(model_cv_adap) == 0, arr.ind = TRUE)

  # !! ** ------ Bridge regression ------ ** !! #
# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# choix du meilleur lambda parmi 100
bridge_cv <- cv.bridge(x, y, q=0.5, lambda = lambdas_to_try, nfolds = 10)

# Figures of lambdas
plot(bridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- bridge_cv$lambda.min
lambda_cv #0.001

# Evaluation of the final model with the selected lambda
model_cv <- bridge(x, y, q=0.5, lambda = lambda_cv)
summary(model_cv)

# Ridge betas
model_cv$beta

  # !! ** ------ GETS ------ ** !!

# convert tibble in matrix for the function arx
str(newbase)

tbase <- newbase[,-1]
str(tbase)
class(tbase[,2:27])

#class(newbase[,2:28])
mX = data.matrix(tbase[,2:27])
#str(mX)

# ARX model

 # variables: 4, 11, 20, 21, 22 => render the matrix (mX) not singular: Removed!
Mod02WTI <- arx(tbase$RI, mc = T, ar = 1, mxreg = mX[,c(1,2,3,5,6,7,8,9,10,12,13,14,15,16,17,18,19,23,24,25,26)], vcov.type = "white", normality.JarqueB=T)
Mod02WTI

Mod03WTI <- arx(tbase$RI, mc = T, ar = 1:4, arch=1:4, mxreg = mX[,c(1,2,3,5,6,7,8,9,10,12,13,14,15,16,17,18,19,23,24,25,26)], vcov.type = "white", normality.JarqueB=T)
Mod03WTI #not accepted

# GETS modelling
getsm_WTI2 <- getsm(Mod02WTI, arch.LjungB = NULL, ar.LjungB = NULL) #the test can block the estimation
getsm_WTI2

# GETS betas
coef.arx(getsm_WTI2)

# Get the name of relevant variables
names_mX <- names(coef.arx(getsm_WTI2))
names_mX <- names_mX[-1] # on retire le ar1
names_mX

# isat function
yy <- newbase[,2]
isat(yy, sis=TRUE, iis=FALSE, plot=TRUE, t.pval=0.005)
isat(yy, sis=FALSE, iis=TRUE, plot=TRUE, t.pval=0.005)
isat(yy, sis=FALSE, iis=FALSE, tis=TRUE, plot=TRUE, t.pval=0.005)
isat(yy, sis=TRUE, iis=TRUE, tis=TRUE, plot=TRUE, t.pval=0.005)

 # !! ** NEW APPROACH **  !!
### Selecting the Best lambda

xbis <- newbase[,-c(1,2)] #"ncvreg standardizes the data and includes an intercept by default".

SCADandMCP_cv <- cv.ncvreg(xbis, y, nfolds=10,returnY=FALSE, trace=FALSE)

# Figures of lambdas
plot(SCADandMCP_cv)
summary(SCADandMCP_cv)

lambda_cv <- SCADandMCP_cv$lambda.min
lambda_cv #0.0001117894

#### SCAD Approach ####

fit.SCAD <- ncvreg(xbis,y, family = c("gaussian", "binomial", "poisson"), penalty = c("SCAD"),lambda=lambda_cv)
fit.SCAD
cv.ncvreg
fit.SCAD$beta
# Get the name of relevant variables
which(! coef(fit.SCAD) == 0, arr.ind = TRUE)

#### MCP Approach ####
fit.MCP <- ncvreg(xbis,y, family = c("gaussian", "binomial", "poisson"), penalty = c("MCP"),lambda=lambda_cv)
fit.MCP
fit.MCP$beta
# Get the name of relevant variables
which(! coef(fit.MCP) == 0, arr.ind = TRUE)


 # ** Regression using OLS ** ##

tRI <- ts(newbase$RI)

RI_lag <- stats::lag(tRI,-1)

   # GETS #
ols.gets <- lm(tRI~DY+D.P+b.m+AAA+BAA+ntis+svar+gap+RI_lag, data=newbase)
summary(ols.gets) #R-sq =1: Overfitting
   # However: corr between DY and D.P =1 ==> multicolinnearity problem
   # Both variables are also highly correlated with b.m
   # The lag is causing multicollinearity problems

       # Test Multicolinearity #
vif(ols.gets) #there are aliased coefficients in the model

ols.gets.nonlag <- lm(tRI~DY+D.P+b.m+AAA+BAA+ntis+svar+gap, data=newbase)
summary(ols.gets.nonlag)

vif(ols.gets.nonlag)

ols.gets_DY <- lm(tRI~DY+ntis+svar+gap, data=newbase)
summary(ols.gets_DY)

ols.gets_DP <- lm(tRI~D.P+ntis+svar+gap, data=newbase)
summary(ols.gets_DP)

ols.gets_bm <- lm(tRI~b.m+ntis+svar+gap, data=newbase)
summary(ols.gets_bm)

ols.gets_AAA <- lm(tRI~AAA+ntis+svar+gap, data=newbase)
summary(ols.gets_AAA)

ols.gets_BAA <- lm(tRI~BAA+ntis+svar+gap, data=newbase)
summary(ols.gets_BAA)

   # Ridge #
ols.ridge <- lm(tRI~D.P+DY+EP+DE+b.m+tbl+AAA+BAA+lty+ntis+Rfree+infl+ltr+corpr+svar+CRSP_SPvw+CRSP_SPvwx+IPG+gap+tms+dfr+dfy+epu+D12+E12+IP+RI_lag, data=newbase)
summary(ols.ridge) #R-sq =1: Overfitting

  # LASSO & Adaptive LASSO #
ols.lasso <- lm(tRI~svar+CRSP_SPvw+CRSP_SPvwx+RI_lag, data=newbase)
summary(ols.lasso) #R-sq=1: overfitting + all coeff sign.

vif(ols.lasso)

ols.lasso.CRSP.vw <- lm(tRI~svar+CRSP_SPvw, data=newbase)
summary(ols.lasso.CRSP.vw) #R-sq=0.993

ols.lasso.CRSP.vwx <- lm(tRI~svar+CRSP_SPvwx, data=newbase)
summary(ols.lasso.CRSP.vwx) #R-sq=0.996

ols.lasso.lag <- lm(tRI~svar+RI_lag, data=newbase)
summary(ols.lasso.lag) #R-sq=1

  # Elastic-Net #
ols.en <- lm(tRI~D.P+svar+CRSP_SPvw+CRSP_SPvwx+EP+RI_lag, data=newbase)
summary(ols.en)

vif(ols.en)

ols.en.vw <- lm(tRI~D.P+svar+CRSP_SPvw+EP, data=newbase)
summary(ols.en.vw) #R-sq=0.994

ols.en.vwx <- lm(tRI~D.P+svar+CRSP_SPvwx+EP, data=newbase)
summary(ols.en.vwx) #R-sq=0.996

  # SCAD #
ols.scad <- lm(tRI~D.P+DY+EP+DE+svar+CRSP_SPvw+CRSP_SPvwx+ltr+lty+corpr+IPG+dfr+dfy+E12+Rfree+infl+RI_lag, data=newbase)
summary(ols.scad) #same

vif(ols.s) #coudln't pass

  # MPC #
ols.mpc <- lm(tRI~D.P+DY+EP+DE+ntis+svar+CRSP_SPvw+CRSP_SPvwx+ltr+lty+corpr+IPG+dfr+dfy+E12+Rfree+infl+RI_lag, data=newbase)
summary(ols.mpc) #same

vif(ols.s) #coudln't pass
