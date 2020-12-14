library(readxl)
library(urca)
library(tsoutliers)
library(ggplot2)
library(purrr)
library(glmulti)
library(factoextra)
library(gridExtra)

database_project_trade <- read_excel("Documents/Master 2/Econométrie des Big Data /database project trade.xlsx")
View(database_project_trade)

dlbase <- database_project_trade [1:144,]
database_project_trade<- database_project_trade[,c(-1)]
database_project_trade<- database_project_trade[c(1:144),]


### 1) Analyse de la stationnarité
####### TEST ADF #############
##############################

noms<-as.null(c())
Liste_variables_stationaires<-data.frame()
for (n in names(database_project_trade[,c(1:28)])){
  Statio <- ur.df(as.ts(database_project_trade[,c(as.character(n))]), type=c("trend"), selectlags = "AIC")
  if (Statio@testreg[["coefficients"]]["tt","Pr(>|t|)"] < 0.05){
    if(Statio@teststat[1,1] < Statio@cval[1,"5pct"]){j<-"Stationnaire" } else {j<-"NON-Stationnaire" }
  } else { Statio <- ur.df(as.ts(database_project_trade[,c(as.character(n))]), type=c("drift"), selectlags = "AIC")}

  if(Statio@testreg[["coefficients"]]["(Intercept)","Pr(>|t|)"] < 0.05){
    if(Statio@teststat[1,1] < Statio@cval[1,"5pct"]){j<-"Stationnaire"} else { j<-"NON-Stationnaire"}
  } else{Statio <- ur.df(as.ts(database_project_trade[,c(as.character(n))]), type=c("none"), selectlags = "AIC")}

  if(Statio@teststat[1,1] < Statio@cval[1,"5pct"]){j<-"Stationnaire"} else {j<-"NON-Stationnaire"}

  DF<-Statio@teststat[1,1]
  stat<-Statio@cval[1,"5pct"]
  ligne<-c(j,DF,stat)
  Liste_variables_stationaires<-rbind(Liste_variables_stationaires,ligne)
  names(Liste_variables_stationaires)<-c("Stationarité","DF","Statistic(Seuil de 5%)")
  noms<-c(noms,n)
}
row.names(Liste_variables_stationaires)<-noms
print(Liste_variables_stationaires)

#2)
#### Analyse des outliers #########

#Pour les variables financières (méthode de Boudt et Ali)

#base_cleaned_boudt<-data.frame()
#j<-0
#for (n in names(dlbase[,c(1:26)])){
# y<-ts(dlbase[,c(as.character(n))])
#i<-Return.clean(y,method = "boudt")
#if(j==0){base_cleaned_boudt<-i} else{base_cleaned_boudt<-data.frame(base_cleaned_boudt,i)}
#j<-j+1
#}
#names(base_cleaned_boudt)<-names(dlbase[,c(1:26)])



# Pour les variables non-financières méthode de Chen et Liu (Ajustement Auto-ARIMA)
#-> Change toutes les valeurs après le point atypique pour suivre l'ARIMA ajusté sur toute la série
#-> Pas très bon pour des séries très volatiles


base_cleaned_chen<-data.frame()
j<-0
for (n in names(database_project_trade[,c(1:28)])){
  y<-ts(database_project_trade[,c(as.character(n))])
  fit<-tso(y)
  i<-fit$yadj
  if(j==0){base_cleaned_chen<-i} else{base_cleaned_chen<-data.frame(base_cleaned_chen,i)}
  j<-j+1
}
names(base_cleaned_chen)<-names(database_project_trade[,c(1:28)])
View(base_cleaned_chen)
names(base_cleaned_chen)


######
######3) Graphs des series en niveau et en serie transformées
################ En serie en niveau (avant différenciation)

#Statonnaire

dev.new()
par(mfrow=c(3,3))
plot(dlbase$Date,dlbase$Baltic_Dry_Index,xlab="Date",ylab="Baltic_Dry_Index",type="l",col="red")
plot(dlbase$Date,dlbase$Container_Index,xlab="Date",ylab="Container_Index",type="l",col="red")
plot(dlbase$Date,dlbase$Global_Economic_Policy,xlab="Date",ylab="Global_Economic_Policy",type="l",col="red")
plot(dlbase$Date,dlbase$Geopolitical_Risk,xlab="Date",ylab="Geopolitical_Risk",type="l",col="red")
plot(dlbase$Date,dlbase$Business_Tendency_Surveys,xlab="Date",ylab="Business_Tendency_Surveys",type="l",col="red")
plot(dlbase$Date,dlbase$Consumer_Sentiment,xlab="Date",ylab="Consumer_Sentiment",type="l",col="red")
plot(dlbase$Date,dlbase$CPI,xlab="Date",ylab="CPI",type="l",col="red")
plot(dlbase$Date,dlbase$GISS_Temperature,xlab="Date",ylab="GISS_Temperature",type="l",col="red")
plot(dlbase$Date,dlbase$GEPU_current,xlab="Date",ylab="GEPU_current",type="l",col="red")


dev.new()
par(mfrow=c(3,3))
plot(dlbase$Date,dlbase$GEPU_ppp,xlab="Date",ylab="GEPU_ppp",type="l",col="red")
plot(dlbase$Date,dlbase$Indice_Kilian,xlab="Date",ylab="Indice_Kilian",type="l",col="red")
plot(dlbase$Date,dlbase$Industrial_Capacity,xlab="Date",ylab="Industrial_Capacity",type="l",col="red")
plot(dlbase$Date,dlbase$Three_Component_Index,xlab="Date",ylab="Three_Component_Index",type="l",col="red")
plot(dlbase$Date,dlbase$News_Based_Policy_Uncert_Index,xlab="Date",ylab="News_Based_Policy_Uncert_Index",type="l",col="red")
plot(dlbase$Date,dlbase$VIX,xlab="Date",ylab="VIX",type="l",col="red")


#Non Stationnaire

dev.new()
par(mfrow=c(3,3))
plot(dlbase$Date,dlbase$world_trade_volume,xlab="Date",ylab="world_trade_volume",type="l",col="red")
plot(dlbase$Date,dlbase$Crude_Oil_Prices_Brent,xlab="Date",ylab="Crude_Oil_Prices_Brent",type="l",col="red")
plot(dlbase$Date,dlbase$WTI,xlab="Date",ylab="WTI",type="l",col="red")
plot(dlbase$Date,dlbase$Capacity_Utilization,xlab="Date",ylab="Capacity_Utilization",type="l",col="red")
plot(dlbase$Date,dlbase$Gold_Princing_Index,xlab="Date",ylab="Gold_Princing_Index",type="l",col="red")
plot(dlbase$Date,dlbase$Industrial_Production,xlab="Date",ylab="Industrial_Production",type="l",col="red")
plot(dlbase$Date,dlbase$M2,xlab="Date",ylab="M2",type="l",col="red")
plot(dlbase$Date,dlbase$OECD_6NME_Industrial_Production,xlab="Date",ylab="OECD_6NME_Industrial_Production",type="l",col="red")
plot(dlbase$Date,dlbase$Petroleum_and_other_liquids_stocks_US,xlab="Date",ylab="Petroleum_and_other_liquids_stocks_US",type="l",col="red")


dev.new()
par(mfrow=c(3,3))
plot(dlbase$Date,dlbase$Spread,xlab="Date",ylab="Spread",type="l",col="red")
plot(dlbase$Date,dlbase$Trade_Weighted_USD,xlab="Date",ylab="Trade_Weighted_USD",type="l",col="red")
plot(dlbase$Date,dlbase$Taux_change_effectif_USD,xlab="Date",ylab="Taux_change_effectif_USD",type="l",col="red")
plot(dlbase$Date,dlbase$US_Ending_Stocks_Crude_Oil,xlab="Date",ylab="US_Ending_Stocks_Crude_Oil",type="l",col="red")




### Boucle pour différencier et stationnariser les variables non stationnaires ###
nonstationnaire<-base_cleaned_chen[,c("world_trade_volume","Crude_Oil_Prices_Brent","WTI","Capacity_Utilization","Gold_Princing_Index","Industrial_Production", "M2","OECD_6NME_Industrial_Production","Petroleum_and_other_liquids_stocks_US","Spread","Trade_Weighted_USD","Taux_change_effectif_USD","US_Ending_Stocks_Crude_Oil")]

r<-nrow(nonstationnaire)
p<-ncol(nonstationnaire)


diff<- matrix(NA, nrow=(r-1), ncol = p)
res <- matrix(NA, nrow=r, ncol = p)

for (i in 1:ncol(nonstationnaire)){
  res <- diff(nonstationnaire[,i], lag=frequency(nonstationnaire[,i]), differences=1)
  diff[,i] <- cbind(as.matrix(res))
}
diff

diff <- data.frame(diff)
colnames(diff) <- colnames(nonstationnaire)


###*** !! Combiner les séries rendues stationnaires avec la base !!  ***###
basecombinee<-cbind(base_cleaned_chen[-1,-c(1,4,7,9,15,17,19,20,21,22,23,24,25)],diff)
basecombinee2 <- cbind(dlbase[2:144,c(1)],basecombinee)
newbase <- basecombinee2[c(1, 17, 2:16, 18:29)]

###*** !! Sauvegarder la base sous Excel !! ***###
library(xlsx)
write.xlsx(newbase, file ="Base Finale")




################ En serie transformée (après différenciation)

scatter_fun_n = function(x, y) {
  ggplot(newbase, aes(x = newbase[[x]], y = newbase[[y]]) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw() +
    labs(x = x,
         y = y)
}

scatter_fun_n("Date","M2")

expln = names(newbase)[1:29]
expln = set_names(expln)
elevn_plots = map(expln, ~scatter_fun_n("Date" ,.x ) )
elevn_plots

################################

#Statonnaire

dev.new()
par(mfrow=c(3,3))
plot(newbase$Date,newbase$Baltic_Dry_Index,xlab="Date",ylab="Baltic_Dry_Index",type="l",col="red")
plot(newbase$Date,newbase$Container_Index,xlab="Date",ylab="Container_Index",type="l",col="red")
plot(newbase$Date,newbase$Global_Economic_Policy,xlab="Date",ylab="Global_Economic_Policy",type="l",col="red")
plot(newbase$Date,newbase$Geopolitical_Risk,xlab="Date",ylab="Geopolitical_Risk",type="l",col="red")
plot(newbase$Date,newbase$Business_Tendency_Surveys,xlab="Date",ylab="Business_Tendency_Surveys",type="l",col="red")
plot(newbase$Date,newbase$Consumer_Sentiment,xlab="Date",ylab="Consumer_Sentiment",type="l",col="red")
plot(newbase$Date,newbase$CPI,xlab="Date",ylab="CPI",type="l",col="red")
plot(newbase$Date,newbase$GISS_Temperature,xlab="Date",ylab="GISS_Temperature",type="l",col="red")
plot(newbase$Date,newbase$GEPU_current,xlab="Date",ylab="GEPU_current",type="l",col="red")


dev.new()
par(mfrow=c(3,3))
plot(newbase$Date,newbase$GEPU_ppp,xlab="Date",ylab="GEPU_ppp",type="l",col="red")
plot(newbase$Date,newbase$Indice_Kilian,xlab="Date",ylab="Indice_Kilian",type="l",col="red")
plot(newbase$Date,newbase$Industrial_Capacity,xlab="Date",ylab="Industrial_Capacity",type="l",col="red")
plot(newbase$Date,newbase$Three_Component_Index,xlab="Date",ylab="Three_Component_Index",type="l",col="red")
plot(newbase$Date,newbase$News_Based_Policy_Uncert_Index,xlab="Date",ylab="News_Based_Policy_Uncert_Index",type="l",col="red")
plot(newbase$Date,newbase$VIX,xlab="Date",ylab="VIX",type="l",col="red")


#Non Stationnaire

dev.new()
par(mfrow=c(3,3))
plot(newbase$Date,newbase$world_trade_volume,xlab="Date",ylab="world_trade_volume",type="l",col="red")
plot(newbase$Date,newbase$Crude_Oil_Prices_Brent,xlab="Date",ylab="Crude_Oil_Prices_Brent",type="l",col="red")
plot(newbase$Date,newbase$WTI,xlab="Date",ylab="WTI",type="l",col="red")
plot(newbase$Date,newbase$Capacity_Utilization,xlab="Date",ylab="Capacity_Utilization",type="l",col="red")
plot(newbase$Date,newbase$Gold_Princing_Index,xlab="Date",ylab="Gold_Princing_Index",type="l",col="red")
plot(newbase$Date,newbase$Industrial_Production,xlab="Date",ylab="Industrial_Production",type="l",col="red")
plot(newbase$Date,newbase$M2,xlab="Date",ylab="M2",type="l",col="red")
plot(newbase$Date,newbase$OECD_6NME_Industrial_Production,xlab="Date",ylab="OECD_6NME_Industrial_Production",type="l",col="red")
plot(newbase$Date,newbase$Petroleum_and_other_liquids_stocks_US,xlab="Date",ylab="Petroleum_and_other_liquids_stocks_US",type="l",col="red")


dev.new()
par(mfrow=c(3,3))
plot(newbase$Date,newbase$Spread,xlab="Date",ylab="Spread",type="l",col="red")
plot(newbase$Date,newbase$Trade_Weighted_USD,xlab="Date",ylab="Trade_Weighted_USD",type="l",col="red")
plot(newbase$Date,newbase$Taux_change_effectif_USD,xlab="Date",ylab="Taux_change_effectif_USD",type="l",col="red")
plot(newbase$Date,newbase$US_Ending_Stocks_Crude_Oil,xlab="Date",ylab="US_Ending_Stocks_Crude_Oil",type="l",col="red")


######################################


#Question 4
#Statistiques déscriptive

statistiquesdescriptives<-summary(newbase)
statistiquesdescriptives
png("Stat.png", height = 30*nrow(statistiquesdescriptives), width = 80*ncol(statistiquesdescriptives))
grid.table(statistiquesdescriptives)
dev.off()


install.packages("summarytools")
library(summarytools)
view(dfSummary(newbase))


###Question 5
###Classification



###Question 6
###Corrélations


Tableau_Correlation<- round(cor(newbase[2:29]),2)
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


########################################################
###############   PARTIE 2 #############################
########################################################

# Penalized regressions
str(newbase)


# standardized y and x (centered and standardized)
y <- data.frame(newbase[,-1]) %>%
  select(world_trade_volume) %>%
  scale(center = T, scale = F) %>%
  as.matrix()

x <- data.frame(newbase[,-1]) %>%
  select(-world_trade_volume) %>% # on retire la variable ? expliquer y
  as.matrix()


#---------------------------------------------------------------------
# Ridge regression
# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 0, implementation of ridge regression
# choix du meilleur lambda parmi 100
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10)

# Figures of lambdas
plot(ridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- ridge_cv$lambda.min

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
summary(model_cv)

# Ridge betas
model_cv$beta


#---------------------------------------------------------------------
# Elastic-Net regression
# Choose alpha sequencially with 0 < alpha < 1

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- glmnet::cv.glmnet(x, y,  alpha = i, lambda = lambdas_to_try, standardize = T, nfolds = 10)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

# Figures of lambdas
#plot(cv)

# Implementation of EN regression
elasticnet_cv <- search[search$cvm == min(search$cvm), ]

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- elasticnet_cv$lambda.1se

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, lambda = elasticnet_cv$lambda.1se, alpha = elasticnet_cv$alpha)

# EN betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
# SCAD
library(ncvreg)
fit_SCAD=ncvreg(x, y, penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.10)

# Validation croisÈ pour le meilleur lambda
cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda
lambda_SCAD <- cvfit_SCAD$lambda.min

#Modele finale
SCAD_Final=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

which(! coef(SCAD_Final) == 0, arr.ind = TRUE)



#---------------------------------------------------------------------
# Adaptive Lasso regression

#LASSO
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)
coef_lasso<-predict(model_cv,type="coef",s=lambda_cv)

# Weighted with gamma = 0.5
gamma=0.5
w0<-1/(abs(coef_lasso) + (1/length(y)))
poids.lasso<-w0^(gamma)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.lasso)
fit_cv_adalasso<-cv.glmnet(x, y,penalty.factor=poids.lasso)

# Figure of lambdas
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- fit_cv_adalasso$lambda.1se

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)



#-----------------------------------------------------------------------
# Weighted fusion Lasso regressions

# Initialization of parameters
gamma=0.5
mu=0.1

# Compute pxQ matrix.
cor.mat <- cor(x)
abs.cor.mat <- abs(cor.mat)
sign.mat <- sign(cor.mat) - diag(2, nrow(cor.mat))
Wmat <- (abs.cor.mat^gamma - 1 * (abs.cor.mat == 1))/(1 -abs.cor.mat * (abs.cor.mat != 1))
weight.vec <- apply(Wmat, 1, sum)
fusion.penalty <- -sign.mat * (Wmat + diag(weight.vec))

# Compute Cholesky decomposition
R<-chol(fusion.penalty, pivot = TRUE)

# Transform Weighted Fusion in a Lasso issue sur les donn¥ees augment¥ees (1.40).
p<-dim(x)[2]
xstar<-rbind(x,sqrt(mu)*R)
ystar<-c(y,rep(0,p))

# Apply Lasso .
fit_wfusion<-glmnet(xstar,ystar)
fit_cv_wfusion<-cv.glmnet(xstar,ystar)

par(mfrow=c(1,2))
plot(fit_wfusion,xvar ="lambda",label = FALSE,col=T,xlab=expression(log(lambda)))
plot(fit_cv_wfusion,xlab=expression(log(lambda)))

min(fit_cv_wfusion$cvm)
lambda.opt<-fit_cv_wfusion$lambda.min

# We obtain the estimator of beta_weighted-fusion
fit_coef_wfusion<-predict(fit_wfusion,type="coef",s=lambda.opt)
show(fit_coef_wfusion)


library(readxl)
library(tidyverse)
library(msaenet)
library(glmnet)


## adaptive EN
# Choose alpha sequencially with 0 < alpha < 1
a <- seq(0.1, 0.9, 0.05)
aenet.fit <- aenet(x, y, family = "gaussian", init = "enet", alphas = a, tune = "cv", nfolds = 10, seed = 1001)
print(aenet.fit)
coef(aenet.fit)
aenet.fit$beta
which(! coef(aenet.fit) == 0, arr.ind = TRUE)	# Get the name of relevant variables

msaenet.nzv(aenet.fit)		# Get Indices of Non-Zero Variables
msaenet.fp(aenet.fit, 1:5)	# Get the Number of False Positive Selections
msaenet.tp(aenet.fit, 1:5)	# Get the Number of True Positive Selections

#----------------------------------------------------------------------------
## Adaptive SCAD-Net
a <- seq(0.1, 0.9, 0.05)
asnet.fit <- asnet(x, y, family = "gaussian", init = "snet", alphas = a, tune = "cv", nfolds = 10, seed = 1001)
print(asnet.fit)
coef(asnet.fit)
asnet.fit$beta
which(! coef(asnet.fit) == 0, arr.ind = TRUE)	# Get the name of relevant variables

msaenet.nzv(asnet.fit)		# Get Indices of Non-Zero Variables
msaenet.fp(asnet.fit, 1:5)	# Get the Number of False Positive Selections
msaenet.tp(asnet.fit, 1:5)	# Get the Number of True Positive Selections



#----------------------------------------------------------------------------
## Multi-Step Adaptive Elastic-Net
a <- seq(0.1, 0.9, 0.05)
msaenet.fit <- msaenet(x, y, family = "gaussian", init = "enet", alphas = a, tune = "cv", nfolds = 10, seed = 1001)
print(msaenet.fit)
coef(msaenet.fit)
msaenet.fit$beta
which(! coef(msaenet.fit) == 0, arr.ind = TRUE)	# Get the name of relevant variables

msaenet.nzv(msaenet.fit)		# Get Indices of Non-Zero Variables
msaenet.fp(msaenet.fit, 1:5)	# Get the Number of False Positive Selections
msaenet.tp(msaenet.fit, 1:5)	# Get the Number of True Positive Selections



#----------------------------------------------------------------------------
## Multi-Step Adaptive SCAD-Net (Est-ce la même chose que MD aSCAD ?)
a <- seq(0.1, 0.9, 0.05)
msasnet.fit <- msasnet(x, y, family = "gaussian", init = "snet", alphas = a, tune = "cv", nfolds = 10, seed = 1001)
print(msasnet.fit)
coef(msasnet.fit)
msasnet.fit$beta
which(! coef(msasnet.fit) == 0, arr.ind = TRUE)	# Get the name of relevant variables

msaenet.nzv(msasnet.fit)		# Get Indices of Non-Zero Variables
msaenet.fp(msasnet.fit, 1:5)	# Get the Number of False Positive Selections
msaenet.tp(msasnet.fit, 1:5)	# Get the Number of True Positive Selections

#------------------------------------------------------------------------
# !! ** ------ GETS ------ ** !!

# convert tibble in matrix for the function arx
str(newbase)

tbase <- newbase[,-1]
str(tbase)
class(tbase[,2:28]) #tibble

#class(newbase[,2:28])
mX = data.matrix(tbase[,2:28])
#str(mX)

# ARX model

# variables: 4, 11, 20, 21, 22 => render the matrix (mX) not singular: Removed!
Mod02world_trade_volume <- arx(tbase$world_trade_volume, mc = T, ar = 1, mxreg = mX[,c(1:27)], vcov.type = "white", normality.JarqueB=T)
Mod02world_trade_volume

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

