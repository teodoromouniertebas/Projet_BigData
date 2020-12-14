#quelques graphiques stylé sur le sujet:

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
library(rio)




# Core Packages
library(rsample)
library(knitr)
# Data Cleaning
library(janitor)
# EDA
library(skimr)
library(DataExplorer)



df_fret_clean <- import("./Données/base_clean.xlsx" )
str(df_fret_clean)

df_fret <- read_excel("./Données/database project fret.xlsx")
df_fret <- data.frame(df_fret)


#Summary générale :
library(summarytools)
summarytools::descr(df_fret_clean, style = "rmarkdown")
view(descr(df_fret_clean))

#Graphique des variable manquante sur la base df_fret
library(DataExplorer)
DataExplorer::plot_missing(df_fret)


#Graphique histogramme bien fini
library(scales)
theme_set(theme_tq())
library(DataExplorer)
view(DataExplorer::plot_histogram(df_fret, ggtheme = theme_tq()))
view(DataExplorer::plot_histogram(df_fret_clean, ggtheme = theme_tq()))







#---------------------------------------------------------




df_fret_clean <- import("./Données/base_clean.xlsx" )
str(df_fret_clean)

df_fret <- read_excel("./Données/database project fret.xlsx")
df_fret <- data.frame(df_fret)




#Variables explicatives

library(ggplot2)
library(reshape2)
library(gganimate)

#Representation de graphique pour dont l'information est proches


  #Graphique 1 la variables à expliquer BDI



df1 <- df_fret %>% select(1,2)
p1 <- ggplot(df1, mapping = aes(Date,Baltic.Dry.Index,color=Baltic.Dry.Index)) +
    geom_line()+
    labs(title ="Evolution du Baltic Dry Index",
    y = "Cours") +
    transition_reveal(Date)+
    theme_minimal()
p1

library(plotly)
plot_ly(data=df_fret, x = ~Date , y = ~Baltic.Dry.Index, mode = 'lines')

df11 <- df_fret_clean %>% select(1,2)
p11 <- ggplot(df11, mapping = aes(Date,Baltic.Dry.Index,color=Baltic.Dry.Index)) +
    geom_line() +
    labs(title ="Evolution du Baltic Dry Index stationnarisé",
    y = "Cours") +
    theme_minimal()

p11


library(plotly)
plot_ly(data=df_fret, x = ~Date , y = ~Baltic.Dry.Index, mode = 'lines')



  #Graphique 2 variables explicative d'indice qui apporte la même info :


df2 <- melt(df_fret[,c(1,27,28)], id="Date")


p2 <- ggplot(df2) + geom_line(aes(x=Date, y=value, color=variable)) +
    scale_colour_manual(values = c(Three_Component_Index = "blue", News_Based_Policy_Uncert_Index = "red")) +
    labs(title ="Evolution de variables d'indices",
    y = "Cours") +
    theme_minimal()
p2

  #Graphique 3 variables explicatives totatlement corrélé entre elle :

df3 <- melt(df_fret[,c(1,14,15)], id="Date")


p3 <- ggplot(df3) + geom_line(aes(x=Date, y=value, color=variable)) +
    scale_colour_manual(values = c(GEPU_current = "blue", GEPU_ppp = "red")) +
    labs(title ="Evolution des variables explicatives GEPU_current et GEPU_ppp",
    y = "Cours") +
    theme_minimal()
p3

