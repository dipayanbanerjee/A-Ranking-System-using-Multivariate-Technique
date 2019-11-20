rm(list = ls())

getwd()
setwd("~/Documents/Sem-4/Minor Thesis")


# Required Packeges
library(magrittr)
library(dplyr)
library(data.table)
library(graphics)
library(STAT)

# Reading data

#-----------------------------------------------Data till Match 30------------------------------------------
library(readxl)

df.forward <- read_excel(path = 'Rating_Round_30.xlsx',sheet = "Forward-Rating")
View(df.forward)

df.midfield <- read_excel(path = 'Rating_Round_30.xlsx',sheet = "Midfield-Rating")
View(df.midfield)

df.defence <- read_excel(path = 'Rating_Round_30.xlsx',sheet = "Defence-Rating")
View(df.defence)

df.goalkeeper <- read_excel(path = 'Rating_Round_30.xlsx',sheet = "Goalkeeper-Rating")
View(df.goalkeeper)

#----------------------------------------------Data till Match 36--------------------------------------

df.forward <- read_excel(path = 'Player_data.xlsx',sheet = "Forward")
#View(df.forward)

df.midfield <- read_excel(path = 'Player_data.xlsx',sheet = "Midfielder")
#View(df.midfield)

df.defence <- read_excel(path = 'Player_data.xlsx',sheet = "Defence")
#View(df.defence)

df.goalkeeper <- read_excel(path = 'Player_data.xlsx',sheet = "Goalkeeper")
#View(df.goalkeeper)

# Data Reduction

pca.goalkeeper <- df.goalkeeper[,c(-1,-2,-3)]
#View(pca.goalkeeper)

pca.forward <- df.forward[,c(-1,-2,-3)]
#View(pca.forward)

pca.midfielder <- df.midfield[,c(-1,-2,-3)]
#View(pca.midfielder)

pca.defence <- df.defence[,c(-1,-2,-3)]
#View(pca.defence)


# -----------------------------------------------Finding the Correlation-------------------------------
library(purrr)
library(corrplot)
library(caret)

forward.numeric <- pca.forward %>% keep(is.numeric)
midfielder.numeric <- pca.midfielder %>% keep(is.numeric)
defence.numeric <- pca.defence %>% keep(is.numeric)
goalkeeper.numeric <- pca.goalkeeper %>% keep(is.numeric)

cormat.1 <- cor(forward.numeric)
corMat.2 <- cor(midfielder.numeric)
corMat.3 <- cor(defence.numeric)
corMat.4 <- cor(goalkeeper.numeric)


corrplot(cormat.1, order = "hclust",method  = "number")
corrplot(corMat.2,order = "hclust")
corrplot(corMat.3,order = "hclust")
corrplot(corMat.4,order = "hclust")



highlyCor <-findCorrelation(corMat, cutoff = 0.6, verbose = TRUE, names = TRUE)
highlyCor

# -------------------------------------------------PCA and Factor Analysis--------------------------------------------------
library(psych)
library(GPArotation) # For Oblique rotation
library(FactoMineR)
library(factoextra)
library(BBmisc) # For interpolation

x$Structure # Factor defining the variable
x$Vaccounted # Factor summary
x$np.obs # No. of observation of each factor
x$r # Correlation matrix
x$dof
x$chi
x$nh
x$rms
x$EPVAL
x$residual
x$scores # Factor Scores of individual



# -------------------------------Goalkeeper-------------------

#-------------------------PCA----------------------------------

pca.1 <- PCA(pca.goalkeeper,scale.unit = TRUE,graph = TRUE)
print(pca.1)

eig_val.1 <- get_eigenvalue(pca.1)
print(eig_val.1)

fviz_eig(pca.1,addlabels = TRUE,ylim=c(0,50),main = 'Goalkeeper') # screeplot using the eigenvalue

fviz_pca_var(pca.1,col.var = "black")

#-------------------------Factor Analysis-----------------

#---------------------Week-30------------------------
fact.1 <- fac(pca.goalkeeper,nfactors = 2,rotate = "promax",scores = "Thurstone") # 3/5
fact.1$scores
fact.1$Structure
fact.1$Vaccounted # Explain variance of each factor.

#---------------------Week-36------------------
fact.1 <- fac(pca.goalkeeper,nfactors = 3,rotate = "promax",scores = "Thurstone") # 3/5
fact.1$scores
fact.1$Structure
fact.1$Vaccounted 

#----------------------Comprehensiove Score and Rating------------------------
goalkeeper.Rating <- df.goalkeeper[,c(1,2,3)]
goalkeeper.Rating <- cbind(goalkeeper.Rating,fact.1$scores)
View(goalkeeper.Rating)

goalkeeper.Rating[,c(4,5)] <- scale(goalkeeper.Rating[,c(4,5)])

#-------------CS and Rating from Round 30 data
goalkeeper.Rating$CS <- (0.831*goalkeeper.Rating$MR1) + (0.17*goalkeeper.Rating$MR2)
goalkeeper.Rating$Rating <- normalize(goalkeeper.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
goalkeeper.Rating$Rating <- round(goalkeeper.Rating$Rating,1)

#-------------CS and Rating from Round 36 data

goalkeeper.Rating$CS <- ((0.618*goalkeeper.Rating$MR1) + (0.18*goalkeeper.Rating$MR2) + (0.2*goalkeeper.Rating$MR3))
goalkeeper.Rating$Rating <- normalize(goalkeeper.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
goalkeeper.Rating$Rating <- round(goalkeeper.Rating$Rating,1)

goalkeeper.Rating <- goalkeeper.Rating[,c(1,2,3,4,5,6,8,7)]

View(goalkeeper.Rating)

write.csv(goalkeeper.Rating,file = 'Goalkeeper_rating.csv',sep = ",")



# -------------------------------------------------Forward-----------------------------------------------------

pca.2 <- PCA(pca.forward,scale.unit = TRUE,graph = TRUE)
print(pca.2)

eig_val.2 <- get_eigenvalue(pca.2)
print(eig_val.2)

fviz_eig(pca.2,addlabels = TRUE,main = "Forward")
fviz_pca_var(pca.2,col.var = "black")

#-----------------------------------------------Factor Analysis--------------------------------------------

#------------------Week-30---------------
fact.2 <- fac(pca.forward,nfactors = 3,rotate = "promax",scores = "Thurstone")
fact.2$scores
fact.2$Structure
fact.2$Vaccounted

#------------------Week-36---------------
fact.2 <- fac(pca.forward,nfactors = 3,rotate = "promax",scores = "Thurstone")
fact.2$scores
fact.2$Vaccounted

forward.Rating <- df.forward[,c(1,2,3)]
forward.Rating <- cbind(forward.Rating,fact.2$scores)
View(forward.Rating)

forward.Rating[,c(4,5,6)] <- scale(forward.Rating[,c(4,5,6)])

#----------------------CS and Rating from Round-30 data
forward.Rating$CS <- (0.417 * forward.Rating$MR1) + (0.324 * forward.Rating$MR2) + (0.257 * forward.Rating$MR3)
forward.Rating$Rating <- normalize(forward.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
forward.Rating$Rating <- round(forward.Rating$Rating,1)

#----------------------CS and Rating from Round-36 data
forward.Rating$CS <- (0.406 * forward.Rating$MR1) + (0.257 * forward.Rating$MR2) + (0.336 * forward.Rating$MR3)
forward.Rating$Rating <- normalize(forward.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
forward.Rating$Rating <- round(forward.Rating$Rating,1)

View(forward.Rating)

write.csv(forward.Rating,file = "forward_Ratings.csv",sep = ",")



# ----------------------------------------------------Midfielder---------------------------------------------

pca.3 <- PCA(pca.midfielder,scale.unit = TRUE,graph = TRUE)
print(pca.3)

eig_val.3 <- get_eigenvalue(pca.3)
print(eig_val.3)
View(eig_val.3)

fviz_eig(pca.3,addlabels = TRUE,main = "Midfield")
fviz_pca_var(pca.3,col.var = "black")

#---------------------------------------Factor Analysis----------------------------------------------

#---------------Week-30-----------
fact.3 <- fac(pca.midfielder,nfactors = 4,rotate = "promax",scores = "Thurstone")
fact.3$scores
fact.3$Vaccounted

#---------------Week-36----------
fact.3 <- fac(pca.midfielder,nfactors = 4,rotate = "promax",scores = "Thurstone")
fact.3$scores
fact.3$Structure
fact.3$Vaccounted


midfield.Rating <- df.midfield[,c(1,2,3)]
midfield.Rating <- cbind(midfield.Rating,fact.3$scores)
midfield.Rating[,c(4,5,6,7)] <- scale(midfield.Rating[,c(4,5,6,7)])
View(midfield.Rating)

#----------------CS and Rating from Week-30-----------------
midfield.Rating$CS <- ((0.502 * midfield.Rating$MR1) + (0.326 * midfield.Rating$MR2) + (0.092 * midfield.Rating$MR3) + 
                                 (0.08 * midfield.Rating$MR4))
midfield.Rating$Rating <- normalize(midfield.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
midfield.Rating$Rating <- round(midfield.Rating$Rating,1)

#---------------CS and Rating from Week-36-------------------
midfield.Rating$CS <- ((0.473 * midfield.Rating$MR1) + (0.352 * midfield.Rating$MR2) + (0.088 * midfield.Rating$MR3) + 
                         (0.086 * midfield.Rating$MR4))
midfield.Rating$Rating <- normalize(midfield.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
midfield.Rating$Rating <- round(midfield.Rating$Rating,1)

View(midfield.Rating)

write.csv(midfield.Rating,file = 'midfielder_rating.csv',sep = ',')



# ----------------------------------------Defence-----------------------------------------------------

pca.4 <- PCA(pca.defence,scale.unit = TRUE,graph = FALSE)
print(pca.4)

eig_val.4 <- get_eigenvalue(pca.4)
print(eig_val.4)

fviz_eig(pca.4,addlabels = TRUE,main = "Defense")
fviz_pca_var(pca.4,col.var = "black")

#---------------------------------------------Factore Analysis------------------------------

#----------------Week-30-----------------
fact.4 <- fac(pca.defence,nfactors = 3,rotate = "promax",scores = "Thurstone")
fact.4$scores
fact.4$Vaccounted
fact.4$Structure

#----------------Week-36-----------------
fact.4 <- fac(pca.defence,nfactors = 3,rotate = "promax",scores = "Thurstone")
fact.4$scores
fact.4$Structure
fact.4$Vaccounted


defence.Rating <- df.defence[,c(1,2,3)]
defence.Rating <- cbind(defence.Rating,fact.4$scores)
defence.Rating[,c(4,5,6)] <- scale(defence.Rating[,c(4,5,6)])
print(defence.Rating)
View(defence.Rating)

# FGor Round 30
defence.Rating$CS <- ((0.468 * defence.Rating$MR1) + (0.441 * defence.Rating$MR2) + (0.089 * defence.Rating$MR3))
defence.Rating$Rating <- normalize(defence.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
defence.Rating$Rating <- round(defence.Rating$Rating,1)

# For Round 36
defence.Rating$CS <- ((0.478 * defence.Rating$MR1) + (0.393 * defence.Rating$MR2) + (0.128 * defence.Rating$MR3))
defence.Rating$Rating <- normalize(defence.Rating$CS,method = "range",range = c(1,10),margin = 2,on.constant = "quiet")
defence.Rating$Rating <- round(defence.Rating$Rating,1)

View(defence.Rating)


write.csv(defence.Rating,file = 'defence_rating.csv',sep = ',')

#---------------------------------------------------#



par(mfrow=c(2,2))
fviz_eig(pca.2,addlabels = TRUE,main = "Forward")
fviz_eig(pca.3,addlabels = TRUE,main = "Midfield")
fviz_eig(pca.4,addlabels = TRUE,main = "Defence")
fviz_eig(pca.1,addlabels = TRUE,main = 'Goalkeeper')

KMO(forward.numeric)
View(forward.numeric)
KMO(goalkeeper.numeric)
KMO(defence.numeric)
KMO(midfielder.numeric)

library(graphics)

plot(forward.numeric)
plot(midfielder.numeric)
plot(defence.numeric)
plot(goalkeeper.numeric)
