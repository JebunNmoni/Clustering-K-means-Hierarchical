# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('<FolderPath>')
# Load Data

#Install if the package doesn't exist 
if (!require(ISLR)) install.packages('ISLR')
library(ISLR) 
if (!require(MASS)) install.packages('MASS')
library(MASS)
if (!require(e1071)) install.packages('e1071')
library(e1071)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(lattice)) install.packages('lattice')
library(lattice)
if (!require(class)) install.packages('class')
library(class)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(pROC)) install.packages('pROC')
library(pROC)
if (!require(boot)) install.packages('boot')
library(boot)
if (!require(lmtest)) install.packages('lmtest')
library(lmtest)
if (!require(hmeasure)) install.packages('hmeasure')
library(hmeasure)
if (!require(leaps)) install.packages('leaps')
library(leaps)
if (!require(glmnet)) install.packages('glmnet')
library(glmnet)
if (!require(tree)) install.packages('tree')
library(tree)
if (!require(psych)) install.packages('psych')
library(psych)
if (!require('dendextend')) install.packages('dendextend'); 
library('dendextend')


####################################
cereal <- read.csv('cereal.csv')
sum(is.na(cereal))
str(cereal)

describe(cereal) # Summary of the data

## Pairwise correlation and plot
panel.cor = function(x, y, digits = 2, prefix = "", cex.cor, ...) {
usr = par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r = cor(x, y)
txt = format(c(r, 0.123456789), digits = digits)[1]
txt = paste0(prefix, txt)
text(0.5, 0.5, txt, cex = 1.5)
}
pairs(cereal, col="purple" , lower.panel = panel.cor, pch = 18)

par(mfrow=c(2,2))
STD.cereal = scale(cereal[,4:11], center = TRUE, scale = TRUE) # Standardizing Data
hc.compl.cereal = hclust(dist(STD.cereal, method = "euclidean"), method="complete" )
dendo.cereal = as.dendrogram(hc.compl.cereal, cex = 0.5)
dendo.color.cereal=color_branches(dendo.cereal, k=2, col = c(1,3)) # auto-coloring 3 clusters of branches.
# Define node parameters
nodePar = list(lab.cex = 0.75, pch = c(NA, NA), col = "gray80")
plot(dendo.color.cereal, nodePar = nodePar) # Customized dendogram
abline(h = 8, col = "forestgreen", lty = "dashed", lwd = 2)
h.cl.cereal = cutree(hc.compl.cereal, 2)
h.cl.cereal
table(h.cl.cereal)




STD.cereal = scale(cereal[,4:11], center = TRUE, scale = TRUE) # Standardizing Data
hc.compl.cereal = hclust(dist(STD.cereal, method = "euclidean"), method="complete" )
dendo.cereal = as.dendrogram(hc.compl.cereal, cex = 0.5)
dendo.color.cereal=color_branches(dendo.cereal, k=3, col = c(1,3,4)) # auto-coloring 3 clusters of branches.
# Define node parameters
nodePar = list(lab.cex = 0.75, pch = c(NA, NA), col = "gray80")
plot(dendo.color.cereal, nodePar = nodePar) # Customized dendogram
abline(h = 6.5, col = "forestgreen", lty = "dashed", lwd = 2)
h.cl.cereal = cutree(hc.compl.cereal, 3)
h.cl.cereal
table(h.cl.cereal)



STD.cereal = scale(cereal[,4:11], center = TRUE, scale = TRUE) # Standardizing Data
hc.compl.cereal = hclust(dist(STD.cereal, method = "euclidean"), method="complete" )
dendo.cereal = as.dendrogram(hc.compl.cereal, cex = 0.5)
dendo.color.cereal=color_branches(dendo.cereal, k=4, col = c(1,3,4,5)) # auto-coloring 3 clusters of branches.
# Define node parameters
nodePar = list(lab.cex = 0.75, pch = c(NA, NA), col = "gray80")
plot(dendo.color.cereal, nodePar = nodePar) # Customized dendogram
abline(h = 6.1, col = "forestgreen", lty = "dashed", lwd = 2)
h.cl.cereal = cutree(hc.compl.cereal, 4)
h.cl.cereal
table(h.cl.cereal)

plot(as.data.frame(STD.cereal), col=h.cl.cereal,pch=h.cl.cereal)

STD.cereal = scale(cereal[,4:11], center = TRUE, scale = TRUE) # Standardizing Data
hc.compl.cereal = hclust(dist(STD.cereal, method = "euclidean"), method="complete" )
dendo.cereal = as.dendrogram(hc.compl.cereal, cex = 0.5)
dendo.color.cereal=color_branches(dendo.cereal, k=5, col = c(1,3,4,5,6)) # auto-coloring 3 clusters of branches.
# Define node parameters
nodePar = list(lab.cex = 0.75, pch = c(NA, NA), col = "gray80")
plot(dendo.color.cereal, nodePar = nodePar) # Customized dendogram
abline(h = 5.4, col = "forestgreen", lty = "dashed", lwd = 2)
h.cl.cereal = cutree(hc.compl.cereal, 5)
h.cl.cereal
table(h.cl.cereal)

##############################################

km.out <- kmeans(as.data.frame(STD.cereal), 2, nstart = 20)
km.out
km.out <- kmeans(as.data.frame(STD.cereal), 3, nstart = 20)
km.out
km.out <- kmeans(as.data.frame(STD.cereal), 4, nstart = 20)
km.out
