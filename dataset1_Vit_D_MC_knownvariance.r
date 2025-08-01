################### Examples for `ppwdeming` ###################
### libraries
library(ppwdeming)

################################################################
########## Illustrative RL_Deming Vitamin D data sets ##########
################################################################

################### Setup and data ##################
# set up output storage if desired
rm(list=ls())
sink()
#sink("Vit_D_MC_known.txt")

# set working directory to location of dataset
setwd(getSrcDirectory(function(){})[1])  # in R terminal
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # in RStudio
infile <- "Vit_D_MC_all.csv"

# read in data and store
datain <- read.csv(infile)
str(datain)
X      <- datain$LIAISON
Y      <- datain$Lumipulse

# define variance functions
gfun <- function(mu, gparms) {
  gparms[1] + gparms[2]*mu^gparms[3]
  }
hfun <- function(fity, hparms) {
  hparms[1] + hparms[2]*fity^hparms[3]
  }

# parameter specifications
lambda <- 1
gparms <- c(3.792e-16, 0.06911, 1.270 )
hparms <- c(6.106e-02, 7.019e-05, 2.184)
sigma  <- NA
kappa  <- NA

# display information about data
cat(sprintf("\nVitamin D example \n"))
hdr <- sprintf("Pearson correlation %5.4f Spearman %5.4f\n",
  cor(X,Y), cor(X,Y,method="spearman"))
cat(hdr)

# build visualization
jpeg(paste("VitD_knownVar.jpg"),
     width=480,height=480)
par(mfrow=c(2,2))
par(oma=c(0,0,2,0))
plot(datain$LIAISON, datain$Lumipulse, main="Scatter plot", xlab="Imprecise predicate",
 ylab="Precise test instrument",pch=20)
legend("topleft", bty="n", sprintf("r= %5.4f", cor(X,Y)))
abline(0,1, lty=2)
title("Known precision profiles", outer=T)

# fit the model
fitter <- PWD_known(X, Y, gfun, hfun, gparms, hparms, MDL=12, printem=TRUE)

# store output and constants for assessment
resis  <- fitter$resi
g      <- fitter$g
h      <- fitter$h
mu     <- fitter$mu
profl  <- fitter$profl
scalr  <- fitter$scalr
N      <- length(X)
deef   <- N-1
cheksd <- sqrt(var(scalr)*c(1, deef/qchisq(c(0.975, 0.025), deef)))

# residual information
cat(sprintf("\nResiduals\n"))
plot(X, resis, main="Residuals vs X", pch=20)
abline(h=0, lty=3)

# fit the Rocke-Lorenzato precision profile model to the residuals
checkr <- PWD_resi(mu, resis, printem=TRUE)
RLscal <- checkr$scalr

# scaled residual information
cat(sprintf("\nScaled residual SD and CI %6.3f %6.3f %6.3f\n",
            cheksd[1], cheksd[2], cheksd[3]))
cat(sprintf("Shapiro test for normality P %6.4f\n",shapiro.test(scalr)$p.value))
Index  <- 1:N
plot(Index, scalr, main="Scaled residuals", xlab="Index",
  ylab="Residual",pch=20)
abline(h=0, lty=2)

# final assessment plot
qqnorm(scalr,pch=20)

# close and write out the plot
dev.off()

