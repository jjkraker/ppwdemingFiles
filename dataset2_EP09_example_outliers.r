################### Examples for `ppwdeming` ###################
### libraries
library(ppwdeming)

################################################################
##########  Illustrative RL_Deming EP09 with outliers ##########
################################################################

# set up output storage if desired
rm(list=ls())
sink()
#sink("EP09_example_1.txt")

# set working directory to location of dataset
setwd(getSrcDirectory(function(){})[1])  # in R terminal
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # in RStudio
infile <- "EP09_example_1.csv"

# read in data and store
datain <- read.csv(infile)
str(datain)
# select response with outliers
X   <- datain$MPX
Y   <- datain$w_outlier

# parameter
lambda <- 1

# display information about data
fitlab <- "Outlier "
cat(sprintf("\n%1s\n", fitlab))
cat(sprintf("Pearson correlation %5.4f Spearman %5.4f\n",
            cor(X,Y), cor(X,Y,method="spearman")))


# build visualization
jpeg(paste("EP09_unknownVar_lambda_1_outliers.jpg", sep=""),
     width=480,height=720)
par(mfrow=c(3,2))
par(mar=c(4,4,2,1))
par(oma=c(0,0,2,0))
N   <- length(X)
pch <- rep(1,N)
pch[datain$Outlier>0] <- 15

plot(X,Y, main="Scatter plot", pch=pch)
legend("topleft", bty="n", sprintf("r= %5.4f", cor(X,Y)))

# fit the model
fitter <- PWD_inference(X, Y, lambda, printem=TRUE)

# store output and constants for assessment
sigma  <- fitter$sigma
kappa  <- fitter$kappa
resi   <- fitter$resi
mu     <- fitter$mu
Index  <- 1:N
deef   <- N-2

# residual information
plot(X, resi , main="Residuals", pch=pch)
title(fitlab, outer=TRUE)

# fit the Rocke-Lorenzato precision profile model to the residuals
checkr <- PWD_resi(X, resi , printem=TRUE)
profl  <- checkr$profl
scalr  <- checkr$scalr
cheksd <- sqrt(var(scalr)*c(1, deef/qchisq(c(0.975, 0.025), deef)))

# scaled residual information
cat(sprintf("\nScaled residual SD and CI %6.3f %6.3f %6.3f\n",
            cheksd[1], cheksd[2], cheksd[3]))
cat(sprintf("Shapiro test for normality P %6.4f\n",shapiro.test(scalr)$p.value))
plot(Index, scalr, main="Scaled residuals", xlab="Index", ylab="Residual", pch=pch)
abline(h=0, lty=2)
qqnorm(scalr, main="QQ plot")

# check for outliers
outt <- PWD_outlier(X, Y, K=5)
if (outt$ndrop > 0) {
  plot(Index, outt$scalr, main="Cleaned fit", xlab="Index", ylab="Residual", pch=pch)
  abline(h=0, lty=2)
  qqnorm(outt$scalr, main="QQ plot")
}

# close and write out the plot
dev.off()
