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
#sink("Vit_D_MC_unknown.txt")

# set working directory to location of dataset
setwd(getSrcDirectory(function(){})[1])  # in R terminal
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # in RStudio
infile <- "Vit_D_MC_all.csv"

# read in data and store
datain <- read.csv(infile)
str(datain)
X      <- datain$LIAISON
Y      <- datain$Lumipulse

# display information about data
cat(sprintf("\nVitamin D example \n"))
hdr <- sprintf("Pearson correlation %5.4f Spearman %5.4f\n",
               cor(X,Y), cor(X,Y,method="spearman"))
cat(hdr)

# fit the model for each of lambda=1 and lambda=25
for (lambda in c(1, 25)) {
  cat(sprintf("\nLambda %4.2f\n", lambda))

  # estimates for variance functions, fit the model
  do <- PWD_get_gh(X, Y, lambda)

  # store and display output
  alpha <- do$alpha
  beta  <- do$beta
  sigma <- do$sigma
  kappa <- do$kappa
  cat(sprintf("alpha %7.4f slope %6.4f\n",
              alpha, beta))
  cat(sprintf("sigma %7.4f kappa %7.4f \n", sigma, kappa))

  # open file for plots and set plotting matrix
  jpeg(paste("VitD_unknownVar_lambda_", lambda, ".jpg", sep=""),
       width=480,height=720)
  par(mfrow=c(3,2))
  par(oma=c(0,0,2,0))
  # start visualizations with scatterplot
  plot(X,Y, main="Showing regression", xlab="Predicate", ylab="test",pch=20)
  title(paste("Vitamin D data set, lambda=", lambda), outer=T)
  abline(alpha, beta)

  # inference about the model
  inf     <- PWD_inference(X, Y, lambda, MDL=12, printem=TRUE)
  sealpha <- inf$sealpha
  sebeta  <- inf$sebeta
  covar   <- inf$covar

  # plot with fitted model
  key  <- order(X)
  sX   <- X[key]
  sY   <- Y[key]
  plot (sX, sY, main="PW Deming with CI", xlab="Predicate", ylab="Test",pch=20)
  legend("topleft", bty="n", legend=sprintf("r= %5.4f", cor(X,Y)))
  fit  <- alpha + beta*sX
  abline(alpha, beta)
  n    <- length(X)
  tcut <- -qt(0.025, n-1)
  moe  <- tcut*sqrt(sealpha^2 + (sebeta*sX)^2 + covar*sX)
  lines(sX, fit-moe, lty=3)
  lines(sX, fit+moe, lty=3)

  # residual information
  truv  <- X
  resi  <- inf$resi
  fits  <- Y - resi

  # plots of residuals
  plot(X, resi , main="residuals",pch=20)
  plot(X, resi /fit, main="Proportional residuals", ylab="resi / fitted",pch=20)

  # fit the Rocke-Lorenzato precision profile model to the residuals
  post  <- PWD_resi(truv, resi )
  with(post,cat(sprintf("RL fit to resi  sigma %6.4f kappa %6.4f\n", sigma, kappa)))
  with(post, cat(sprintf("pooled sigma %7.4f kappa %6.4f\n", poolsig, poolkap)))
  with(post,cat(sprintf("LR tests constant sd %5.3f cv %5.3f P %7.4g %7.4g\n", tests[1], tests[2], Pvals[1], Pvals[2])))

  # store scaled residuals
  profl <- sqrt(post$sigma^2 + (post$kappa*X)^2)
  scalf <- resi  / profl
  cat(sprintf("Scaled residuals mean and sd %5.3f %5.3f\n", mean(scalf), sd(scalf)))

  # plots for scaled residuals
  plot(X, profl, main="Fitted precision profile model", type="l", ylab="Residual sd")
  plot(X, scalf, main="scaled residuals")
  # close and write out the plot
  dev.off()

}

