## the analysis for diamonds  ######################################
source("ci.r")
source("srs sampling.R")
source("srs simple.r")
source("srs ratio.r")
source("srs regression.r")
source("srs size.r")
source("stra sampling.R")
source("stra simple.r")
source("stra ratio.r")
source("stra size.r")
source("deff.r")

set.seed(114)

########## Input data  #########################################
full.data=read.csv("diamonds.csv")
print(full.data)

N=nrow(full.data)
Xbar=mean(full.data$carat)

n=5000
alpha=0.05

method="r"
bound=0.01
Mean.his=mean(full.data$price)
Var.his=var(full.data$price)

mysample=srs.sampling(N,n,full.data)
y.sample=mysample$price
x.sample=mysample$carat

######### data analysis  #######################################################
mean.simple.result=srs.mean(N,y.sample,alpha)
print(mean.simple.result)

mean.ratio.result=ratio.mean(y.sample, x.sample, N, Xbar, alpha)
print(mean.ratio.result)

mean.regression.result=regression.mean(y.sample, x.sample, N, Xbar, alpha, method="Min", beta0=NULL)
print(mean.regression.result)

mean.simple.size.result=size.mean(N, Mean.his, Var.his, method="r", bound, alpha)
print(mean.simple.size.result)

var.result=c(mean.simple.result$ybar.var, mean.ratio.result$ybarR.var, mean.regression.result$Var)
deff.result=deff(var.result)

n.simple=size.mean(N, Mean.his, Var.his, method="r", bound=0.01, alpha)$size
size.result=deff.size(deff.result, n.simple)

rownames(deff.result)=c("Simple", "Ratio", "Regression")
print(deff.result)

rownames(size.result)=c("Simple", "Ratio", "Regression")
print(size.result)


##########  output the restult  ####################
sink("SRS_result.txt")
cat("This is an application to diamonds data:", "\n")
cat("\n")
cat("The estimation of mean price under SRS design:", "\n")
cat("The full size N = ", N, "\n")
cat("The sample size n = ", n, "\n")
cat("\n")

cat("The result for SRS simple estimation:", "\n")
cat("\n")
cat("Est = ", mean.simple.result$ybar, "\n")
cat("Var = ", mean.simple.result$ybar.var, "\n")
cat("SD  = ", mean.simple.result$ybar.sd, "\n")
cat("CI = ", "[", mean.simple.result$left, ",", mean.simple.result$right, "]", "\n")
cat("\n")

cat("The result for SRS ratio estimation:", "\n")
cat("\n")
cat("Est = ", mean.ratio.result$ybarR.est, "\n")
cat("Var = ", mean.ratio.result$ybarR.var, "\n")
cat("SD  = ", mean.ratio.result$ybarR.sd, "\n")
cat("CI = ", "[", mean.ratio.result$ybarR.ci[1,1], ",", mean.ratio.result$ybarR.ci[1,2], "]", "\n")
cat("\n")

cat("The result for SRS regression estimation:", "\n")
cat("\n")
cat("Est = ", mean.regression.result$Est, "\n")
cat("Var = ", mean.regression.result$Var, "\n")
cat("SD  = ", mean.regression.result$SD, "\n")
cat("CI = ", "[", mean.regression.result$Left, ",", mean.regression.result$Right, "]", "\n")
cat("\n")

cat("The design efficiency for the three estimators:","\n")
cat("\n")
print(deff.result)
cat("\n")

cat("When r=0.01 , the sample size for the three estimators:","\n")
cat("\n")
print(size.result)

sink()



