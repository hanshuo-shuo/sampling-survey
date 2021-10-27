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
library(dplyr)

set.seed(114)

########## Input data  #########################################
full.data=read.csv("diamonds.new.csv")
print(full.data)

N=nrow(full.data)
Xbar=mean(full.data$carat)

n=5000
alpha=0.05

allocation="Prop"

method="r"
bound=0.01

mysample=stra.sampling(N,n,full.data,allocation)
print(mysample)

full.data1=filter(full.data,full.data$cut=='Fair')
full.data2=filter(full.data,full.data$cut=='Good')
full.data3=filter(full.data,full.data$cut=='Very Good')
full.data4=filter(full.data,full.data$cut=='Premium')
full.data5=filter(full.data,full.data$cut=='Ideal')

Nh=mysample$Nh
Xbarh=c(mean(full.data1$carat),mean(full.data2$carat),mean(full.data3$carat),mean(full.data4$carat),mean(full.data5$carat))
Xbar=mean(full.data$carat)
Ybar=mean(full.data$price)
S2h=c(var(full.data1$price),var(full.data2$price),var(full.data3$price),var(full.data4$price),var(full.data5$price))

y.sample=c(mysample$stra.srs.sample1$price,mysample$stra.srs.sample2$price,mysample$stra.srs.sample3$price,mysample$stra.srs.sample4$price,mysample$stra.srs.sample5$price)
x.sample=c(mysample$stra.srs.sample1$carat,mysample$stra.srs.sample2$carat,mysample$stra.srs.sample3$carat,mysample$stra.srs.sample4$carat,mysample$stra.srs.sample5$carat)
stra.index=c(mysample$stra.srs.sample1$stra.index,mysample$stra.srs.sample2$stra.index,mysample$stra.srs.sample3$stra.index,mysample$stra.srs.sample4$stra.index,mysample$stra.srs.sample5$stra.index)

######### data analysis  #######################################################
mean.SS.result=stra.srs.mean2(Nh, y.sample, stra.index, alpha)
SS.stra=mean.SS.result$stra.result
SS.mean=mean.SS.result$mean.result
print(mean.SS.result)

mean.RS.result=separate.ratio.mean(Nh, y.sample, x.sample, stra.index, Xbarh, alpha)
RS.stra=mean.RS.result$stra.result
RS.mean=mean.RS.result$yRS.result
print(mean.RS.result)

mean.RC.result=combined.ratio.mean(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
RC.stra=mean.RC.result$stra.result
RC.mean=mean.RC.result$yRC.result
print(mean.RC.result)

var.result=c(SS.mean$mean.var, RS.mean$Var, RC.mean$Var)
deff.result=deff(var.result)

n.stra=strata.mean.size(Nh, S2h, Ch=NULL, allocation="Prop", method="r", bound=0.01, Ybar, alpha)$n
size.result=deff.size(deff.result, n.stra)

rownames(deff.result)=c("Stra Simple", "Sperate Ratio", "Combined Ratio")
print(deff.result)

rownames(size.result)=c("Stra Simple", "Sperate Ratio", "Combined Ratio")
print(size.result)


##########  output the restult  ####################
sink("Stratified_result.txt")
cat("This is an application to diamonds data:", "\n")
cat("\n")
cat("The estimation of mean price under Stratified design:", "\n")
cat("The full size N = ", N, "\n")
cat("The sample size n = ", n, "\n")
cat("\n")

cat("The result for stratified simple estimation:", "\n")
cat("\n")
cat("Est = ", SS.mean$mean.est, "\n")
cat("Var = ", SS.mean$mean.var, "\n")
cat("SD  = ", SS.mean$mean.sd, "\n")
cat("CI = ", "[", SS.mean$mean.left, ",", SS.mean$mean.right, "]", "\n")
cat("\n")

cat("The result for sperate ratio estimation:", "\n")
cat("\n")
cat("Est = ", RS.mean$Est, "\n")
cat("Var = ", RS.mean$Var, "\n")
cat("SD  = ", RS.mean$SD, "\n")
cat("CI = ", "[", RS.mean$Left, ",", RS.mean$Right, "]", "\n")
cat("\n")

cat("The result for combined ratio estimation:", "\n")
cat("\n")
cat("Est = ", RC.mean$Est, "\n")
cat("Var = ", RC.mean$Var, "\n")
cat("SD  = ", RC.mean$SD, "\n")
cat("CI = ", "[", RC.mean$Left, ",", RC.mean$Right, "]", "\n")
cat("\n")

cat("The design efficiency of the three designs:","\n")
cat("\n")
print(deff.result)
cat("\n")

cat("When r=0.01 , the sample size for the three estimators:","\n")
cat("\n")
print(size.result)

sink()

