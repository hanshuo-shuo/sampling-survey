ratio=function(y.sample, x.sample, N=NULL, auxiliary=FALSE, Xbar=NULL, alpha)
{
  n=length(y.sample)
  f=ifelse(is.null(N), 0, n/N)
  nf=(1-f)/n

  ybar=mean(y.sample)
  xbar=mean(x.sample)
  
  sy2 =var(y.sample)
  sx2 =var(x.sample)
  syx =cov(y.sample, x.sample)
  
  cy2 =nf*sy2/(ybar^2)
  cx2 =nf*sx2/(xbar^2)
  cyx =nf*syx/(ybar*xbar)
  
  ratio.est=ybar/xbar
  
  if(auxiliary==FALSE)
  {
    ratio.var=(nf/xbar^2)*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
    ratio.sd =sqrt(ratio.var)
  }
  else
  {
    ratio.var=(nf/Xbar^2)*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
    ratio.sd =sqrt(ratio.var)
  }  
  
  
  ### CI: Method 1 ####################################
  ci1=conf.interval(ratio.est, ratio.sd, alpha)
  left1 =ci1$left
  right1=ci1$right
  
  
  ### CI: Method 2 ####################################
  quan=qnorm(1-alpha/2)
  
  est2=1-quan^2*cyx
  var2=(cy2+cx2-2*cyx)-quan^2*(cy2*cx2-cyx^2)
  
  ci2=conf.interval(est2, sqrt(var2), alpha)
  left2 =ratio.est*(ci2$left)/(1-quan^2*cx2)
  right2=ratio.est*(ci2$right)/(1-quan^2*cx2)
  
  ### CI: Method 3 ####################################
  var3=ratio.est^2*(cy2+cx2-2*cyx)
  
  ci3=conf.interval(ratio.est, sqrt(var3), alpha)
  left3 =ci3$left
  right3=ci3$right
  
  ratio.ci=t(cbind(c(left1, right1, right1-left1), c(left2, right2, right2-left2), c(left3, right3, right3-left3)))
  colnames(ratio.ci)=c("Left", "Right", "Length")
  rownames(ratio.ci)=c("Classic", "Exact", "Exact2")
  return(list(ratio.est=ratio.est, ratio.var=ratio.var, ratio.sd=ratio.sd, ratio.ci=as.data.frame(ratio.ci)))
}



ratio.mean=function(y.sample, x.sample, N=NULL, Xbar, alpha)
{ 
  ratio.result=ratio(y.sample, x.sample, N, auxiliary=TRUE, Xbar, alpha)
  ratio.est=ratio.result$ratio.est
  ratio.var=ratio.result$ratio.var
  ratio.ci =ratio.result$ratio.ci
  
  ybarR.est=Xbar*ratio.est              
  ybarR.var=Xbar^2*ratio.var
  ybarR.sd =sqrt(ybarR.var)
  ybarR.ci =Xbar*ratio.ci
  
  return(list(ybarR.est=ybarR.est, ybarR.var=ybarR.var, ybarR.sd=ybarR.sd, 
              ybarR.ci=as.data.frame(ybarR.ci)))
}


ratio.total=function(y.sample, x.sample, N, Xbar, alpha)
{
  ybarR.result=ratio.mean(y.sample, x.sample, N, Xbar, alpha)
  ybarR.est=ybarR.result$ybarR.est
  ybarR.var=ybarR.result$ybarR.var
  ybarR.ci =ybarR.result$ybarR.ci
  
  ytotal.est=N*ybarR.est              
  ytotal.var=N^2*ybarR.var
  ytotal.sd =sqrt(ytotal.var)
  ytotal.ci =N*ybarR.ci
  
  return(list(ytotal.est=ytotal.est, ytotal.var=ytotal.var, ytotal.sd=ytotal.sd, 
              ytotal.ci=as.data.frame(ytotal.ci)))
}
