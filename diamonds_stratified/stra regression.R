separate.regression.mean=function(Nh,y.sample, x.sample, N=NULL, stra.index, Xbarh, alpha, method="Min", betah=NULL)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  yh.est  =rep(0, stra.num)
  yh.var  =rep(0, stra.num)
  yh.sd   =rep(0, stra.num)
  yh.left =rep(0, stra.num)
  yh.right=rep(0, stra.num)
  
  nh  =rep(0, stra.num)
  sy2 =rep(0, stra.num)
  sx2 =rep(0, stra.num)
  syx =rep(0, stra.num)
  
  for (h in 1:stra.num)
  {
    y.hth=y.sample[stra.index==h]
    x.hth=x.sample[stra.index==h]
    stra.regression=regression.mean(y.hth, x.hth, Nh[h], Xbar[h], alpha, method="Min", betah=NULL)
    
    nh[h]=length(y.hth)
    
    sy2[h] =var(y.hth)
    sx2[h] =var(x.hth)
    syx[h]=cov(y.hth, x.hth)
    
    yh.est[h]  =stra.regression$Est
    yh.var[h]  =stra.regression$Var
    yh.sd[h]   =stra.regression$SD
    yh.left[h] =stra.regression$Left
    yh.right[h]=stra.regression$Right
  }
  stra.result=cbind(Nh, Wh, yh.est, yh.var, yh.sd, yh.left, yh.right)
 
  fh=ifelse(is.null(Nh), 0, nh/Nh)
  nf=(1-fh)/nh

  if (method=="Min")
  {
    beta=syx/sx2
    ylrS.est=sum(Wh*yh.est)
    ylrS.var=sum((Wh^2)*nf*(n-1)/(n-2)*(sy2-syx^2/sx2))
    ylrS.sd =sqrt(ylrS.var)
    
    ylrS.ci=conf.interval(ylrS.est, ylrS.sd, alpha)
    ylrS.left =ylrS.ci$left
    ylrS.right=ylrS.ci$right
    
    ylrS.result=matrix(c(ylrS.est, ylrS.var, ylrS.sd, ylrS.left, ylrS.right), nrow=1)
    colnames(ylrS.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ylrS.result)=c("Mean_lrS")
  }
  
  if (method=="Constant")
  {
    beta=betah
    ylrS.est=sum(Wh*yh.est)
    ylrS.var=sum((Wh^2)*yh.var)
    ylrS.sd =sqrt(ylrS.var)
    
    ylrS.ci=conf.interval(ylrS.est, ylrS.sd, alpha)
    ylrS.left =ylrS.ci$left
    ylrS.right=ylrS.ci$right
    
    ylrS.result=matrix(c(ylrS.est, ylrS.var, ylrS.sd, ylrS.left, ylrS.right), nrow=1)
    colnames(ylrS.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ylrS.result)=c("lrS_Reg")
  }
  return(list(stra.result=as.data.frame(stra.result), ylrS.result=as.data.frame(ylrS.result)))
}




combined.regression.mean=function(Nh,y.sample, x.sample, N=NULL, stra.index, Xbar, alpha, method="Min", beta0=NULL)
{
  yst.result=stra.srs.mean2(Nh, y.sample, stra.index, alpha)$mean.result
  xst.result=stra.srs.mean2(Nh, x.sample, stra.index, alpha)$mean.result
  
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  nh  =rep(0, stra.num)
  sy2 =rep(0, stra.num)
  sx2 =rep(0, stra.num)
  syx =rep(0, stra.num)
  
  for (h in 1:stra.num)
  {
    y.hth=y.sample[stra.index==h]
    x.hth=x.sample[stra.index==h]
    
    nh[h]=length(y.hth)
    
    sy2[h] =var(y.hth)
    sx2[h] =var(x.hth)
    syx[h]=cov(y.hth, x.hth)
  }
  stra.result=cbind(Nh, Wh, nh, fh)
  
  fh=nh/Nh
  nf=(1-fh)/nh
  
  if (method=="Min")
  {
    beta=sum((Wh^2)*nf*syx)/sum((Wh^2)*nf*sx2)
    ylrC.est=yst.result$mean.est+beta*(Xbar-xst.result$mean.est)
    ylrC.var=sum((Wh^2)*nf*(sy2+(beta^2*sx2-2*beta*syx)))
    ylrC.sd =sqrt(ylrC.var)
    
    ylrC.ci=conf.interval(ylrC.est, ylrC.sd, alpha)
    ylrC.left =ylrC.ci$left
    ylrC.right=ylrC.ci$right
    
    ylrC.result=matrix(c(ylrC.est, ylrC.var, ylrC.sd, ylrC.left, ylrC.right), nrow=1)
    colnames(ylrC.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ylrC.result)=c("Mean_lrC")
  }
  
  if (method=="Constant")
  {
    beta=beta0
    ylrC.est=yst.result$mean.est+beta*(Xbar-xst.result$mean.est)
    ylrC.var=ylrC.var=sum((Wh^2)*nf*(sy2+(beta^2*sx2-2*beta*syx)))
    ylrC.sd =sqrt(ylrC.var)
    
    ylrC.ci=conf.interval(ylrC.est, ylrC.sd, alpha)
    ylrC.left =ylrC.ci$left
    ylrC.right=ylrC.ci$right
    
    ylrC.result=matrix(c(ylrC.est, ylrC.var, ylrC.sd, ylrC.left, ylrC.right), nrow=1)
    colnames(ylrC.result)=c("Est", "Var", "SD", "Left", "Right")
    rownames(ylrC.result)=c("lrC_Reg")
  }

  return(list(stra.result=as.data.frame(stra.result), ylrC.result=as.data.frame(ylrC.result)))
}



