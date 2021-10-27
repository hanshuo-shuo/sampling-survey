stra.srs.mean1=function(Nh, nh, yh, s2h, alpha)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  
  yh.est  =rep(0, stra.num)
  yh.var  =rep(0, stra.num)
  yh.sd   =rep(0, stra.num)
  yh.left =rep(0, stra.num)
  yh.right=rep(0, stra.num)
  
  for (h in 1:stra.num)
  {
    yh.est[h]=yh[h]
    yh.var[h]=((1-fh[h])/nh[h])*s2h[h]
    yh.sd[h]=sqrt(yh.var[h])
      
    ci.stra=conf.interval(yh.est[h], yh.sd[h], alpha)
    yh.left[h] =ci.stra$left
    yh.right[h]=ci.stra$right
  }
  
  stra.result=cbind(Nh, nh, Wh, yh.est, yh.var, yh.sd, yh.left, yh.right)
  
  mean.est=sum(Wh*yh.est)
  mean.var=sum(Wh^2*yh.var)
  mean.sd =sqrt(mean.var)
  
  ci.result=conf.interval(mean.est, mean.sd, alpha)
  mean.left =ci.result$left
  mean.right=ci.result$right
  
  mean.result=matrix(c(mean.est, mean.var, mean.sd, mean.left, mean.right), nrow=1)
  colnames(mean.result)=c("mean.est", "mean.var", "mean.sd", "mean.left", "mean.right")
  rownames(mean.result)="Strat"
  return(list(stra.result=as.data.frame(stra.result), mean.result=as.data.frame(mean.result)))
}

stra.srs.mean2=function(Nh, mysample, stra.index, alpha)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  yh.est  =rep(0, stra.num)
  yh.var  =rep(0, stra.num)
  yh.sd   =rep(0, stra.num)
  yh.left =rep(0, stra.num)
  yh.right=rep(0, stra.num)
  
  for (h in 1:stra.num)
  {
    sample.hth=mysample[stra.index==h]
    stra.result=srs.mean(Nh[h], sample.hth, alpha)
    
    yh.est[h]  =stra.result$ybar
    yh.var[h]  =stra.result$ybar.var
    yh.sd[h]   =stra.result$ybar.sd
    yh.left[h] =stra.result$left
    yh.right[h]=stra.result$right
  }
  
  stra.result=cbind(Nh, Wh, yh.est, yh.var, yh.sd, yh.left, yh.right)
  
  mean.est=sum(Wh*yh.est)
  mean.var=sum(Wh^2*yh.var)
  mean.sd =sqrt(mean.var)
  
  ci.result=conf.interval(mean.est, mean.sd, alpha)
  mean.left =ci.result$left
  mean.right=ci.result$right
  
  mean.result=matrix(c(mean.est, mean.var, mean.sd, mean.left, mean.right), nrow=1)
  colnames(mean.result)=c("mean.est", "mean.var", "mean.sd", "mean.left", "mean.right")
  rownames(mean.result)="Strat"
  return(list(stra.result=as.data.frame(stra.result), mean.result=as.data.frame(mean.result)))
}


stra.srs.prop1=function(Nh, nh, ah, alpha)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  
  ph.est  =rep(0, stra.num)
  ph.var  =rep(0, stra.num)
  ph.sd   =rep(0, stra.num)
  ph.left =rep(0, stra.num)
  ph.right=rep(0, stra.num)
  
  for (h in 1:stra.num)
  {
    ph.est[h]=ah[h]/nh[h]
    ph.var[h]=((1-fh[h])/(nh[h]-1))*ph.est[h]*(1-ph.est[h])
    ph.sd[h] =sqrt(ph.var[h])
    
    ci.stra=conf.interval(ph.est[h], ph.sd[h], alpha)
    ph.left[h] =ci.stra$left
    ph.right[h]=ci.stra$right
  }
  
  stra.result=cbind(Nh, nh, ah, Wh, ph.est, ph.var, ph.sd, ph.left, ph.right)
  
  prop.est=sum(Wh*ph.est)
  prop.var=sum(Wh^2*ph.var)
  prop.sd =sqrt(prop.var)
  
  ci.result=conf.interval(prop.est, prop.sd, alpha)
  prop.left =ci.result$left
  prop.right=ci.result$right
  
  prop.result=matrix(c(prop.est, prop.var, prop.sd, prop.left, prop.right), nrow=1)
  colnames(prop.result)=c("prop.est", "prop.var", "prop.sd", "prop.left", "prop.right")
  rownames(prop.result)="Prop"
  return(list(stra.result=as.data.frame(stra.result), prop.result=as.data.frame(prop.result)))
}



stra.srs.prop2=function(Nh, nh, ah, alpha)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  ph.est  =rep(0, stra.num)
  ph.var  =rep(0, stra.num)
  ph.sd   =rep(0, stra.num)
  ph.left =rep(0, stra.num)
  ph.right=rep(0, stra.num)
  
  for (h in 1:stra.num)
  {
    stra.result=srs.prop(Nh[h], nh[h], ah[h], alpha)
    ph.est[h]  =stra.result$p.est
    ph.var[h]  =stra.result$p.var
    ph.sd[h]   =stra.result$p.sd
    ph.left[h] =stra.result$left
    ph.right[h]=stra.result$right
  }
  stra.result=cbind(Nh, nh, ah, Wh, ph.est, ph.var, ph.sd, ph.left, ph.right)
  
  prop.est=sum(Wh*ph.est)
  prop.var=sum(Wh^2*ph.var)
  prop.sd =sqrt(prop.var)
  
  ci.result=conf.interval(prop.est, prop.sd, alpha)
  prop.left =ci.result$left
  prop.right=ci.result$right
  
  prop.result=matrix(c(prop.est, prop.var, prop.sd, prop.left, prop.right), nrow=1)
  colnames(prop.result)=c("prop.est", "prop.var", "prop.sd", "prop.left", "prop.right")
  rownames(prop.result)="Prop"
  return(list(stra.result=as.data.frame(stra.result), prop.result=as.data.frame(prop.result)))
}
