separate.ratio.mean=function(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
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
    y.hth=y.sample[stra.index==h]
    x.hth=x.sample[stra.index==h]
    stra.ratio=ratio.mean(y.hth, x.hth, Nh[h], Xbar[h], alpha)
    
    yh.est[h]  =stra.ratio$ybarR.est
    yh.var[h]  =stra.ratio$ybarR.var
    yh.sd[h]   =stra.ratio$ybarR.sd
    
    yh.ci      =stra.ratio$ybarR.ci
    yh.ci.left =yh.ci$Left
    yh.ci.right=yh.ci$Right
    yh.left[h] =yh.ci.left[1]
    yh.right[h]=yh.ci.right[1]
  }
  stra.result=cbind(Nh, Wh, yh.est, yh.var, yh.sd, yh.left, yh.right)
  
  yRS.est=sum(Wh*yh.est)
  yRS.var=sum(Wh^2*yh.var)
  yRS.sd =sqrt(yRS.var)
  
  yRS.ci=conf.interval(yRS.est, yRS.sd, alpha)
  yRS.left =yRS.ci$left
  yRS.right=yRS.ci$right
  
  yRS.result=matrix(c(yRS.est, yRS.var, yRS.sd, yRS.left, yRS.right), nrow=1)
  colnames(yRS.result)=c("Est", "Var", "SD", "Left", "Right")
  rownames(yRS.result)="Mean_RS"
  return(list(stra.result=as.data.frame(stra.result), yRS.result=as.data.frame(yRS.result)))
}



separate.ratio.total=function(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
{
  N=sum(Nh)
  Wh=Nh/sum(Nh)
  
  mean.RS.result=separate.ratio.mean(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
  RS.stra=mean.RS.result$stra.result
  RS.mean=mean.RS.result$yRS.result
  
  yh.totR.est  =N*RS.stra$yh.est
  yh.totR.var  =N^2*RS.stra$yh.var
  yh.totR.sd   =sqrt(yh.totR.var)
  yh.totR.left =N*RS.stra$yh.left
  yh.totR.right=N*RS.stra$yh.right

  stra.result=cbind(Nh, Wh, yh.totR.est, yh.totR.var, yh.totR.sd, yh.totR.left, yh.totR.right)
  
  ytot.RS.est  =N*RS.mean$Est             
  ytot.RS.var  =N^2*RS.mean$Var
  ytot.RS.sd   =sqrt(ytot.RS.var)
  ytot.RS.left =N*RS.mean$Left
  ytot.RS.right=N*RS.mean$Right
  
  ytot.RS.result=matrix(c(ytot.RS.est, ytot.RS.var, ytot.RS.sd, ytot.RS.left, ytot.RS.right), nrow=1)
  colnames(ytot.RS.result)=c("Est", "Var", "SD", "Left", "Right")
  rownames(ytot.RS.result)="TOTAL_RS"
  return(list(stra.result=as.data.frame(stra.result), ytot.RS.result=as.data.frame(ytot.RS.result)))
}



combined.ratio.mean=function(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
{
  yst.result=stra.srs.mean2(Nh, y.sample, stra.index, alpha)$mean.result
  xst.result=stra.srs.mean2(Nh, x.sample, stra.index, alpha)$mean.result
  
  ratio.est=yst.result$mean.est/xst.result$mean.est
  yRC.est=Xbar*ratio.est
  
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
  
  fh=nh/Nh
  nf=(1-fh)/nh
  
  stra.result=cbind(Nh, Wh, nh, fh)
  
  yRC.var=sum(Wh^2*nf*(sy2+ratio.est^2*sx2-2*ratio.est*syx))
  yRC.sd =sqrt(yRC.var)

  yRC.ci=conf.interval(yRC.est, yRC.sd, alpha)
  yRC.left =yRC.ci$left
  yRC.right=yRC.ci$right
  
  yRC.result=matrix(c(yRC.est, yRC.var, yRC.sd, yRC.left, yRC.right), nrow=1)
  colnames(yRC.result)=c("Est", "Var", "SD", "Left", "Right")
  rownames(yRC.result)="Mean_RC"
  return(list(stra.result=as.data.frame(stra.result), yRC.result=as.data.frame(yRC.result)))
}


combined.ratio.total=function(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
{
  N=sum(Nh)

  mean.RC.result=combined.ratio.mean(Nh, y.sample, x.sample, stra.index, Xbar, alpha)
  RC.stra=mean.RC.result$stra.result
  RC.mean=mean.RC.result$yRC.result
  
  ytot.RC.est  =N*RC.mean$Est             
  ytot.RC.var  =N^2*RC.mean$Var
  ytot.RC.sd   =sqrt(ytot.RC.var)
  ytot.RC.left =N*RC.mean$Left
  ytot.RC.right=N*RC.mean$Right
  
  ytot.RC.result=matrix(c(ytot.RC.est, ytot.RC.var, ytot.RC.sd, ytot.RC.left, ytot.RC.right), nrow=1)
  colnames(ytot.RC.result)=c("Est", "Var", "SD", "Left", "Right")
  rownames(ytot.RC.result)="TOTAL_RC"
  return(list(stra.result=as.data.frame(RC.stra), ytot.RC.result=as.data.frame(ytot.RC.result)))
}



