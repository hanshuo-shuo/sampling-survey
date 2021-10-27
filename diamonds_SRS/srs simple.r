srs.mean=function(N, mysample, alpha)
{
  n=length(mysample)
  f=n/N
  
  ybar=mean(mysample)
  ys2 =var(mysample)
  
  ybar.var=((1-f)/n)*ys2
  ybar.sd =sqrt(ybar.var)
  
  ci.result=conf.interval(ybar, ybar.sd, alpha)
  d    =ci.result$d
  r    =ci.result$r
  left =ci.result$left
  right=ci.result$right
  
  return(list(ybar=ybar, ybar.var=ybar.var, ybar.sd=ybar.sd, 
              d=d, r=r, left=left, right=right))
}



srs.total=function(N, mysample, alpha)
{
  n=length(mysample)
  f=n/N
  
  ybar=mean(mysample)
  ys2 =var(mysample)
  
  ytot.est=N*ybar
  ytot.var=N^2*((1-f)/n)*ys2
  ytot.sd =sqrt(ytot.var)
  
  ci.result=conf.interval(ytot.est, ytot.sd, alpha)
  d    =ci.result$d
  r    =ci.result$r
  left =ci.result$left
  right=ci.result$right
  
  return(list(ytot.est=ytot.est, ytot.var=ytot.var, ytot.sd=ytot.sd, 
              d=d, r=r, left=left, right=right))
}



srs.prop=function(N=NULL, n, event.num, alpha)
{
  f=ifelse(is.null(N), 0, n/N)
  
  p.est=event.num/n
  p.var=((1-f)/(n-1))*p.est*(1-p.est)
  p.sd =sqrt(p.var)

  ci.result=conf.interval(p.est, p.sd, alpha)
  d    =ci.result$d
  r    =ci.result$r
  left =ci.result$left
  right=ci.result$right
  
  return(list(p.est=p.est, p.var=p.var, p.sd=p.sd, 
              d=d, r=r, left=left, right=right))
}


srs.num=function(N=NULL, n, event.num, alpha)
{
  f=ifelse(is.null(N), 0, n/N)
  
  p.est=event.num/n
  p.var=((1-f)/(n-1))*p.est*(1-p.est)
    
  A.est=N*p.est
  A.var=N^2*p.var
  A.sd =sqrt(A.var)
  
  ci.result=conf.interval(A.est, A.sd, alpha)
  d    =ci.result$d
  r    =ci.result$r
  left =ci.result$left
  right=ci.result$right
  
  return(list(A.est=round(A.est), A.var=A.var, A.sd=A.sd, 
              d=d, r=r, left=round(left), right=round(right)))
}


