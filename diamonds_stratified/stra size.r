### stratified random sampling ################################
### wh for three allocations ##################################
strata.weight=function(Wh, S2h, Ch=NULL, allocation)
{
  if (allocation=="Prop") 
  {
    wh=Wh
  }
  if (allocation=="Opt") 
  {
    wh=(Wh*sqrt(S2h)/sqrt(Ch))/sum(Wh*sqrt(S2h)/sqrt(Ch))
  }
  if (allocation=="Neyman") 
  {
    wh=(Wh*sqrt(S2h))/sum(Wh*sqrt(S2h))
  }
  return(wh)
}

### stratum size nh with given n ##############################
strata.size=function(n, Wh, S2h, Ch=NULL, allocation)
{
  wh=strata.weight(Wh, S2h, Ch, allocation)
  nh=wh*n 
  return(list(n=n, allocation=allocation, wh=wh, nh=round(nh)))
}

### transformation of V,C,d,r bounds ##########################
VCdr.transfer=function(method, bound, Ybar=NULL, alpha=NULL)
{
  if (method=="V") 
  {
    var.bound=bound
  }
  
  if (method=="C")
  {
    var.bound=(bound*Ybar)^2
  }
  
  if (method=="d") 
  { 
    quan=qnorm(1-alpha/2)
    var.bound=(bound/quan)^2
  }
  
  if (method=="r") 
  {
    quan=qnorm(1-alpha/2)
    var.bound=(bound*Ybar/quan)^2
  }
  return(var.bound)
}

### size n for mean given VCdr of stratified random sampling ############
strata.mean.size=function(Nh, S2h, Ch=NULL, allocation, method, bound, Ybar=NULL, alpha=NULL)
{
  N=sum(Nh)
  Wh=Nh/N
  
  wh=strata.weight(Wh, S2h, Ch, allocation)
  var.bound=VCdr.transfer(method, bound, Ybar, alpha)
  
  n=sum(Wh^2*S2h/wh)/(var.bound+sum(Wh*S2h)/N)
  nh=wh*n
return(list(method=method, bound=bound, allocation=allocation, n=round(n), nh=round(nh)))  
}

### size n for proportion given VCdr of stratified random sampling ############
strata.prop.size=function(Nh, Ph, Ch=NULL, allocation, method, bound, Ybar=NULL, alpha=NULL)
{
  S2h=(Nh/(Nh-1))*Ph*(1-Ph)
  size.result=strata.mean.size(Nh, S2h, Ch, allocation, method, bound, Ybar, alpha)
  return(size.result)  
}


