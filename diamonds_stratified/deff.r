deff=function(var.result)
{
deff.vector=var.result/var.result[1]
deff.result=cbind(var.result, deff.vector)
deff.result=round(deff.result,5)

colnames(deff.result)=c("Var", "Deff")
return(deff.result=as.data.frame(deff.result))
}


deff.size=function(deff.result, size1)
{
  size2=size1*deff.result$Deff
  deff.result$Size=round(size2)
return(deff.result)
}


