conf.interval=function(para.hat, SD.hat, alpha)
##############    Input     ########################
## para.hat = estimator of character
## SD.hat   = estimator of sd
## alpha    = confidence level (1-alpha)
##############    Output    ########################
## d        = absolute error
## r        = relative error
## left     = left of confidence interval
## right    = right of confidence interval
####################################################

{
  quan=qnorm(1-alpha/2)
  
  d=quan*SD.hat
  r=quan*SD.hat/para.hat
  
  para.left =para.hat-quan*SD.hat
  para.right=para.hat+quan*SD.hat
  
  return(list(d=d, r=r, left=para.left, right=para.right))
}