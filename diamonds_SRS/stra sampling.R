## stratified random application to diamonds data:
stra.sampling=function(N,n,full.data,allocation)
{
  full.data1=filter(full.data,full.data$cut=='Fair')
  print(full.data1)
  Nh1=nrow(full.data1)
  Wh1=Nh1/N
  nh1=n*Wh1
  stra.srs.sample1=srs.sampling(Nh1,nh1,full.data1)
  print(stra.srs.sample1)
  
  full.data2=filter(full.data,full.data$cut=='Good')
  print(full.data2)
  Nh2=nrow(full.data2)
  Wh2=Nh2/N
  nh2=n*Wh2
  stra.srs.sample2=srs.sampling(Nh2,nh2,full.data2)
  print(stra.srs.sample2)
  
  
  full.data3=filter(full.data,full.data$cut=='Very Good')
  print(full.data3)
  Nh3=nrow(full.data3)
  Wh3=Nh3/N
  nh3=n*Wh3
  stra.srs.sample3=srs.sampling(Nh3,nh3,full.data3)
  print(stra.srs.sample3)
  
  
  full.data4=filter(full.data,full.data$cut=='Premium')
  print(full.data4)
  Nh4=nrow(full.data4)
  Wh4=Nh4/N
  nh4=n*Wh4
  stra.srs.sample4=srs.sampling(Nh4,nh4,full.data4)
  print(stra.srs.sample4)
  
  
  full.data5=filter(full.data,full.data$cut=='Ideal')
  print(full.data5)
  Nh5=nrow(full.data5)
  Wh5=Nh5/N
  nh5=n*Wh5
  stra.srs.sample5=srs.sampling(Nh5,nh5,full.data5)
  print(stra.srs.sample5)
  
  Nh=cbind(Nh1,Nh2,Nh3,Nh4,Nh5)
  Wh=cbind(Wh1,Wh2,Wh3,Wh4,Wh5)
  nh=cbind(round(nh1),round(nh2),round(nh3),round(nh4),round(nh5))
  
  return(list(n=n, allocation=allocation, Nh=Nh, Wh=Wh, nh=nh,
              stra.srs.sample1=stra.srs.sample1,stra.srs.sample2=stra.srs.sample2,stra.srs.sample3=stra.srs.sample3,stra.srs.sample4=stra.srs.sample4,stra.srs.sample5=stra.srs.sample5))
}


