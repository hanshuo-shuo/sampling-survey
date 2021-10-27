## simple random application to diamonds data:
srs.sampling=function(N,n,full.data)
{
  srs.subset=sample(1:N,n)
  srs.sample=full.data[srs.subset, ]
  return(srs.sample)
}