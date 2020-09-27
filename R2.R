ul<-rnorm(30)
print("loop calculates square of first 10 elements")
usq<-0
for(i in 1:10)
{
  usq[i]<-ul[i]*ul[i]
  print(usq[i])
}
for (i in 1:10)
{
  u[i]<-sqrt(usq[i])
  print(u[i])
}

x<-rnorm(5)
y<-rnorm(5)
d<-as.data.frame(cbind(x,y))
print(d)


#install ggplot2

mymat<-matrix(nrow=10,ncol=10)
for (i in 1:dim(mymat)[1])
  {for(j in 1:dim(mymat)[2])
  {mymat[i,j]=i*j}
}
print (mymat)