Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n) 
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
  X<-xi[,3]
  Binom<-as.numeric(cut(X,breaks = c(0,1/8,4/8,7/8,1),include.lowest = T))-1
  (tabel<-table(Binom)/length(Binom))
  print(Binom)
}
Additive_RNG(35,11123,437,138,100)


######
Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n) 
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
  X<-xi[,3]
  i<-1000
  lambda<-4
  U<-runif(i)
  X<--log(U)/lambda
  hist(X)
  X<- rexp(16,4)
  X
}
Additive_RNG(35,11123,437,138,100)
