#---------------------------------
#simulating data
#---------------------------------
n=100
p =300
SigmaX=matrix(0, nrow=p, ncol=p)
for(j in 1:p){
  for(k in 1:p){
    SigmaX[j,k]=0.75^abs(j-k)
  }
}
eo=eigen(SigmaX)
SigmaXsqrt=eo$vec%*%diag(eo$val^.5)%*%t(eo$vec)
SigmaX=eo$vec%*%diag(eo$val)%*%t(eo$vec)
beta=c(1, 0, 1, 0, -1, -1, rep(0, p-6))
lambda_values=10^seq(4, -4, length=50)
