mp <- matrix(c(0, 0, -3/2, 2, -3/4, 1/2, 0, -3/4, -1, -2, 1, -1, 1/2, -3, 3/2, -3/2),4,4,byrow=T)
mp
e = eigen(mp)
e$values
e$vectors
foo = list(); es=list(); troo=list();
fee=-10:10

for(k in 1:length(fee)) {
  j = fee[k]-1
  foo[[k]]= matrix(c(1,j,-j,3),2,2,byrow=T);
  es[[k]] = eigen(foo[[k]])
  troo[[k]] = c(j,is.complex(es[[k]]$values))
}
troo
foo[11:13]
