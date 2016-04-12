#retention curve
soil=c('loamy sand','loam')
theta_s=c(0.53,0.51)
theta_r=c(0.05, 0.07)
alpha=c(0.04,0.01)
n=c(1.68,1.50)
m=1-(1/n)
K_s=c(244/24,25.6/24)

Se_ls=(1+(alpha[1]*seq(0,10000,0.1))^n[1])^(-m[1])*(theta_s[1]-theta_r[1])+theta_r[1]
Se_l=(1+(alpha[2]*seq(0,10000,0.1))^n[2])^(-m[2])*(theta_s[2]-theta_r[2])+theta_r[2]


plot(Se_ls,log10(seq(0,10000,0.1)),type="l",xlab="Volumetric water content, cm3/cm3",ylab="log(h) Matric potential, cm H2O")
lines(Se_l,log10(seq(0,10000,0.1)),type="l")
