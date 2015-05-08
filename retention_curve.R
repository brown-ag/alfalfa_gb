#retention curve
soil=c('loam','sandy loam')
theta_s=c(0.44,0.41)
theta_r=c(0.07, 0.06)
alpha=c(0.009,0.028)
n=c(1.53,1.46)
m=1-(1/n)
K_s=c(18.84/24,126/24)
Se_l=(1+(alpha[1]*seq(0,1000,1))^n[1])^(-m[1])*(theta_s[1]-theta_r[1])+theta_r[1]
Se_s=(1+(alpha[2]*seq(0,1000,1))^n[2])^(-m[2])*(theta_s[2]-theta_r[2])+theta_r[2]
