soil=c('sand','loam')
theta_s=c(0.43,0.43)
theta_r=c(0.045, 0.078)
alpha=c(0.145,0.036)
n=c(2.68,1.56)
m=1-(1/n)
K_s=c(29.7,1.04) #convert to cm/s

theta_sand=seq(theta_r[1],theta_s[1],0.02)
theta_loam=c(0,seq(theta_r[2],theta_s[2],0.02),theta_s[2])

Se_sand=(theta_sand-theta_r[1])/(theta_s[1]-theta_r[1])
Se_loam=(theta_loam-theta_r[2])/(theta_s[2]-theta_r[2])

h_sand=(((((Se_sand)^(1/-m[1]))-1)^(1/n[1]))/alpha[1])
h_loam=(((((Se_loam)^(1/-m[2]))-1)^(1/n[2]))/alpha[2])

K_theta_sand=K_s[1]*sqrt(Se_sand)*((1-(1-((Se_sand^(1/m[1]))^m[1])))^2)
K_theta_loam=K_s[2]*sqrt(Se_loam)*((1-(1-((Se_loam^(1/m[2]))^m[2])))^2)

deltatheta=0.02 #cm^3/cm^3
gamma=1 #surf tension g/cm-s^2
nu = 0.000727 #viscosity 
#tau = tortuosity
rho=0.998 #g/cm^3
g=981 #cm/s

tau_sand_sat=sqrt(1/K_s[1]*2*nu*rho*g/deltatheta/(gamma^2))
tau_loam_sat=sqrt(1/K_s[2]*2*nu*rho*g/deltatheta/(gamma^2))
tau_sand=NA
tau_loam=NA
for(i in 0:19) {
  tau_sand[20-i] = sqrt(1/K_theta_sand[20-i]*2*nu*rho*g/deltatheta/(gamma^2))
  tau_loam[20-i] = sqrt(1/K_theta_loam[20-i]*2*nu*rho*g/deltatheta/(gamma^2))
}
par(mfrow=c(1,2))
plot(theta_sand,tau_sand)
plot(theta_loam,tau_loam)

# y = ax^b => log10(y)=log10(a)+b*log10(x)
sandmod=lm(log10(tau_sand[2:20])~log10(Se_sand[2:20]))
loammod=lm(log10(tau_loam[3:18])~log10(Se_loam[3:18]))
plot(log10(tau_sand[2:20])~log10(Se_sand[2:20]))
abline(sandmod)
plot(log10(tau_loam[3:18])~log10(Se_loam[3:18]))
abline(loammod)
a_sand=10^sandmod$coefficients[1]
b_sand=sandmod$coefficients[2]
a_loam=10^loammod$coefficients[1]
b_loam=loammod$coefficients[2]


summary(sandmod)

#the a term equals the tortuosity at saturation