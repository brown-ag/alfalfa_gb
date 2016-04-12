delsolH=1700 #Enthalpy of dissolution for oxygen divided by gas constant
trange=seq(0,100)+273.15 #temp range in Kelvin

henryT <- function(tt,Hstd,enthdiss) {
  Hstd * exp(-enthdiss*((1/tt) - (1/298.15)))
}

plot(trange-273.15,0.2095/henryT(trange,770,delsolH))

#Sander, R. (2015), "Compilation of Henry's law constants (version 4.0) for water as solvent", Atmos. Chem. Phys. 15: 4399–4981, doi:10.5194/acp-15-4399-2015