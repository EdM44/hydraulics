# hydraulics

This R package contains functions to 1) describe properties of water, 2) solve the Darcy-Weisbach equation
for friction loss through pipes, and 3) plot a Moody diagram. 

# Installation

```R
# install.packages("hydraulics") someday, when this is on CRAN
devtools::install_github("EdM44/hydraulics")
```

# Examples (see more examples in the function descriptions)

```R
library("hydraulics")

# 1)
#Type 1 problem (solve for friction loss): Eng (US) units
D <- 20/12  
L <- 10560
Q <- 4
T <- 60
ks <- 0.0005

#find the friction factor, f:
f <- colebrook(ks=ks,V=velocity(D,Q), D=D, nu=kvisc(T=T, units="Eng"))

#solve for this missing value 
darcyweisbach(Q=Q,D=D, L = L, ks = ks, nu = kvisc(T=T, units="Eng"), units = c("Eng"))

# 2) Type 2 (solving for flow rate, Q): SI Units
D <- .5
L <- 10
hf <- 0.006*L
T <- 20
ks <- 0.000046
darcyweisbach(D = D, hf = hf, L = L, ks = ks, nu = kvisc(T=T, units='SI'), units = c('SI'))

#Type 3 (solving for diameter, D): Eng (US) units
Q <- 37.5     #flow in ft^3/s
L <- 8000     #pipe length in ft
hf <- 215     #head loss due to friction, in ft
T <- 68       #water temperature, F
ks <- 0.0008  #pipe roughness, ft
darcyweisbach(Q = Q, hf = hf, L = L, ks = ks, nu = kvisc(T=T, units='Eng'), units = c('Eng'))

#Utility finctions can be used independently as well:
#Find kinematic viscocity for water temperature of 55 F
nu = kvisc(T = 55, units = 'Eng')

#Find kinematic viscocity assuming default water temperature of 68 F
nu = kvisc(units = 'Eng')

#Find water density for water temperature of 25 C
rho = kvisc(T = 25, units = 'SI')
