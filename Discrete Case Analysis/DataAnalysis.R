library(Matrix)
library(tictoc)
library(optimbase)
library(mypkg)

# devtools::check()
# Remember: If N = 4 then must input N = 5 since N indicates the instants of time between 0 and N
# Remember: If t = 2 then must input t = 3 since t indicates the instants of time between 0 and t
# Remember: If input S is a vector of prices at time N then S is (S_0,S_1,...,S_N)

# Functions in the package ------------------------------------------------

# VCallOption_pricingfunction_det -> Return the price of an European Call Option using a deterministic formula
# VPutOption_pricingfunction_det -> Return the price of an European Put Option using a deterministic formula
# ECallOption_pricingfunction_det -> Return the price of an Asian Call Option using a deterministic formula
# EPutOption_pricingfunction_det -> Return the price of an Asian Put Option using a deterministic formula
# VCAllOption_pricingfunction_rand -> Return the price of an European Call Option using Monte Carlo
# VPutOption_pricingfunction_rand -> Return the price of an European Put Option using Monte Carlo
# ECAllOption_pricingfunction_rand -> Return the price of an Asian Call Option using Monte Carlo
# EPutOption_pricingfunction_rand -> Return the price of an Asian Put Option using Monte Carlo

# (eta,eps) is the vector respectively of (Quantity of Free-risk asset,Quantity of Risk asset) to hedge the contingent claim

# VCAllOption_hedgingfunction_det -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Deterministic
# VPutOption_hedgingfunction_det -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Deterministic
# ECallOption_hedgingfunction_det -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Deterministic
# EPutOption_hedgingfunction_det -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Deterministic
# VCallOption_hedgingfunction_rand -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Monte Carlo
# VPutOption_hedgingfunction_rand -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Monte Carlo
# ECallOption_hedgingfunction_rand -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Monte Carlo
# EPutOption_hedgingfunction_rand -> Return the 2 two-dimension vector (eta,eps) hedging the contingent claim - Monte Carlo

# Monte Carlo and Deterministic give the same results when difficult is low (N = 3)-----------
# Portfolio Pricing
set.seed(1)
VCallprice_det = VCallOption_pricingfunction_det(1,4,1.05,1/2,-0.3,0.5,0.1,0.99)
VCallprice_rand = VCallOption_pricingfunction_rand(1,4,1.05,1/2,-0.3,0.5,0.1,0.99,100000)

VPutprice_det = VPutOption_pricingfunction_det(1,4,1.05,1/2,-0.3,0.5,0.1,0.99)
VPutprice_rand = VPutOption_pricingfunction_rand(1,4,1.05,1/2,-0.3,0.5,0.1,0.99,100000)

ECallprice_det = ECallOption_pricingfunction_det(1,4,c(1),1/2,-0.3,0.5,0.1,0.99)
ECallprice_rand = ECallOption_pricingfunction_rand(1,4,c(1),1/2,-0.3,0.5,0.1,0.99,100000)

EPutprice_det = EPutOption_pricingfunction_det(1,4,c(1),1/2,-0.3,0.5,0.1,0.99)
EPutprice_rand = EPutOption_pricingfunction_rand(1,4,c(1),1/2,-0.3,0.5,0.1,0.99,100000)

# Option Hedging
set.seed(1)
VCallhedging_det = VCallOption_hedgingfunction_det(2,4,1.05,1/2,-0.3,0.5,0.1,0.99,1)
VCallhedging_rand = VCallOption_hedgingfunction_rand(2,4,1.05,1/2,-0.3,0.5,0.1,0.99,1,100000)

VPuthedging_det = VPutOption_hedgingfunction_det(2,4,1.05,1/2,-0.3,0.5,0.1,0.99,1)
VPuthedging_rand = VPutOption_hedgingfunction_rand(2,4,1.05,1/2,-0.3,0.5,0.1,0.99,1,100000)

ECallhedging_det = ECallOption_hedgingfunction_det(2,4,c(1),1/2,-0.3,0.5,1,0.1,0.99)
ECallhedging_rand = ECallOption_hedgingfunction_rand(2,4,c(1),1/2,-0.3,0.5,1,0.1,0.99,100000)

EPuthedging_det = EPutOption_hedgingfunction_det(2,4,c(1),1/2,-0.3,0.5,1,0.1,0.99)
EPuthedging_rand = EPutOption_hedgingfunction_rand(2,4,c(1),1/2,-0.3,0.5,1,0.1,0.99,100000)

# Asian Options, Deterministic approach against Monte Carlo approach  -------------------------------------------------------------------

# N ~ 20 - Deterministic formula has exponential complexity
set.seed(1)
tic()
ECallprice_det = ECallOption_pricingfunction_det(1,20,1.05,1/2,-0.3,0.5,0.1,0.99)
toc()
tic()
ECallprice_det = ECallOption_pricingfunction_det(1,21,1.05,1/2,-0.3,0.5,0.1,0.99)
toc()
tic()
ECallprice_det = ECallOption_pricingfunction_det(1,22,1.05,1/2,-0.3,0.5,0.1,0.99)
toc()

# N = 40 - Monte Carlo is faster and has also a good precision
set.seed(1)
tic()
ECallprice_rand = ECallOption_pricingfunction_rand(1,40,1.05,1/2,-0.3,0.5,0.1,0.99,100000)
print(ECallprice_rand)
toc()
tic()
ECallprice_rand = ECallOption_pricingfunction_rand(1,40,1.05,1/2,-0.3,0.5,0.1,0.99,1000000)
print(ECallprice_rand)
toc()
tic()
ECallprice_rand = ECallOption_pricingfunction_rand(1,40,1.05,1/2,-0.3,0.5,0.1,0.99,10000000)
print(ECallprice_rand)
toc()

# N = 80 - Monte Carlo is better than Deterministic
set.seed(1)
tic()
ECallprice_rand = ECallOption_pricingfunction_rand(1,80,1.05,1/2,-0.3,0.5,0.1,0.99,100000)
print(ECallprice_rand)
toc()
tic()
ECallprice_rand = ECallOption_pricingfunction_rand(1,80,1.05,1/2,-0.3,0.5,0.1,0.99,1000000)
print(ECallprice_rand)
toc()
tic()
ECallprice_rand = ECallOption_pricingfunction_rand(1,80,1.05,1/2,-0.3,0.5,0.1,0.99,10000000)
print(ECallprice_rand)
toc()

# Standard deviation of the Monte Carlo approach -------

# European Call Options - N = 10
set.seed(1)
tic()
N = 11 # Difficoltà
index_vector = c(10,20,40,100,150,200,250,300,350,400,450,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800) 
# vettore degli M per cui eseguiamo il Monte Carlo
indexvector_size = size(index_vector,2) # Numero di indici
y_axis = zeros(indexvector_size,2) # Tabella dei valori attesi e delle deviazioni standard
M_prime = 0 # Contatore
for (M in index_vector) {
  M_prime = M_prime + 1
  vector_of_values = rep(0,M) # Inizializziamo il vettore dei risultati del Monte Carlo per M fissato
  for (i in 1:M) { # T = M
    vector_of_values[i] = VCallOption_pricingfunction_rand(1,N,1.05,1/2,-0.3,0.5,0.1,0.99,M) # Inseriamo i risultati nel vettore
  }
  y_axis[M_prime,1] = mean(vector_of_values) # Compiliamo la tabella dei valori attesi
  y_axis[M_prime,2] = sd(vector_of_values) # Compiliamo la tabella delle deviazioni standard
}

out_lw=y_axis[1:indexvector_size,1]-y_axis[1:indexvector_size,2]  
out_up=y_axis[1:indexvector_size,1]+y_axis[1:indexvector_size,2] 

ymin=min(out_lw) # estremo inferiore dell'intervallo a 1 deviazione standard
ymax=max(out_up) # estremo superiore dell'intervallo a 1 deviazione standard
plot(index_vector,y_axis[1:indexvector_size,1],ylim=c(ymin,ymax)) # Grafico dei valori attesi del Monte Carlo al variare di M
arrows(index_vector,out_lw,index_vector,out_up,length=0.02,angle=90,code=3,col="green4") # Deviazioni standard del Monte Carlo al variare di M
E = VCallOption_pricingfunction_det(1,N,1.05,1/2,-0.3,0.5,0.1,0.99) # Valore Reale dell'opzione
lines(rep(E,2000),col="red") # Linea del valore atteso reale
toc() # 220 s

# Asian Call Options - N = 10
set.seed(1)
tic()
N = 11 # Difficoltà
index_vector = c(10,20,40,100,150,200,250,300,350,400,450,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800) 
# vettore degli M per cui eseguiamo il Monte Carlo
indexvector_size = size(index_vector,2) # Numero di indici
y_axis = zeros(indexvector_size,2) # Tabella dei valori attesi e delle deviazioni standard
M_prime = 0 # Contatore
for (M in index_vector) {
  M_prime = M_prime + 1
  vector_of_values = rep(0,M) # Inizializziamo il vettore dei risultati del Monte Carlo per M fissato
  for (i in 1:M) { # T = M
    vector_of_values[i] = ECallOption_pricingfunction_rand(1,N,1.05,1/2,-0.3,0.5,0.1,0.99,M) # Inseriamo i risultati nel vettore
  }
  y_axis[M_prime,1] = mean(vector_of_values) # Compiliamo la tabella dei valori attesi
  y_axis[M_prime,2] = sd(vector_of_values) # Compiliamo la tabella delle deviazioni standard
}

out_lw=y_axis[1:indexvector_size,1]-y_axis[1:indexvector_size,2]  
out_up=y_axis[1:indexvector_size,1]+y_axis[1:indexvector_size,2] 

ymin=min(out_lw) # estremo inferiore dell'intervallo a 1 deviazione standard
ymax=max(out_up) # estremo superiore dell'intervallo a 1 deviazione standard
plot(index_vector,y_axis[1:indexvector_size,1],ylim=c(ymin,ymax)) # Grafico dei valori attesi del Monte Carlo al variare di M
arrows(index_vector,out_lw,index_vector,out_up,length=0.02,angle=90,code=3,col="green4") # Deviazioni standard del Monte Carlo al variare di M
E = ECallOption_pricingfunction_det(1,N,1.05,1/2,-0.3,0.5,0.1,0.99) # Valore Reale dell'opzione
lines(rep(E,2000),col="red") # Linea del valore atteso reale
toc() #318 s

# Asian Call Options - N = 40
tic()
N = 41 # Difficoltà
index_vector = c(10,20,40,100,150,200,250,300,350,400,450,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,2000,2200,2400) 
# vettore degli M per cui eseguiamo il Monte Carlo
indexvector_size = size(index_vector,2) # Numero di indici
y_axis = zeros(indexvector_size,2) # Tabella dei valori attesi e delle deviazioni standard
M_prime = 0 # Contatore
for (M in index_vector) {
  M_prime = M_prime + 1
  vector_of_values = rep(0,M) # Inizializziamo il vettore dei risultati del Monte Carlo per M fissato
  for (i in 1:M) { # T = M
    vector_of_values[i] = ECallOption_pricingfunction_rand(1,N,1.05,1/2,-0.3,0.5,0.1,0.99,M) # Inseriamo i risultati nel vettore
  }
  y_axis[M_prime,1] = mean(vector_of_values) # Compiliamo la tabella dei valori attesi
  y_axis[M_prime,2] = sd(vector_of_values) # Compiliamo la tabella delle deviazioni standard
}

out_lw=y_axis[1:indexvector_size,1]-y_axis[1:indexvector_size,2]  
out_up=y_axis[1:indexvector_size,1]+y_axis[1:indexvector_size,2] 

ymin=min(out_lw) # estremo inferiore dell'intervallo a 1 deviazione standard
ymax=max(out_up) # estremo superiore dell'intervallo a 1 deviazione standard
plot(index_vector,y_axis[1:indexvector_size,1],ylim=c(ymin,ymax)) # Grafico dei valori attesi del Monte Carlo al variare di M
arrows(index_vector,out_lw,index_vector,out_up,length=0.02,angle=90,code=3,col="green4") # Deviazioni standard del Monte Carlo al variare di M
toc() #735 s
