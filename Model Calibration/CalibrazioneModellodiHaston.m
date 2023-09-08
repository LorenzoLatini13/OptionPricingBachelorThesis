% Prova che le discretizzazioni funzionano
%k = 8;
%theta = 0.002;
%epsilon = 1.5;
%lambda = 0.02;
%rho = 0.002;
%v = 0.0019;
%r = 0.02;
%S = 100;
%T = 10;
%[y,z] = VectorDiscretization(k,theta,epsilon,lambda,rho,v,r,S,T);
%x = 0:T;
%plot(x,y)
%hold
%plot(x,z)
%[S_Euler,S_Milstein] = Discretizzazione(k,theta,epsilon,lambda,rho,v,r,S,T);

% DATI INIZIALI
k = 3; % I primi 3 parametri vanno a definire il comportamento [1e-3,5]
theta = 0.05; % Della volatilità, se sono troppo grandi il sottostante [1e-3,0.1]
epsilon = 0.3; % Si comporta in maniera incontrollata [1e-2,1]
lambda = 0.03; % Anche questo interagisce con la volatilità, più e grande meno 
          % Controllabili saranno i risultati Per valori negativi 
          % Sembra stabilizzare il comportamentodell'asset, in effetti 
          % Se è molto negativo la varianza è nulla a causa di max(v,0)
rho = -0.8; % Più è alta più il prezzo è stabile - Correlazion negativa
         % causa un calo medio nel prezzo dell'asset
v = 0.1; % Il valore iniziale non sembra avere grossi effetti sul prezzo,
         % probabilmente da quanto incidono i parametri che la modificano
         % se questi fanno casino non incide, altrimenti si
Par = [k,theta,epsilon,lambda,rho,v];
r = 0.0001; % Circa il 3% annuo
S = 20; % Prezzo di partenza
T = 365; % Scadenza ad un anno
[y,z] = VectorDiscretization(k,theta,epsilon,lambda,rho,v,r,S,T);
x = 0:T;
plot(x,y)
hold
plot(x,z)

% Monte Carlo 
strike = 10;
M = 10000;
ST_Matrix = zeros(100,2);
tic
for i = 1:100
    [E_ST1,E_ST2] = MyMonteCarlo(k,theta,epsilon,lambda,rho,v,r,S,T,M); % Dove si troverà il prezzo tra un anno
    ST_Matrix(i,1)= E_ST1;
    ST_Matrix(i,2)= E_ST2;
end
Mean_ST_Matrix = mean(ST_Matrix)
std_ST_Matrix = std(ST_Matrix)
toc % 40 sec 

tic
prices_Matrix = zeros(100,2);
for i = 1:100
    [prices_E,prices_M] = MonteCarlo(k,theta,epsilon,lambda,rho,v,r,S,T,strike,M); % Dove si troverà il prezzo tra un anno
    prices_Matrix(i,1)= prices_E;
    prices_Matrix(i,2)= prices_M;
end
Mean_prices_Matrix = mean(prices_Matrix)
std_prices_Matrix = std(prices_Matrix)
toc % 54 sec

% Ottimizzazione parametri
mkt_prices = 15;
alpha = 1.0e-4;
tic
Par_E = Error_function_Euler(k,theta,epsilon,lambda,rho,v,r,S,T,M,strike,mkt_prices,alpha)
Par_M = Error_function_Milstein(k,theta,epsilon,lambda,rho,v,r,S,T,M,strike,mkt_prices,alpha)
toc %
% Nuovi valori con Monte Carlo - Ci aspettiamo siano vicini a 15
NewST_Matrix = zeros(100,2);
tic
for i = 1:100
    [NewE_ST1,~] = MyMonteCarlo(Par_E(1),Par_E(2),Par_E(3),Par_E(4),Par_E(5),Par_E(6),r,S,T,M); % Dove si troverà il prezzo tra un anno
    [~,NewE_ST2] = MyMonteCarlo(Par_M(1),Par_M(2),Par_M(3),Par_M(4),Par_M(5),Par_M(6),r,S,T,M); % Dove si troverà il prezzo tra un anno
    NewST_Matrix(i,1)= NewE_ST1;
    NewST_Matrix(i,2)= NewE_ST2;
end
Mean_NewST_Matrix = mean(NewST_Matrix)
std_NewSTMatrix = std(NewST_Matrix)
toc % 80 sec

Newprices_Matrix = zeros(100,2);
tic
for i = 1:100
    [Newprices_E,~] = MonteCarlo(Par_E(1),Par_E(2),Par_E(3),Par_E(4),Par_E(5),Par_E(6),r,S,T,strike,M);
    [~,Newprices_M] = MonteCarlo(Par_M(1),Par_M(2),Par_M(3),Par_M(4),Par_M(5),Par_M(6),r,S,T,strike,M);
    Newprices_Matrix(i,1)= Newprices_E;
    Newprices_Matrix(i,2)= Newprices_M;
end
Mean_Newprices_Matrix = mean(Newprices_Matrix)
std_NewpricesMatrix = std(Newprices_Matrix)
toc