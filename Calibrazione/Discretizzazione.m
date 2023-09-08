function [S_Euler,S_Milstein] = Discretizzazione(k,theta,epsilon,lambda,rho,v,r,S,T)
  % S è il prezzo iniziale
  % v è il valore iniziale della varianza
  % k è il tasso di Mean-Reversion
  % theta è valore medio della varianza
  % epsilon la volatilità della varianza
  % lambda coefficiente Risk-Premium della varianza
  % r è il tasso di interesse del Risk-Free asset
  % T è il numero di intervalli da considerare
  delta = 1/T;
  S_Euler = S; % S_T - Schema di Eulero
  S_Milstein = S; % S_T - Schema di Milstein % S è il prezzo iniziale
  vE = v;
  vM = v;
  B_s = normrnd(0,1,T,1);
  B_v = normrnd(0,1,T,1);
  Z_s = B_s;
  Z_v = rho*Z_s + sqrt(1-(rho^2))*B_v;
  % Calcolo di S_T con lo Schema di Eulero rispetto alla misura P*
  for j = 1:T 
    S_Euler = S_Euler + r*S_Euler*delta + S_Euler*sqrt(vE*delta)*Z_s(j);
    vE = max((vE + (k*theta  -(k+lambda)*vE)*delta + epsilon*sqrt(vE*delta)*Z_v(j)),0);
  end
  % Calcolo di S_T con lo Schema di Milstein rispetto alla misura P*
  for j = 1:T 
    S_Milstein = S_Milstein + r*S_Milstein*delta + S_Milstein*sqrt(vM*delta)*Z_s(j) + (0.5*S_Milstein*delta*vM)*((Z_s(j)^2)-1);
    vM = max((vM + (k*theta  -(k+lambda)*vE)*delta + epsilon*sqrt(vM*delta)*Z_v(j) + (0.25*(epsilon^2)*delta)*((Z_v(j)^2)-1)),0);
  end
end