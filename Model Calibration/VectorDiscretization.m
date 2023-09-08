function [SVector_Euler,SVector_Milstein] = VectorDiscretization(k,theta,epsilon,lambda,rho,v,r,S,N)
  % S è il prezzo iniziale
  % v è il valore iniziale della varianza
  % k è il tasso di Mean-Reversion
  % theta è valore medio della varianza
  % epsilon la volatilità della varianza
  % r è il tasso di interesset del Risk-Free asset
  % N è il numero di intervalli da considerare
  delta = 1/N;
  SVector_Euler = [S,zeros(1,N)];
  SVector_Milstein = [S,zeros(1,N)];
  vE = v;
  vM = v;
  B_s = normrnd(0,1,N,1);
  B_v = normrnd(0,1,N,1);
  Z_s = B_s;
  Z_v = rho*Z_s + sqrt(1-(rho^2))*B_v;
  for j = 1:N 
    SVector_Euler(j+1) = SVector_Euler(j) + r*SVector_Euler(j)*delta + SVector_Euler(j)*sqrt(vE*delta)*Z_s(j);
    vE = max((vE + (k*theta  -(k+lambda)*vE)*delta + epsilon*sqrt(vE*delta)*Z_v(j)),0);
  end
  for j = 1:N 
    SVector_Milstein(j+1) = SVector_Milstein(j) + r*SVector_Milstein(j)*delta + SVector_Milstein(j)*sqrt(vM*delta)*Z_s(j) + (0.5*SVector_Milstein(j)*delta*vM)*((Z_s(j)^2)-1);
    vM = max((vM + (k*theta  -(k+lambda)*vE)*delta + epsilon*sqrt(vM*delta)*Z_v(j) + (0.25*(epsilon^2)*delta)*((Z_v(j)^2)-1)),0);
  end
end