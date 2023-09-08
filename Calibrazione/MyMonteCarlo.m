function [E_Recursive_sum,M_Recursive_sum] = MyMonteCarlo(k,theta,epsilon,lambda,rho,v,r,S,T,M)
    E_Recursive_sum = 0;
    M_Recursive_sum = 0;
    for j = 1:M
        [S_T1,S_T2] = Discretizzazione(k,theta,epsilon,lambda,rho,v,r,S,T);
        E_Recursive_sum = E_Recursive_sum + S_T1;
        M_Recursive_sum = M_Recursive_sum + S_T2;
    end
    E_Recursive_sum = E_Recursive_sum.*(1/M); % .*(exp(-r*T))
    M_Recursive_sum = M_Recursive_sum.*(1/M); % .*(exp(-r*T))