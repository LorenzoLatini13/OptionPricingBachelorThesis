function [E_Recursive_sum,M_Recursive_sum] = MonteCarlo(k,theta,epsilon,lambda,rho,v,r,S,T,strike,M)
    E_Recursive_sum = 0;
    M_Recursive_sum = 0;
    for j = 1:M
        [S_T1,S_T2] = Discretizzazione(k,theta,epsilon,lambda,rho,v,r,S,T);
        E_Recursive_sum = E_Recursive_sum + max((S_T1 - strike),0);
        M_Recursive_sum = M_Recursive_sum + max((S_T2 - strike),0);
    end
    E_Recursive_sum = E_Recursive_sum.*(1/M).*(exp(-r*T));
    M_Recursive_sum = M_Recursive_sum.*(1/M).*(exp(-r*T));
end

