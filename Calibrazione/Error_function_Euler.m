function Parameters = Error_function_Euler(k,theta,epsilon,lambda,rho,v,r,S,T,M,strike,mkt_prices,alpha)
p_0 = [k,theta,epsilon,lambda,rho,v];
options.FunctionTolerance = 5e-2;
Parameters = lsqnonlin(@nestedfun,p_0,[0,0,0,-1,-1,0],[Inf,Inf,Inf,1,1,Inf],options);
    function Price_error_Euler = nestedfun(p)
      rng(4);
      [E_Recursive_sum,~] = MonteCarlo(p(1),p(2),p(3),p(4),p(5),p(6),r,S,T,strike,M);
      Price_error_Euler = [E_Recursive_sum-mkt_prices,alpha.*p];
    end
end
