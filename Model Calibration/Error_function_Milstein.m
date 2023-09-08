function Parameters = Error_function_Milstein(k,theta,epsilon,lambda,rho,v,r,S,T,M,strike,mkt_prices,alpha)
p = [k,theta,epsilon,lambda,rho,v];
options.FunctionTolerance = 5e-2;
Parameters = lsqnonlin(@nestedfun,p,[0,0,0,-1,-1,0],[Inf,Inf,Inf,1,1,Inf],options);
    function Price_error_Milstein = nestedfun(p)
      rng(4);
      [~,M_Recursive_sum] = MonteCarlo(p(1),p(2),p(3),p(4),p(5),p(6),r,S,T,strike,M);
      Price_error_Milstein = [M_Recursive_sum-mkt_prices,alpha.*p];
    end
end
