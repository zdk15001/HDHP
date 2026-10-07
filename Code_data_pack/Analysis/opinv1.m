function ex = opinv1(s, hsa, hra, p, tau, tr,d,r,t, limit, hd)
% s: spending; hsa: HSA contribution from firm; HRA: HRA contribution from
% firm; p: premium; tau: income tax; tr: tax on interests earned; d:
% discount rate; r: interest rate; hd: plan is HD; limit: maximum amount
% one could contribute to HSA, including employer contribution.

% The function calculate if under both plan, people invest all of the
% income after paying prm and oop.

% setting limit == hsa is equivalent as not allowing self-contribution.

% personal contribution to match the maximum allowed
    self = (limit - hsa) .* (hd == 1) .* (hra == 0) .* (limit >= hsa); 
    
    A = -p*(1-tau)-(s-hra).*(s>=hra) - self * (1-tau);
    income = (A*(1+r)^t-A)*(1-tr)/(1+d)^t + A + ((hsa+self)*(1+r)^t)/(1+d)^t;
    ex = -income;
    
end