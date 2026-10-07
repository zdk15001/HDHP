function ex = oph(s, hsa, hra, tau, p, hd, limit)

% The function calculate net payment assuming: 1) consumers pay oop with
% HRA pre-tax as much as possible. If there is left-over, set them as zero. 
% 2) % consumers pay oop with HSA pre-tax as much as possible. Further, 
% they could also contribute to HSA account up to the limit (the limit
% includes employer contribution) and pay oop with them pre-tax. If there
% is left-over of HSA then deduct them directly from premium.

% Not allowing extra contribution into HSA is equivalent as setting limit
% the same as HSA

% s: raw oop expenditure
% tau: income tax rate
% p: premium
% hd: indicate whether a plan is a HD plan
% limit: max contribution limit into HSA

% net income after paying tax:

    tp = (1 - tau) .* (-p) + ... % no tax on premium
        hd .* (hra == 0) .* (limit - hsa) .* tau .* (limit >= hsa) + ... % tax deduction from self-contribution to HSA
        hsa - (s - hra) .* (s>hra); % spending and HSA/HRA

    ex = -tp;
end

% % unused code:
%     self = (limit - hsa) .* (hd == 1) .* (hra == 0) .* (limit >= hsa); 
%     limit2 = self + hsa;
%     
% %     ex = p + (hra > 0) .* (s - hra) .* (s >= hra) ./ (1 - tau) + ...
% %         (hsa > 0) .* ( (s - hsa ) .* (s >= hsa) ./ (1 - tau) - ...
% %         (hsa - s) .* (s < hsa)) + (hra == 0) .* (hsa == 0) .* s ./ (1 - tau);
%     
%     ex = p + (hra > 0) .* (s - hra) .* (s >= hra) ./ (1 - tau) + ... %HRA
%         (hd == 1) .* (hra == 0) .* ... % HSA...
%         ((s - hsa) .* (s >= hsa) .* (s < limit) - ... % HSA and pay with hsa and some portion contributed pre-tax
%         (hsa - s) .* (s < hsa) + ... % HSA and pay with HSA
%         ((s-limit2) ./ (1-tau) + self).* (s > limit2)) + ... % HSA and pay with both HSA and all contribution; the rest pay post-tax
%         (hra == 0) .* (hsa == 0) .* s ./ (1 - tau);