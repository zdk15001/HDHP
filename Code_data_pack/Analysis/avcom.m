%% KFF Project: Program Considering Separate Drug and Office Visits Ddct
% Created by: Chenyuan Liu (cliu439@wisc.edu)

% Created on: 27 Feb 2020; updated 12 Jun 2020

%% Import data
clear
clc
cd('\Analysis')%Put the folder path

data=xlsread('runavc.xlsx','output_normal');

%Replace HRA ctrb=0 if not hdhp
data(:,9)=data(:,9).*(data(:,4)==4); 

% Delete firms with only one plan
n=size(data,1);
data=[data zeros(n,1)];
for j = 1:n
    if j>1 && j<n-1
        if data(j,3)~=data(j-1,3) && data(j,3)~=data(j+1,3)
            data(j,end)=1;
        end
    end
end
% delete rows where less than two plans have valid info for the same firm
data(data(:,end)==1,:)=[];

firm=data(:,3);
pty=data(:,4);
nprm=data(:,5); %The imported nprm=premium*worker's ctrb-hsa ctrb
ddct=data(:,6); %general ddct
moop=data(:,7);
price=data(:,8); %total premium
hra=data(:,9); %HRA contribution
av=data(:,10); %Actuarial value of the plan
co=1-data(:,13); %Cost sharing: percentage paid by the consumer
pthr=data(:,15); %passthrough rate
ddct3b=data(:,16); %office visits subject to general ddct? 1=yes, 2=no.
csov=[data(:,17),data(:,18)/100,data(:,19),data(:,20)/100]; %ov cost sharing rule
ddct3c=data(:,21); %drug subject to general ddct?
cs27=data(:,22); %durg has separate ddct?
cs27a=data(:,23); %separate drug ddct. 0 means no separate drug ddct
csdrug=[data(:,25),data(:,26)/100,data(:,28),data(:,29)/100,data(:,31),data(:,32)/100,data(:,34),data(:,35)/100]; %drug cost sharing rule
hsa=round(price.*pthr-nprm);
n=size(data,1);

cont = price .* (1-pthr) + hsa + hra;
contdif = cont(2:2:end) - cont(1:2:end);
contdif = repelem(contdif,2,1);

%% Calculate dominance
comcontg = xlsread('riskdist.xlsx','goldcombined');
[test3, prmoopg, exc] = filldist5(data, comcontg); %Gold

% mannually adjust for firm with the same prm and moop
test3(firm == 4500, 1) = [1; 1];
test3(firm == 91, 1) = [-1; -1];

% export data to Stata
exdata = [firm; pty; test3(:,1)];

%% Tables 2. Strict Dominance Classification for Simplified Plan Representations
[sum(test3(:,1) == -3)/2, mean(test3(:,1) == -3); ...
    sum(test3(:,1) == -2)/2, mean(test3(:,1) == -2); ...
    sum(test3(:,1) == -1)/2, mean(test3(:,1) == -1); ...
    sum(test3(:,1) == 1)/2, mean(test3(:,1) == 1); ...
    sum(test3(:,1) == 2)/2, mean(test3(:,1) == 2); ...
    sum(test3(:,1) == 3)/2, mean(test3(:,1) == 3);
    size(test3,1)/2, 1]

%% Table 3. Average Savings and Risk-Adjusted Savings with the HD Plan
[mean(test3(:,5)), mean(test3(:,6)), mean(test3(:,7)); ...
   mean(test3(test3(:,1) == 3,5)), mean(test3(test3(:,1) == 3,6)), ...
   mean(test3(test3(:,1) == 3,7)); ... 
    mean(test3(test3(:,1) ~= 3,5)), mean(test3(test3(:,1) ~= 3,6)), ...
    mean(test3(test3(:,1) ~= 3,7))]

test4 = test3(2:2:end,:);
[std(test4(:,5)), std(test4(:,6)), std(test4(:,7)); ...
   std(test4(test4(:,1) == 3,5)), std(test4(test4(:,1) == 3,6)), ...
   std(test4(test4(:,1) == 3,7)); ... 
    std(test4(test4(:,1) ~= 3,5)), std(test4(test4(:,1) ~= 3,6)), ...
    std(test4(test4(:,1) ~= 3,7))]

% CARA in context
r1 = 0.0005;
r2 = 0.002;

eu = 0.5 * (1-exp(-r1*1000))/r1 + 0.5 * (1 - exp(-r1*0))/r1;
c1 = -log(1-r1*eu)/r1;
eu = 0.5 * (1-exp(-r2*1000))/r2 + 0.5 * (1 - exp(-r2*0))/r2;
c2 = -log(1-r2*eu)/r2;

u1 = (1-exp(r1*1000))/r1;
G1 = - log(1 + r1* u1) / r1;
if 1+r1*u1<0
    G1 = Inf;
end
u2 = (1-exp(r2*1000))/r2;
G2 = - log(1 + r2* u2) / r2;
if 1+r2*u2<0
    G2 = Inf;
end
% CARA in context
[c1, c2; G1, G2]

%% Table 4. Robustness with MEPS data
pmeps=xlsread('riskdist_meps2.xlsx','prop');
domimeps=zeros(2,4);
for i=1:2
    domimeps(i,:)=fillmeps(pmeps(:,i+3),prmoopg,comcontg);
end
domimeps(:,1) = domimeps(:,1)/331;
domimeps
% CARA in context
[c1, c2; G1, G2]

%% Table 5. Robustness with Tax and Investment Benefits of HD
res2 = zeros(7,5);

%%% benchmark: full sample
res2(1,1) = mean(test3(:,1) == 3);
res2(1,2) = mean(test3(:,3) == 3);
res2(1,3) = mean(test3(:,5));
res2(1,4) = mean(test3(:,6));
res2(1,5) = mean(test3(:,7));

%%% Case 1: pay oop with HSA/HRA pre-tax, otherwise post-tax; full sample
tax = 0.25;
hd = zeros(size(data,1),1);
hd(2:2:end) = 1;
[tp, ~, ~] = filldist_hsa(data, comcontg, tax, hsa, hd);
res2(2,1) = mean(tp(:,1) == 3);
res2(2,2) = mean(tp(:,3) == 3);
res2(2,3) = mean(tp(:,5));
res2(2,4) = mean(tp(:,6));
res2(2,5) = mean(tp(:,7));

%%% Case 2: LD post-tax, HD pre-tax up to HSA limit; full sample
[tp2, ~, ~] = filldist_hsa(data, comcontg, tax, repmat(3350, size(data,1),1), hd);   
res2(3,1) = mean(tp2(:,1) == 3);
res2(3,2) = mean(tp2(:,3) == 3);
res2(3,3) = mean(tp2(:,5));
res2(3,4) = mean(tp2(:,6));
res2(3,5) = mean(tp2(:,7));

%%% Case 3: invest HSA with 2% interest rate, doesn't allow extra HSA contribution
rint = 0.02;
rt = 0.25;
d = 0.01;
t = 30;
[tp3, ~, ~] = filldist_hsainvest1(data, comcontg, tax, rt,d,rint,t, hsa, hd);
res2(4,1) = mean(tp3(:,1) == 3);
res2(4,2) = mean(tp3(:,3) == 3);
res2(4,3) = mean(tp3(:,5));
res2(4,4) = mean(tp3(:,6));
res2(4,5) = mean(tp3(:,7));

%%% Case 4: invest HSA with 8% interest rate, doesn't allow extra HSA contribution
rint = 0.08;
[tp3, ~, ~] = filldist_hsainvest1(data, comcontg, tax, rt,d,rint,t, hsa, hd);
res2(5,1) = mean(tp3(:,1) == 3);
res2(5,2) = mean(tp3(:,3) == 3);
res2(5,3) = mean(tp3(:,5));
res2(5,4) = mean(tp3(:,6));
res2(5,5) = mean(tp3(:,7));

%%% Case 5: invest HSA with 2%, allowing extra HSA up to the limit; full sample
rint = 0.02;
[tp4, ~, ~] = filldist_hsainvest1(data, comcontg, tax, rt, d, rint, t, ...
    repmat(3350, size(data,1),1), hd);
res2(6,1) = mean(tp4(:,1) == 3);
res2(6,2) = mean(tp4(:,3) == 3);
res2(6,3) = mean(tp4(:,5));
res2(6,4) = mean(tp4(:,6));
res2(6,5) = mean(tp4(:,7));

%%% Case 5: invest HSA with 8%, allowing extra HSA up to the limit; full sample
rint = 0.08;
[tp4p, ~, ~] = filldist_hsainvest1(data, comcontg, tax, rt, d, rint, t, ...
    repmat(3350, size(data,1),1), hd);
res2(7,1) = mean(tp4p(:,1) == 3);
res2(7,2) = mean(tp4p(:,3) == 3);
res2(7,3) = mean(tp4p(:,5));
res2(7,4) = mean(tp4p(:,6));
res2(7,5) = mean(tp4p(:,7));
res2
% CARA in context
[c1, c2; G1, G2]
%% Table 6. Robustness with Information Friction of HD
res = zeros(6, 5);

%%% no HSA/HRA: sub sample
sb = hra == 0;
sb = sb(2:2:end);
sb = repelem(sb, 2);
data2 = data(sb,:);
data2(:,9) = 0;
data2(:,5) = data2(:,8) .* data2(:,15);
[testnohsa,~,~]=filldist5(data2,comcontg); %Gold
res(6,1) = mean(testnohsa(:,1) == 3);
res(6,2) = mean(testnohsa(:,3) == 3);
res(6,3) = mean(testnohsa(:,5));
res(6,4) = mean(testnohsa(:,6));
res(6,5) = mean(testnohsa(:,7));

%%% discounted HSA/HRA: sub sample
data3 = data(sb,:);
data3(:,5) = data3(:,8) .* data3(:,15) - 0.55*hsa(sb,:);
[testdishsa,~,~]=filldist5(data3,comcontg); %Gold
res(5,1) = mean(testdishsa(:,1) == 3);
res(5,2) = mean(testdishsa(:,3) == 3);
res(5,3) = mean(testdishsa(:,5));
res(5,4) = mean(testdishsa(:,6));
res(5,5) = mean(testdishsa(:,7));

%%% benchmark: full sample
res(4,1) = mean(test3(sb,1) == 3);
res(4,2) = mean(test3(sb,3) == 3);
res(4,3) = mean(test3(sb,5));
res(4,4) = mean(test3(sb,6));
res(4,5) = mean(test3(sb,7));

%%% hassle costs: low; full sample
data4 = data;
data4(2:2:end,5) = data4(2:2:end,5) + 9.72*8;
[testhclow,~,~]=filldist5(data4,comcontg); %Gold
res(2,1) = mean(testhclow(:,1) == 3);
res(2,2) = mean(testhclow(:,3) == 3);
res(2,3) = mean(testhclow(:,5));
res(2,4) = mean(testhclow(:,6));
res(2,5) = mean(testhclow(:,7));

%%% hassle costs: high; full sample
data5 = data;
data5(2:2:end,5) = data(2:2:end,5) + 138.7*8;
[testhchigh,~,~]=filldist5(data5,comcontg); %Gold
res(3,1) = mean(testhchigh(:,1) == 3);
res(3,2) = mean(testhchigh(:,3) == 3);
res(3,3) = mean(testhchigh(:,5));
res(3,4) = mean(testhchigh(:,6));
res(3,5) = mean(testhchigh(:,7));

%%% benchmark: full sample
res(1,1) = mean(test3(:,1) == 3);
res(1,2) = mean(test3(:,3) == 3);
res(1,3) = mean(test3(:,5));
res(1,4) = mean(test3(:,6));
res(1,5) = mean(test3(:,7));

[[repelem(size(data,1)/2,3)'; repelem(size(data2,1)/2,3)'],res]

% CARA in context
[c1, c2; G1, G2]
%% Figures 1: Plan Option Classification
% Case 1: Classic Trade-off
x1 = 0:100:25000;
f1 = 1070;
y1 = oop(x1,ddct(firm == f1 & pty == 4),moop(firm == f1 & pty == 4), ...
    co(firm == f1 & pty == 4),0) + nprm(firm == f1 & pty == 4);
y2 = oop(x1,ddct(firm == f1 & pty ~= 4),moop(firm == f1 & pty ~= 4), ...
    co(firm == f1 & pty ~= 4),0) + nprm(firm == f1 & pty ~= 4);
p = plot(x1 / 1000, [y1 / 1000; y2 / 1000], 'LineWidth', 3);
p(1).LineStyle = '--';
p(1).Color = [51/256,51/256,178/256];
ylim([0 6])
xlim([0 15])
a = legend('HD Plan','LD Plan','location','southeast');
a.FontSize = 14;
set(gcf, 'Color', 'w');
%export_fig figgty1.jpg -m2

% Case 2: Ambiguous
x1 = 0:100:35000;
f1 = 4867;
y1 = oop(x1,ddct(firm == f1 & pty==4),moop(firm == f1 & pty == 4), ...
    co(firm == f1 & pty == 4),0) + nprm(firm == f1 & pty == 4);
y2 = oop(x1,ddct(firm == f1 & pty ~= 4), moop(firm == f1 & pty ~= 4), ...
    co(firm == f1 & pty ~= 4),0) + nprm(firm == f1 & pty ~= 4);
p = plot(x1 / 1000,[y1 / 1000; y2 / 1000], 'LineWidth', 3);
p(1).LineStyle = '--';
p(1).Color = [51/256,51/256,178/256];
a = legend('HD Plan', 'LD Plan', 'location', 'southeast');
a.FontSize = 14;
set(gcf, 'Color', 'w');
%export_fig figgty21.jpg -m2

% Case 3: Strict Dominance
x1 = 0:100:20000;
f1 = 1882;
y1 = oop(x1,ddct(firm == f1 & pty == 4),moop(firm == f1 & pty == 4), ...
    co(firm == f1 & pty == 4),0) + nprm(firm == f1 & pty == 4);
y2 = oop(x1,ddct(firm == f1 & pty ~= 4),moop(firm == f1 & pty ~= 4), ...
    co(firm == f1 & pty ~= 4),0) + nprm(firm == f1 & pty ~= 4);
p = plot(x1/1000,[y1/1000; y2/1000],'LineWidth',3);
p(1).LineStyle = '--';
p(1).Color = [51/256,51/256,178/256];
ylim([0 4])
xlim([0 20])
a = legend('HD Plan','LD Plan','location','southeast');
a.FontSize = 14;
set(gcf, 'Color', 'w');
%export_fig figgty3.jpg -m2 

%% Figure 2: Distribution of Difference in Total Firm Contributions to Plans
contr=price.*(1-pthr)+hra+hsa;
hraindex=0*hra;
for i=1:(size(hra,1)/2)
    hraindex(2*i)=hra(2*i)>0;
    hraindex(2*i-1)=hraindex(2*i);
end
contr(hraindex==1)=[];
difcontr=contr(2:2:end)-contr(1:2:end);

edges=-1950:100:1950;
histogram(difcontr,edges,'Normalization' ,'probability')
title('Distribution of Difference in Firm Contribution to Premium and HSA')
xlabel('Total Firm Contribution Difference ($): HD-LD')
ylabel('Fraction of Firms')
set(gcf, 'Color', 'w');
set(gca, 'XTick', -2000:500:2000)
ylim([0 0.15])
%print -painters -depsc Figure2.eps
%export_fig totalcont.jpg -m2 

%% Figure 3. Average Differences in Premiums and Expected OOP Costs by Deducitble Difference
difprice=price(1:2:end)-price(2:2:end);
difnprm=nprm(1:2:end)-nprm(2:2:end);
difexc=exc(1:2:end)-exc(2:2:end);
difddct=ddct(2:2:end)-ddct(1:2:end);

m=4; %# of groups
y=[0;750;1250;1750];
group=zeros(size(difprice,1),1);
for i=1:4
    group=group+(difddct>y(i));
end

mat=zeros(3,m);
error=mat;
for i=1:m
    mat(1,i)=mean(difprice(group==i));
    mat(2,i)=mean(difnprm(group==i));
    mat(3,i)=mean(difexc(group==i));
    error(1,i)=1.96*std(difprice(group==i))/sqrt(size(difprice(group==i),1));
    error(2,i)=1.96*std(difnprm(group==i))/sqrt(size(difnprm(group==i),1));   
    error(3,i)=1.96*std(difexc(group==i))/sqrt(size(difexc(group==i),1));
end
xaxis=[500;1000;1500;2000]';
e1=errorbar(xaxis,mat(1,:),error(1,:));
xlim([400 2100])
e1.Color=[51/256,51/256,178/286];
e1.Marker='*';
e1.LineWidth=1.5;
hold on
e2=errorbar(xaxis,mat(2,:),error(2,:));
e2.LineStyle='--';
e2.Marker='o';
e2.LineWidth=1.5;
hold off
hold on
e3=errorbar(xaxis,mat(3,:),error(3,:));
e3.Color='k';
e3.LineStyle='-.';
e3.Marker='+';
e3.LineWidth=1.5;
hold off

legend('Total Premium Diff: LD-HD','Net Employee Premium Diff: LD-HD','Expected OOP Cost Diff: HD-LD','Location','Southeast')
title({'Average Difference in Premiums and Expected OOP Costs'; 'by Deductible Difference'})
xlabel('Deductible Difference ($): HD-LD')
ylabel('$')
set(gcf, 'Color', 'w');
set(gca, 'XTick', [500 1000 1500 2000])
%export_fig figreasons2.jpg -m2 

%% Appendix Figure 1: CMS spending distribution
cms=xlsread('riskdist.xlsx','cms');
s1=cms(1:75,1)/1000;
s2=cms(1:75,3)/1000;
s3=cms(1:75,5)/1000;
s4=cms(1:75,7)/1000;
p1=cms(1:75,2);
p2=cms(1:75,4);
p3=cms(1:75,6);
p4=cms(1:75,8);
p2(67)=p2(67)/10;
p2(68)=p2(68)/25;
p2(69:end)=p2(69:end)/50;
p=plot (s2,p2,'LineWidth',1.5);
p(1).Color=[0.85 0.64 0.125];
title('CMS Total Medical Spending Distribution: Gold Tier')
xlabel('Total medical spending in $1,000s')
set(gcf, 'Color', 'w');
%export_fig figcmsgold2.eps -m2 

%% Section 4 counterfactual: no more contribution
id = 1:1:size(data,1);
tp = id(hra ~= 0);
tp = [tp, tp-1];
tp = sort(tp);
sb = setdiff(id, tp);
sb = sb'; % set of indexes indicating firms offering HSA account
clear id tp

%%% bounded total contribution difference by zero: sub sample
data6 = data;
condition = (hra(2:2:end) == 0) .* (contdif(2:2:end) > 0);
tp = condition .* (price(2:2:end) - cont(1:2:end));
data6(2:2:end,5) = condition .* tp + (condition == 0) .* data6(2:2:end,5);
[testtax, ~, ~] = filldist5(data6, comcontg);
(mean(test3(sb, 1) == 3) - mean(testtax(sb,1) == 3)) ...
    / mean(test3(sb, 1) == 3)
(mean(test3(sb, 3) == 3) - mean(testtax(sb,3) == 3)) ...
    / mean(test3(sb, 3) == 3)



