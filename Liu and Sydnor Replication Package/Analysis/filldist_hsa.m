function [test,prmoop,exc]=filldist_hsa(data,comcont,tax,limit, hd)

% this function will calculate filldist assuming LD pays oop post-tax, HD
% pays oop post-tax, and HSA/HRA pre-tax

nprm = data(:,5); %The imported nprm=premium*worker's ctrb-hsa ctrb
ddct = data(:,6); %general ddct
moop = data(:,7);
price = data(:,8); %total premium
hra = data(:,9); %HRA contribution
co = 1-data(:,13); %Cost sharing: percentage paid by the consumer
pthr = data(:,15); %passthrough rate
prm = price.*pthr;
hsa = round (prm - nprm);
ddct3b=data(:,16); %office visits subject to general ddct? 1=yes, 2=no.
csov=[data(:,17),data(:,18)/100,data(:,19),data(:,20)/100]; %ov cost sharing rule
ddct3c=data(:,21); %drug subject to general ddct?
cs27=data(:,22); %durg has separate ddct?
cs27a=data(:,23); %separate drug ddct. 0 means no separate drug ddct
csdrug=[data(:,25),data(:,26)/100,data(:,28),data(:,29)/100,data(:,31),data(:,32)/100,data(:,34),data(:,35)/100]; %drug cost sharing rule

n=size(data,1);

%% out-of-pocket
m=size(comcont,1);
prmoop=zeros(n,m); % prm+oop for each plan in each state
prmoop(:,1)=nprm;
oopd=zeros(n,m);
oopov=zeros(n,m);
for i=1:n
    for j=2:m
        % get oopov
        t1=comcont(j,6)*csov(i,2);
        t2=min(comcont(j,6),comcont(j,7)*csov(i,1));
        t3=comcont(j,9)*csov(i,4);
        t4=min(comcont(j,9),comcont(j,10)*csov(i,3));
        oopov(i,j)=t1+t2+t3+t4;
        % get oopd
        if (comcont(j,12)+comcont(j,15)+comcont(j,18)+comcont(j,21))<=cs27a(i)
            oopd(i,j)=comcont(j,12)+comcont(j,15)+comcont(j,18)+comcont(j,21);
        else
            d1=cs27a(i)*comcont(j,12)/(comcont(j,12)+comcont(j,15)+comcont(j,18)+comcont(j,21));
            d2=cs27a(i)*comcont(j,15)/(comcont(j,12)+comcont(j,15)+comcont(j,18)+comcont(j,21));
            d3=cs27a(i)*comcont(j,18)/(comcont(j,12)+comcont(j,15)+comcont(j,18)+comcont(j,21));
            d4=cs27a(i)*comcont(j,21)/(comcont(j,12)+comcont(j,15)+comcont(j,18)+comcont(j,21));
            s1=comcont(j,12)-d1;
            f1=comcont(j,13)*s1/comcont(j,12);
            s2=comcont(j,15)-d2;
            f2=comcont(j,16)*s2/comcont(j,15);
            s3=comcont(j,18)-d3;
            f3=comcont(j,19)*s3/comcont(j,18);
            s4=comcont(j,21)-d4;
            f4=comcont(j,22)*s4/comcont(j,21);
            if isnan(f1)
                f1=0;
            end
            if isnan(f2)
                f2=0;
            end
            if isnan(f3)
                f3=0;
            end
            if isnan(f4)
                f4=0;
            end
            d0=[s1,s2,s3,s4]*csdrug(i,2:2:end)';
            d1=min(comcont(j,12),f1*csdrug(i,1));
            d2=min(comcont(j,15),f2*csdrug(i,3));
            d3=min(comcont(j,18),f3*csdrug(i,5));
            d4=min(comcont(j,21),f4*csdrug(i,7));
            oopd(i,j)=cs27a(i)+d0+d1+d2+d3+d4;
        end
        % doing calculation
        if ddct3b(i) == 1 && ddct3c(i) == 1 && cs27(i) == 2 %all services subject to general ddct
            tp = oop(comcont(j,3),ddct(i),moop(i),co(i),0);
        else
            if ddct3b(i) == 2 && ddct3c(i) == 1 && cs27(i) == 2 %only ov not subject to ddct
                opg = oop2((1-comcont(j,5)-comcont(j,8))*comcont(j,3),ddct(i),co(i));
                tp = min(moop(i),oopov(i,j)+opg);
            else 

                if ddct3b(i) == 1 %ov subject to ddct, drug not
                    opg = oop2((1-comcont(j,4))*comcont(j,3),ddct(i),co(i));
                    tp = min(moop(i),oopd(i,j)+opg);
                else %neither ov nor drug subject to ddct
                    opg=oop2((1-comcont(j,4)-comcont(j,5)-comcont(j,8))*comcont(j,3),ddct(i),co(i));
                    tp = min(moop(i),oopov(i,j)+oopd(i,j)+opg);
                end
            end
        end
        prmoop(i,j)= oph(tp, hsa(i), hra(i), tax, prm(i), hd(i), limit(i));
    end
end

%% Statewise dominance
wc = oph(moop, hsa, hra, tax, prm, hd, limit);
gty=zeros(n,1);
for i=1:n/2
    if prmoop(2*i-1,1)>prmoop(2*i,1) && wc(2*i-1)>wc(2*i)
        if sum(prmoop(2*i-1,:)>=prmoop(2*i,:))==84
            gty(2*i)=3;
        else
            gty(2*i)=2;
        end
    else
        if prmoop(2*i-1,1)>prmoop(2*i,1) && wc(2*i-1)<wc(2*i)
            gty (2*i)=1;
        else
            if prmoop(2*i-1,1)<prmoop(2*i,1) && wc(2*i-1)>wc(2*i)
                gty(2*i)=-1;
            else
                if sum(prmoop(2*i-1,:)>=prmoop(2*i,:))==0
                    gty(2*i)=-3;
                else
                    gty(2*i)=-2;
                end
            end
        end
    end
    gty(2*i-1)=gty(2*i);
end

%% rgty classification
p=comcont(:,2); %probablity of each state
m=100; %number of r simulation
a=0.01; %the upper bound of r
% result variable
eu=zeros(n,m+1); %expected utility for each plan
deu=zeros(n,m+1); % diff of expected utility
cs=zeros(n,m+1); 
rgty=ones(n,1)*(-9); % rgraph type
exe=zeros(n,1); %expected expense for each plan
exc=zeros(n,1); %expected coverage for each plan

dexe=zeros(n,1); %difference of expected expense: other plan minus high deductible plan
% some working variable
s2=ones(n/2,size(deu,2)-1)*(-9);
c2=ones(n/2,1)*(-9);
% self defined function
euf=@(r,v,pp)-pp'*exp(r*v); %expected utility. r, v and pp are column vector
%eupf=@(r,v,pp) -(v.*pp)'*exp(r*v); % first order derivative of eu
%euppf=@(r,v,pp) -((v.^2).*pp)'*exp(r*v); %second order derivative of eu

% calculate expected utility given the r matrix (eu)
% calculate expected expense (exe)
for i=1:n
    exe(i)=prmoop(i,:)*p;
    exc(i)=comcont(:,3)'*p-(prmoop(i,:)-nprm(i))*p;
    for r=0:m
        eu(i,r+1)=euf(a*r/m,prmoop(i,:)',p);
    end
end

% calculate deu, dexe, cs and rgty (classification based on CARA utility fcn)
for j=1:n/2
    for r=0:m
        deu(2*j-1,r+1)=-eu(2*j-1,r+1)+eu(2*j,r+1);
        dexe(2*j-1)=exe(2*j-1)-exe(2*j);
        if r==0
            cs(2*j-1,r+1)=dexe(2*j-1);
        else
            cs(2*j-1,r+1)=-m/(r*a)*log(eu(2*j,r+1)/eu(2*j-1,r+1));
        end
    end
    deu(2*j,:)=deu(2*j-1,:);
    dexe(2*j)=dexe(2*j-1);
    cs(2*j,:)=cs(2*j-1,:);
    w22=[deu(2*j-1,3:end) deu(2*j-1,end)];
    s2(j,:)=((w22>=0)==(deu(2*j-1,2:end)>=0)); 
    c2(j)=sum(s2(j,:)==0); %c is number of crossings of difference of EU with x axis
    % rgty==1: not monotone
    if sum(deu(2*j-1,:)<0)==0 %rgty=3: the eu dif is always positive
        rgty(2*j-1)=3;
    else if sum(deu(2*j-1,:)>0)==0 %rgty=-3: the eu dif is always negative
            rgty(2*j-1)=-3;
        else if c2(j,1)==2 %rgty=2: crosses x axis twice (exept for the origin)
                rgty(2*j-1)=2;
            else %rgty=1: crosses x axis once (exept for the origin)
                rgty(2*j-1)=1;
            end
        end
    end
    if rgty(2*j-1)==3 && dexe(2*j-1)<0
        rgty(2*j-1)=-1;
    end
    if rgty(2*j-1)==-3 && dexe(2*j-1)>0
        rgty(2*j-1)=1;
    end 
    rgty(2*j)=rgty(2*j-1);
end
csr0=cs(:,1);
csr05=cs(:,100*0.0005/0.01);
csr2=cs(:,100*0.002/0.01);


%% Stochastic Dominance
F=ones(size(p,1),1); % Original cummulative dist
F(1)=p(1);
F12=zeros(size(p,1),1); % F_1(x2)
F21=zeros(size(p,1),1); % F_2(x1)
n2=size(prmoop,2);
fgty=ones(n,1)*(-9); 
sgty=ones(n,1)*(-9); 
dg=zeros(n/2,2*n2-1);

F0=F;
for i=2:size(p,1)
    F0(i)=F0(i-1)+p(i);
end
for i=2:size(p,1)
    F(i)=F0(i-1)+p(i)*(comcont(i,3)-comcont(i-1,1))/(comcont(i,1)-comcont(i-1,1));
end

F(end)=1;

for i=1:n/2 %loop over firms
    % Calculate F1(x1)
    F11=F;
    for k=1:(n2-1)
        if prmoop(2*i-1,k)==prmoop(2*i-1,end)
            F11(k:end)=1;
            break
        end
    end
    % Calculate F2(x2)
    F22=F;
    for k=1:(n2-1)
        if prmoop(2*i,k)==prmoop(2*i,end)
            F22(k:end)=1;
            break
        end
    end
    for j=1:n2 %loop over expense of each state, to find F11, F22, F12 and F21
        % Calculate F1(x2)
        for k=1:n2
            if prmoop(2*i,j)<prmoop(2*i-1,k)
                break
            end
        end
        if k==1
                F12(j)=0;
        else if k==n2
                F12(j)=1;
            else
                F12(j)=F11(k-1);
            end
        end
        % Calculate F2(x1)
        for k=1:n2
            if prmoop(2*i-1,j)<prmoop(2*i,k)
                break
            end
        end
        if k==1
                F21(j)=0;
        else if k==n2
                F21(j)=1;
            else
                F21(j)=F22(k-1);
            end
        end      
    end
    % First stochastic dominance classification
    if sum(F12>=F22)==n2 && sum(F11>=F21)==n2
        fgty(2*i-1)=-3;
    else if sum(F22>=F12)==n2 && sum(F21>=F11)==n2
            fgty(2*i-1)=3;
        else
            fgty(2*i-1)=1;
        end
    end
    fgty(2*i)=fgty(2*i-1);
    % Second stochastic dominance classification
    %classification based on definition
    x=[-prmoop(2*i-1,:) -prmoop(2*i,:)]; % Acombination of two plan x values
    F1=[1-F11;1-F12]; %dist of ldp
    F2=[1-F21;1-F22]; %dist of hdp
    B=[x' F1 F2];
    B1=sort(B);
    x=B1(:,1)';
    F1=B1(:,2);
    F2=B1(:,3);
    dg(i,1)=(F1(2)-F2(2)+F1(1)-F2(1))*(x(2)-x(1))/2; %loop to get d2
    for j=2:(2*n2-1)
        dg(i,j)=dg(i,j-1)+(F1(j)-F2(j)+F1(j+1)-F2(j+1))*(x(j+1)-x(j))/2;
    end
    if sum(dg(i,:)<=0)==(2*n2-1)
        sgty(2*i-1)=-3;
    else if sum(dg(i,:)>=0)==(2*n2-1)
            sgty(2*i-1)=3;
        else sgty(2*i-1)=1;
        end
    end
    sgty(2*i)=sgty(2*i-1);
end
test=[gty, fgty, sgty, rgty, csr0, csr05, csr2];


