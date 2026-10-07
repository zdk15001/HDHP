function [exsaving]=fillmeps(prob,prmoop,comcont)

%% Risk-adjusted savings from HD plan
p=prob; %probablity of each state
m=100; %number of r simulation
a=0.01; %the upper bound of r
n=size(prmoop,1);
% result variable
eu=zeros(n,m+1); %expected utility for each plan
deu=zeros(n,m+1); % diff of expected utility
cs=zeros(n,m+1); 
exe=zeros(n,1); %expected expense for each plan
dexe=zeros(n,1); %difference of expected expense: other plan minus high deductible plan
% self defined function
euf=@(r,v,pp)-pp'*exp(r*v); %expected utility. r, v and pp are column vector
%eupf=@(r,v,pp) -(v.*pp)'*exp(r*v); % first order derivative of eu
%euppf=@(r,v,pp) -((v.^2).*pp)'*exp(r*v); %second order derivative of eu

% calculate expected utility given the r matrix (eu)
% calculate expected expense (exe)
for i=1:n
    exe(i)=prmoop(i,:)*p;
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
        else fgty(2*i-1)=1;
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
sosd=sum(sgty==3)/2;
exsaving=zeros(4,1);
exsaving(2)=mean(csr0);
exsaving(3)=mean(csr05);
exsaving(4)=mean(csr2);
exsaving(1)=sosd;

