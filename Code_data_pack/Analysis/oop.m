function y=oop(x,ddct,moop,co,hra)
    oopnohra=x.*(x<=ddct)+(x>ddct).*min((ddct+(x-ddct)*co),moop);
    y=max(oopnohra-hra,0);
end