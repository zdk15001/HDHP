function y=oop2(x,ddct,co)
    y=(x<=ddct)*x+(x>ddct)*(ddct+(x-ddct)*co);
end