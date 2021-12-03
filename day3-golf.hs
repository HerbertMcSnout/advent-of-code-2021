a i=zipWith(\c(x,y)->if c=='0'then(x+i,y)else(x,y+i))
d c m[(p,s)]=reverse p++s
d c((x,y):m)l=uncurry(d c)$foldr(\(p,(h:t))(m,l)->if(h==c)==(x>y)then(m,(h:p,t):l)else(a(-1)t m,l))(m,[])l
main=lines<$>getContents>>= \l->putStrLn(show(product[foldl(\i c->2*i+if c=='0'then 0 else 1)0(d c(foldr(a 1)(replicate(length(head l))(0,0))l)[("",j)|j<-l])|c<-['0','1']]))
