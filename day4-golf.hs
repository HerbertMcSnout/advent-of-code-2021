import Data.List.Split
w i b=[[if j==i then-1else j|j<-r]|r<-b]
sc=sum.map(foldr(\s x->if s== -1then x else x+s)0)
g b=any(\i->all(== -1)(b!!i)||all((== -1).(!!i))b)[0..4]
r d=map(\s->read s::Int).filter(not.null).splitWhen(==d)
p[b](i:j)=let c=w i b in if g c then i*(sc c)else p[c]j
p t(i:j)=p[b|b<-map(w i)t,not(g b)]j
main=r ','<$>getLine>>= \ns->map(map(r ' '))<$>splitWhen null<$>tail<$>lines<$>getContents>>= \b->putStrLn(show(p b ns))
