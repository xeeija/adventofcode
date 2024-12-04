import Control.Monad;import Data.List;l=length;m a b@(x,y,z)|o[ä x,w y,ö z]=a#b|
 o[ö x,w y,ä z]=a#b|o[ü x,w y,ü z]=a#b|o[ß x,w y,ß z]=a#b;m a(_:x,_:y,_:z)|l x<2
 =a|1>0=m a(x,y,z);main=interact$show.liftM2(,)(e.n(e.n(c 0)).p)(e.n(m 0).(zip3
 <*>r<*>drop 2).k);d=u.zipWith(\i x->replicate i ' '++x)[0..];(#)a(x,y,z)=m(a+1)
 (r x,r y,r z);r=drop 1;p=liftM4(\a b c d->[a,b,c,d])id v d(d.v).k;c a s|"XMAS"%
 s||"SAMX"%s=c(a+1)$r s|l s<4=a|1>0=c a$r s;(%)=isPrefixOf;o=all(>0);v=n reverse
 .u;ä('M':_:'M':_)=1;ä _=0;ö('S':_:'S':_)=1;ö _=0;ü('M':_:'S':_)=1;ü _=0
ß('S':_:'M':_)=1;ß _=0;w(_:'A':_)=1;w _=0;u=transpose;n=map;e=sum;k=lines


-- fits on an IBM 5081 Punch Cards (10 rows, 80 columns)


-- c a s@('X':'M':'A':'S':_)=c (a+1)$r s
-- c a s@('S':'A':'M':'X':_)=c (a+1)$r s;
-- c a s|l s<4=a|1>0=c a$r s

-- import Control.Monad;import Data.List;u=transpose;n=map;e=sum;k=lines;l=length
-- main=interact$show.liftM2(,)(e.n(e.n(c 0)).p)(e.n(m 0).(zip3<*>o<*>drop 2).k)
-- d=u.zipWith(\i x->replicate i ' '++x)[0..];f a(_:x,_:b,_:c)=m(a+1)(x,b,c);o=tail
-- p=liftM4(\a b c d->[a,b,c,d])id v d(d.v).k;c a s@('X':'M':'A':'S':_)=c(a+1)$o s
-- c a s@('S':'A':'M':'X':_)=c(a+1)$o s;c a s|l s<4=a|1>0=c a$o s;v=n reverse.u
-- m a x@('M':_:'M':_,_:'A':_,'S':_:'S':_)=f a x
-- m a x@('S':_:'S':_,_:'A':_,'M':_:'M':_)=f a x
-- m a x@('M':_:'S':_,_:'A':_,'M':_:'S':_)=f a x
-- m a x@('S':_:'M':_,_:'A':_,'S':_:'M':_)=f a x
-- m a(_:x,_:y,_:z)|l x<2=a|1>0=m a(x,y,z)



-- import Control.Monad;import Data.List

-- main=interact$show.liftM2(,)s t

-- s=e.n(e.n(c 0)).p

-- c a s@('X':'M':'A':'S':_)=c(a+1)$o s
-- c a s@('S':'A':'M':'X':_)=c(a+1)$o s
-- c a s|l s<4=a|1>0=c a$o s

-- p=liftM4(\a b c d->[a,b,c,d])id v d(d.v).k

-- v=n reverse.u

-- d=u.zipWith(\i x->replicate i ' '++x)[0..]

-- t=e.n(m 0).q
-- m a x@('M':_:'M':_,_:'A':_,'S':_:'S':_)=f a x
-- m a x@('S':_:'S':_,_:'A':_,'M':_:'M':_)=f a x
-- m a x@('M':_:'S':_,_:'A':_,'M':_:'S':_)=f a x
-- m a x@('S':_:'M':_,_:'A':_,'S':_:'M':_)=f a x
-- m a(_:x,_:y,_:z)|l x<2=a|1>0=m a(x,y,z)

-- f a(_:x,_:b,_:c)=m(a+1)(x,b,c)

-- q=(zip3<*>o<*>drop 2).k
-- k=lines;l=length;u=transpose;n=map;e=sum;o=tail

