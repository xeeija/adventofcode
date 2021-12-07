import System.Environment
s n=sum.(!!n).iterate t.h
t[a,b,c,d,e,f,g,h,i]=[b,c,d,e,f,g,h+a,i,a]
h x=map(#x)[0..8]
(#)n=toInteger.length.filter(==n)
main|r<-read=do;a<-getArgs;f<-readFile$a!!0;print$s(r$a!!1)(map r$words f)