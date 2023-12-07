import Control.Monad;import Data.List;m=map
main=interact$show.liftM2(,)s t.m(tail.words).lines
s=product.m r.transpose.m(m read);t=r.m(read.foldr1(++))
r(x:y:_)=length[n*(x-n)|n<-[1..x],n*(x-n)>y]