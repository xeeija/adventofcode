import Data.List;import Data.List.Split
main=interact$show.sum.map(s.length.w.p).lines
p=map(map read.words).splitOn" | ".last.splitOn": "::String->[[Int]]
s x=if x>0 then 2^(x-1)else 0
w(x:w:_)=filter(`elem` w)x