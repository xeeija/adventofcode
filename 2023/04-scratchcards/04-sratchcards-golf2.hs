import Data.List.Split
main=interact$show.sum.map(floor.(2^^).(+(-1)).length.w.p).lines
p=map(map read.words).s" | ".last.s": "::String->[[Int]]
w(x:w:_)=filter(`elem`w)x;s=splitOn