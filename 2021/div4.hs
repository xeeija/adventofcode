-- main=interact x;x n|mod(read n)4<1="Ok"|0<3="AAAAAAAAAA!!!"

main=interact x;x n=show$sum[2,4..read n]

mai2=interact$show.(\x->(x+2)*x/4).read

mai3=interact$show.(\n->sum[2,4..n]).read


-- Perl: print hex<>