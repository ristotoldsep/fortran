real a(3), b(3), nurk
open(10, file="test16.in")
read(10, *) a,b 
call alpha(a,b, nurk)
print *, "Nurk on: ", nurk
stop
end



subroutine alpha(a,b,nurk)
    real a(3), b(3), nurk, pi
    pi = 3.1415926
    nurk = acos(dot_product(a,b)/sqrt(dot_product(a,a)*dot_product(b,b)))*180./pi
    return
end
