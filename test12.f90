! SKALAARKORRUTISE ARVUTAMINE FORTRANIGA
real a(3), b(3), pi, sum, sum1, sum2, alpha
integer i 
open(10, file="test12.in")
read(10, *) a, b ! Loeb ühelt realt
! read(10, *) b ! Loeb teiselt realt, samas voib kirjutada ka read(10, *) a, b

sum = 0.
sum1 = 0.
sum2 = 0.
pi = 3.141592
do i=1, 3
    sum = sum + a(i)**2
    sum1 = sum1 + b(i)**2
    sum2 = sum2 + a(i) * b(i)
enddo

alpha = 180. * acos(sum2/sqrt(sum*sum1))/pi

print *, alpha
stop 
end
