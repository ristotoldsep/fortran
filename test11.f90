! RUUTVÕRRANDI LAHENDAMINE FORTRANIGA
real a, b, c, diskr, x1, x2
print '("Enter a,b,c="$)'
read *, a,b,c 
! Arvutame ruutvõrrandit, diskriminant = b^2-4*a*c
diskr = b*b-4.*a*c 

if (diskr > 0.)then 
    x1 = (-b + sqrt(diskr))/(2.*a) 
    x2 = (-b - sqrt(diskr))/(2.*a) 
    print *, x1, x2
    stop
endif

if (diskr == 0)then
    x1 = -b/(2*a)
    print *, "x1=x2= ", x1
    stop
endif

x1 = -b/(2*a)
x2 = sqrt(-diskr)/(2*a)
print *, x1, "+/-i", x2
stop
end
