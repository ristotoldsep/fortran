real massiiv(10), sum ! FORTRANIS HAKKAB LOENDAMINE ühest mitte nullist
integer i 

open(10, file="test6.in")
read(10, *) massiiv

sum = 0 ! initsialiseerime summa

do i = 1,10 ! i muutub ühest kümneni (tsükkel)
    sum = sum + massiiv(i)
end do

print *, 'Failist loetud arvude aritmeetiline keskmine = ', sum/10

open(11, file="test6.out")
write(11, *) 'Failist loetud arvude aritmeetiline keskmine = ', sum/10
write(11, *) massiiv ! Kirjutas faili test6.out

print *,'Masiiv: ', massiiv

stop
end
