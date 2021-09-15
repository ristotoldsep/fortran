real a(10), max, min
integer i
open (10, file="test10.in")
read (10, *) a
max = -1.E+38
min = +1.E+38
do i = 1, 10
    if(a(i) > max)then
        max = a(i)
    endif
    if(a(i) < min)then
        min = a(i)
    endif
enddo
print *, min, max 
stop 
end

