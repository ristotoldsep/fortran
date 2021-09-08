integer n, faktoriaal
print '("Enter n="$)'
read *, n

if(n<0)then
    print *, "Error: n<0"
    stop
endif

if(n < 2)then
    print *, n,'!=1'
    stop
endif

faktoriaal = 1
do i = 2,n 
    faktoriaal = faktoriaal * i 
end do 
print *, n,'!=',faktoriaal

stop
end
