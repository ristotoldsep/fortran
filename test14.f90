real ns, x, y
integer n  
n = 10000 
ns = 0
do i = 1, n 
    x = rand() 
    y = rand() 
    if(y <= x*x)then
        ns = ns + 1
    endif
enddo 
print *, ns/n 
stop
end 
