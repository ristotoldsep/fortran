! FUNKTSIOONI KASUTAMINE!!!!
real a,b,c 
print '("Enter a="$)'
read *, a 
print '("Enter b="$)'
read *, b 

call summ(a,b,c) ! Salvestab tulemuse muutujasse c!
print *, a,"+",b,"=",c 
stop
end

! FUNKTSIOON, kui tahta võib selle funktsiooni tõmmata ka teisest failist, nt panna nimeks test15_1.f90, ss consoolist runimime => gfortran test15.f90 test15_1.f90 -o test15
subroutine summ(a,b,c)
real a,b,c 
c = a + b 
return 
end

