real a,b,abserr,relerr,result,errest,flag
integer nofun 
external fun ! Näitab, et pole tavaline muutuja, vaid välise alamprogrammi nimi

a = 0.
b = 1.
abserr = 1.E-7
relerr = 1.E-7 
call quanc8(fun, a, b, abserr, relerr, result, errest, nofun, flag)

print *, result, errest 
stop 
end

! function = alamfunktsioon, subroutine = alamprogrammm!
function fun(x)
    real x, fun
    fun = sqrt(1.-(x*x))*4 
    return
end 
