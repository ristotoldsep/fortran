real m, y(6), work(39), B(3), E(3), yp(6)
integer iwork (5)
external func
common qm, B, E
open(10, file='lorenz.dat')
q=1.e-9
m=1.e-9 
qm=q/m

dt=0.01
nt=100000
neqn=6
t=0
tout=0.
iflag=1

relerr=1.e-6
abserr=1.e-6

y=(/0.,0.,0.,0.,10.,0./)
B=(/0.,0.,0.1/)
E=(/0.,0.,0./)

do i=1, nt
    tout=t+dt
    call rkf45(func,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)

    if(iflag /= 2)then
        iflag=2
    end if
    call func(t,y,yp)
    write(10,*) tout, y, yp(4:6)
end do
stop
end

subroutine func(t,y,yp)
    real y(6),yp(6),B(3),E(3)
    common qm, E, B
    yp(1)=y(4)
    yp(2)=y(5)
    yp(3)=y(6)
    yp(4)=qm*(y(5)*B(3)-y(6)*B(2)+E(1))
    yp(5)=qm*(y(6)*B(1)-y(4)*B(3)+E(2))
    yp(6)=qm*(y(4)*B(2)-y(5)*B(1)+E(3))
    return 
    end