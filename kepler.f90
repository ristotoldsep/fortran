real work(39),y(6),m_maa,dy(6)
integer iwork(5)
common gm
external func
open(10,file="kepler.dat")

relerr=1.e-7
abserr=1.e-7
m_maa=5.9722e+24
r_maa=6378.
G=6.67408e-11
gm=G*m_maa*1.e-9
t=0.
y(1)=6578.
y(2)=0.
y(3)=0.
y(4)=0.
!y(4)=9.78337
y(5)=8.8337
y(6)=0.
nt=20000
dt=1.

t=0.
iflag=1
neqn=6
do i=1,nt
tout=t+dt
call rkf45(func,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
if(iflag.ne.2)then
iflag=2
endif
vv=sqrt(y(3)**2+y(4)**2)
rr=sqrt(y(1)**2+y(2)**2)
call func(t,y,dy)
write(10,'(100e20.10)') tout,y,rr,vv,dy(4:6)
enddo

stop
end

subroutine func(t,y,dy)
common gm
real y(6),dy(6)
dy(1)=y(4)
dy(2)=y(5)
dy(3)=y(6)
dy(4)=-gm*y(1)/sqrt(y(1)**2+y(2)**2+y(3)**2)**3
dy(5)=-gm*y(2)/sqrt(y(1)**2+y(2)**2+y(3)**2)**3
dy(6)=-gm*y(3)/sqrt(y(1)**2+y(2)**2+y(3)**2)**3
return
end