real y(4),work(27),mu_atm,m_keha
integer i, nt, iwork(5)
common g_rask,ro_atm_0,ro_keha,p1,p2
external f
open(10,file="out2.dat")
pi=3.1415926536
g_rask=9.814
r_keha=0.1
s_keha=pi*r_keha**2
c_keha=0.47
v_keha=4.*pi*r_keha**3/3
m_keha=0.2
ro_keha=m_keha/v_keha

p_atm=100000.
t_atm=2284.
mu_atm=29.
r_gaas=8314.

ro_atm_0=p_atm*mu_atm/(r_gaas*t_atm)
p1=g_rask*mu_atm/(r_gaas*t_atm)
p2=0.5*c_keha*s_keha/m_keha

neqn=4 !    Võrrandite arv
nt=3000
dt=0.001
t=0.
relerr=1.e-6
abserr=1.e-6

y(1)=0.
y(2)=0.
y(3)=10.
y(4)=10.
iflag=1

do it=1,nt
tout=t+dt
iflag=2
call rkf45(f,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
write (10,*)tout,y
enddo

stop
end

subroutine f(t,y,yp)
real y(4),yp(4)
common g_rask,ro_atm_0,ro_keha,p1,p2
yp(1)=y(3)
yp(2)=y(4)
roh=ro_atm_0*exp(-p1*y(2))
v=sqrt(y(3)**2+y(4)**2)
yp(3)=-p2*roh*v*y(3)
yp(4)=g_rask*(roh/ro_keha-1.)-p2*roh*v*y(4)
return
end