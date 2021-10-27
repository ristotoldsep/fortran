! 27.10.2021 - Palli kahemõõtmeline liikumine

    real y(4),work(27),mu_atm,m_keha,a(4)
integer iwork(5)
common /aaa/g_rask,ro_atm_0,m_keha,v_keha,p1,p2
common /bbb/ ar(2),at(2),aa(2)
external f
open(10,file="out3.dat")
open(11,file="kpt.dat")

pi=3.1415926536
g_rask=9.814
r_keha=0.05
s_keha=pi*r_keha**2
c_keha=0.47
v_keha=4.*pi*r_keha**3/3
m_keha=2.
ro_keha=m_keha/v_keha
p_atm=100000.
t_atm=293.
mu_atm=29.
r_gaas=8314.
ro_atm_0=p_atm*mu_atm/(r_gaas*t_atm)
p1=g_rask*mu_atm/(r_gaas*t_atm)
p2=0.5*c_keha*s_keha/m_keha

neqn=4
nt=4000
dt=0.01
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

call rkf45(f,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
if (iflag /= 2)then
iflag=2
endif
if (y(2) < 0.) then
    print *,"Error: h<0"
stop
endif

call f(t,y,a)
ekin=0.5*m_keha*(y(3)*y(3)+y(4)*y(4))
epot=m_keha*g_rask*y(2)
etot=ekin+epot
write (10,*)tout,y,a(3:4)
write (11,*)tout,ekin,epot,etot

enddo

stop
end

subroutine f(t,y,yp)
real y(4),yp(4),m_keha
common /aaa/ g_rask,ro_atm_0,m_keha,v_keha,p1,p2
common /bbb/ ar(2),at(2),aa(2)
yp(1)=y(3)
yp(2)=y(4)
roh=ro_atm_0*exp(-p1*y(2))
vabs=sqrt((y(3))**2+y(4)**2)
yp(3)=-p2*roh*vabs*(y(3))
yp(4)=-g_rask+roh*g_rask*v_keha/m_keha-p2*roh*vabs*y(4)
ar(1)=0.
ar(2)=-g_rask
at(1)=-p2*roh*vabs*y(3)
at(2)=-p2*roh*vabs*y(4)
aa(1)=0.
aa(2)=roh*g_rask*v_keha/m_keha
return
end