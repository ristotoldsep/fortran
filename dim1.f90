real y(2),work(15),m_keha,mu
integer i,nt,iwork(5)
common g_rask,rho_0,p1,p2,v_keha,m_keha
external func
open(10,file="out.dat")
pi=3.1415926536
g_rask=9.814
r_keha=2.
m_keha=15.
c_keha=0.47
s_keha=pi*r_keha**2
v_keha=4.*pi*r_keha**3/3.
mu=29.
t_gaas=282.
r_gaas=8314.
p_gaas=100400.
rho_0=p_gaas*mu/(r_gaas*t_gaas)
p1=mu*g_rask/(r_gaas*t_gaas)
p2=0.5*c_keha*s_keha/m_keha

nt=40000
dt=0.1
t=0.
tout=0.
iflag=1
relerr=1.e-6
abserr=1.e-6
neqn=2
y(1)=0.
y(2)=0.

do i=1,nt
tout=t+dt
call rkf45(func,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
if(iflag.ne.2) then
iflag=2
endif
write(10,*) tout,y

enddo
stop
end

subroutine func(t,y,yp)
real y(2),yp(2),m_keha
common g_rask,rho_0,p1,p2,v_keha,m_keha
rho=rho_0*exp(-p1*y(1))
yp(1)=y(2)
yp(2)=-g_rask+rho*g_rask*v_keha/m_keha-p2*rho*abs(y(2))*y(2)
return
end




