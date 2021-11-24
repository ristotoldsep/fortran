real y(6),work(39),mu_atm,m_keha,m_maa,dy(6)
integer iwork(5)
common gm,r_maa,rho_0,p1,p2
external keplerf
open(20,file="fall.dat")
pi=3.1415926536
c_keha=0.47
r_keha=2.
m_keha=2800.

p_atm=101000.
mu_atm=29.
r_gaas=8314.
t_atm=300.

G=6.67408e-11
m_maa=5.9722e+24
gm=G*m_maa*1.e-9
r_maa=6378.

s_keha=pi*r_keha**2
rho_0=p_atm*mu_atm/(r_gaas*t_atm)
p2=s_keha*c_keha*500./m_keha
p1=mu_atm*9814./(r_gaas*t_atm)

neqn=6
dt=1.
nt=20000
abserr=1.e-8
relerr=1.e-8
v0=7.7884079
alpha=0.7
      
y(1)=6578.0
y(2)=0.
y(3)=0.
y(4)=-v0*sin(pi*alpha/180.)
y(5)=v0*cos(pi*alpha/180.)
y(6)=0.
t=0
iflag=1
    
do i=1,nt
tout=t+dt
call rkf45(keplerf,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)

if(iflag /= 2) then
iflag=2
endif

r=sqrt(y(1)**2+y(2)**2)
h=r-r_maa

if (h.le.5.)then
r_keha=20.
s_keha=pi*r_keha**2
p2=c_keha*s_keha*500./m_keha
endif

if(h.le.0.)then
stop
endif
call keplerf(t,y,dy)

v=sqrt(y(4)**2+y(5)**2+y(6)**2)
a=sqrt(dy(4)**2+dy(5)**2+dy(6)**2)

write(20, '(10f15.5)') tout,y,h,v*1000,a*1000

enddo

stop
end

subroutine keplerf(t,y,dy)
real y(6),dy(6)
common gm,r_maa,rho_0,p1,p2
rabs=sqrt(y(1)**2+y(2)**2+y(3)**2)
vabs=sqrt(y(4)**2+y(5)**2+y(6)**2)
h=rabs-r_maa
rho=rho_0*exp(-p1*h)
dy(1)=y(4)
dy(2)=y(5)
dy(3)=y(6)
dy(4)=-y(1)*gm/rabs**3-p2*rho*vabs*y(4)
dy(5)=-y(2)*gm/rabs**3-p2*rho*vabs*y(5)
dy(6)=-y(3)*gm/rabs**3-p2*rho*vabs*y(6)
return
end
 