real pi, g_rask, m_keha, c_keha, r_keha, s_keha, myy, r_gaas, t_gaas, v_keha
real p_gaas, roo_gaas_0, param1, param2, dt, x, v, a, roo_gaas, e_kin, e_pot, e_tot
integer nt, i
open(10,file="pall_1d.dot")
pi=3.14159265
g_rask=9.814
m_keha=15.
c_keha=0.47
r_keha=2.
s_keha=pi*(r_keha**2)
myy=29.
r_gaas=8341.
t_gaas=288.
v_keha=(4*pi*(r_keha**3))/3
p_gaas=101700
roo_gaas_0=(p_gaas*myy)/(r_gaas*t_gaas)
param1=(0.5*c_keha*s_keha)/m_keha
param2=(myy*g_rask)/(r_gaas*t_gaas)
dt=0.1 !(aja)sammude pikkus
nt=40000 !sammude arv
x=0.
v=0.
do i=1,nt
    roo_gaas = roo_gaas_0 * exp((-1*param2) * x)
    a = (((roo_gaas * v_keha) / m_keha) - 1) * g_rask - param1 * roo_gaas * abs(v) * v
    x = x + v * dt + 0.5 * a * (dt ** 2)
    v = v + a * dt
    e_kin = 0.5 * m_keha * (v ** 2)
    e_pot = m_keha * g_rask * x
    e_tot = e_kin + e_pot
    write(10,*) i*dt,x, v, a, e_kin, e_pot, e_tot
end do
stop
end