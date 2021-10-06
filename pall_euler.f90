real pi, r_keha, m_keha, c_keha, mu_gaas, g_rask, r_gaas, t_gaas, p_gaas, dt, x, v
integer i, nt

open(10, file="pall_euler.dot")
pi = 3.14159265
r_keha = 0.05
m_keha = 15.
c_keha = 0.47
s_keha = pi*r_keha**2
mu_gaas = 44.
g_rask = 8.87
r_gaas = 8314.
t_gaas = 700.
p_gaas = 9000000.
v_keha = 4*pi*r_keha**3/3
dt = 0.01
nt = 1000
p2 = p_gaas*mu_gaas/(r_gaas*t_gaas)
p1 = mu_gaas*g_rask/(r_gaas*t_gaas)

x = 0.
v = 0.

do i = 1, nt
t = i*dt
ro = p2 * exp(-p1 * x)
a = -g_rask + g_rask*ro*v_keha/m_keha - 0.5 * c_keha*s_keha*ro*abs(v) * v / m_keha
v = v + a*dt
x = x + v*dt - 0.5*a*dt*dt

e_kin = 0.5 * m_keha * v**2
e_pot = m_keha * g_rask * x
e_tot = e_kin + e_pot
write(10, *) t, x, v, a, e_kin, e_pot, e_tot

enddo

stop
end