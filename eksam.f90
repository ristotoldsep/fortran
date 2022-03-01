real y(6),work(27),mu_atm,m_keha !reaalväärtusega muutujad
integer iwork(5) ! täiskohalised muutujad
common g_rask,ro_atm_0,ro_keha,param1,param2, roh, v ! globaalsed muutujad
 
external func ! viide välisele funktsioonile
 
open(10,file="eksam.dat") ! eksam.dat-nimelise faili loomine/avamine
open(20,file="tulemus.dat") ! tulemus.dat-nimelise faili loomine/avamine
 
pi=3.1415926536 !pi väärtus
g_rask=3.711 !gravitatsioonijõu väärtus
r_keha=0.05 ! keha raadius 
s_keha=pi*r_keha**2 ! keha ristlõike pindala 
c_keha=0.47 ! keha aerodünaamiline tegur
v_keha=4.*pi*r_keha**3/3 ! keha ruumala
m_keha=1 ! keha mass
ro_keha=m_keha/v_keha ! keha tihedus
 
p_atm=600. ! Marsi atmosfääri rõhk 
t_atm=200. ! Marsi atmosfääri temperatuur 
mu_atm=44. ! ühe kilomooli gaasi mass Marsi atmosfääris
r_gaas=8314. ! universaalne gaasi konstant
 
ro_atm_0=p_atm*mu_atm/(r_gaas*t_atm) ! Marsi atmosfääri tihedus Marsi pinnal
 
! arvutamisi lihtsustavad parameetrid
param1=g_rask*mu_atm/(r_gaas*t_atm)
param2=0.5*c_keha*s_keha/m_keha
 
! rkf45 programmi jaoks vajalikud muutujad
neqn=4  !võrrandite arv
nt=10000 !ajasammude arv
dt=0.01 !ajasammu pikkus 
t=0. !algne aeg
 
!vigade suurused
relerr=1.e-7
abserr=1.e-7
 
!algtingimused
y(1)=0. !x-koordinaat
y(2)=0. !y-koordinaat
y(3)=3.5 !x-suunaline kiirus
y(4)=3.5 !y-suunaline kiirus
y(5)=0. !x-suunaline kiirendus
y(6)=0. !y-suunaline kiirendus
 
!rkf45 programmi alustamiseks vajalik muutuja, näitab et alustame uue simulatsiooniga
iflag=1
 
!tsükkel, mille sees hakkab rkf45 tulemusi arvutama iga ajahetke kohta
do it=1,nt
 
tout=t+dt !uus ajahetk
 
call rkf45(func,neqn,y,t,tout,relerr,abserr,iflag,work,iwork) !rkf45 programm
 
!iflagi kontroll, peab olema väärtusega 2, et edasi minna
if (iflag.ne.2) then
iflag=2
endif
 
! y-koordinaadi kontroll, et y-väärtus ei oleks negatiivne, muidu keha asetseks pinnasest allpool
if(y(2).le.0.) then !kui väärtus on negatiivne siis leitakse lennupikkus ja keha maandumise kiirus ning lõpetatakse tsükkel
    pikkus_lennu = abs(y(1)) !lennupikkus
    tulemus_maandumisKiirus = sqrt((y(3))**2+y(4)**2) !keha maandumise kiirus
    write (20,*)pikkus_lennu, tulemus_maandumisKiirus

stop
endif
 
ekin=0.5*m_keha*(y(3)*y(3)+y(4)*y(4)) ! kineetiline energia
epot=m_keha*g_rask*y(2) ! potentsiaalne energia
etot=ekin+epot ! koguenergia
 
y(5)=(-param2*roh*v*(y(3))) ! x-suunaline kiirendus
y(6)=g_rask*(roh/ro_keha-1.)-param2*roh*v*y(4) ! y-suunaline kiirendus
 
write (10,*)tout,y, ekin, epot, etot !exam.dat faili kirjutamine,aeg, y-masiivi väärtused, energiate väärtused



enddo !tsükli lõpetaja
 
stop !programmi peatamine
end !programmi lõpetamine
 
subroutine func(t,y,yp) !funktsioon, mida rkf45 kasutab, et teada saada
                        !milliseid väärtusi on vaja diferentseerida
 
real y(4),yp(4) !reaalarvulised muutujad
common g_rask,ro_atm_0,ro_keha,param1,param2, roh, v !globaalsed muutujad
 
yp(1)=y(3) !x-koordinaadile väärtuse andmine
yp(2)=y(4) !y-koordinaadile väärtuse andmine
 
roh=ro_atm_0*exp(-param1*y(2)) !atmosfääri gaasi tiheduse leidmine kõrgusel y(2)
 
v=sqrt((y(3))**2+y(4)**2) !kiiruse absoluutväärtus
 
yp(3)=(-param2*roh*v*(y(3))) !x-suunalisnie kiiruse arvutamine
yp(4)=g_rask*(roh/ro_keha-1.)-param2*roh*v*y(4)!y-suunalise kiiruse arvutamine



return !väärtuste tagastamine
end !funktsiooni lõpetamine
