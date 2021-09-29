      subroutine quanc8(fun,a,b,abserr,relerr,result,errest,nofun,flag)
	real qright(31),f(16),x(16),fsave(8,30),xsave(8,30)
c ***** input information *******
c ***** a - low limit
c	  b - up limit 
c	  relerr - border of rel. err.
c	  abserr - abs.err.
c ***** output information *****
c	  result - pribl. k integr., udovl.,mizno nad.,menee zest.
c                iz dvuh granic.
c       errest - realy errror	  
c	  nofun  - number of func. calc.
c       flag - 0 - O'k if xxx.yyy xxx-int. bez.shod. yyy-neobr. 
c              interval.
	levmin=1
	levmax=30
	levout=6
	nomax=5000
	nofin=nomax-8*(levmax-levout+2**(levout+1))
	w0=3956./14175.
	w1=23552./14175.
	w2=-3712./14175.
	w3=41984./14175.
	w4=-18160./14175.
	flag=0.
	result=0.
	cor11=0.
	errest=0.
	area=0.
	nofun=0
	if(a.eq.b) return
	lev=0
	nim=1
	x0=a
	x(16)=b
	qprev=0.
	f0=fun(x0)
	stone=(b-a)/16.
	x(8)=(x0+x(16))/2.
	x(4)=(x0+x(8))/2.
	x(12)=(x(8)+x(16))/2.
	x(2)=(x0+x(4))/2.
	x(6)=(x(4)+x(8))/2.
	x(10)=(x(8)+x(12))/2.
	x(14)=(x(12)+x(16))/2.
	do 25 j=2,16,2
	f(j)=fun(x(j))
25	continue
	nofun=9
30	x(1)=(x0+x(2))/2.
	f(1)=fun(x(1))
	do 35 j=3,15,2
	x(j)=(x(j-1)+x(j+1))/2.
	f(j)=fun(x(j))
35	continue
	nofun=nofun+8
	step=(x(16)-x0)/16.
     	qleft=(w0*(f0+f(8))+w1*(f(1)+f(7))+w2*
     *(f(2)+f(6))+w3*(f(3)+f(5))+w4*f(4))*step
     	qright(lev+1)=(w0*(f(8)+f(16))+w1*(f(9)+f(15))+w2*
     *(f(10)+f(14))+w3*(f(11)+f(13))+w4*f(12))*step
	qnow=qleft+qright(lev+1)
	qdiff=qnow-qprev
	area=area+qdiff
	esterr=abs(qdiff)/1023.
	tolerr=amax1(abserr,relerr*abs(area))*(step/stone)
	if(lev.lt.levmin) goto 50
	if(lev.ge.levmax) goto 62
	if(nofun.gt.nofin) goto 60
	if(esterr.le.tolerr) goto 70
50	nim=2*nim
	lev=lev+1
	do 52 i=1,8
	fsave(i,lev)=f(i+8)
	xsave(i,lev)=x(i+8)
52	continue
	qprev=qleft
	do 55 i=1,8
	j=-i
	f(2*j+18)=f(j+9)
	x(2*j+18)=x(j+9)
55	continue
	goto 30	
60	nofin=2*nofin
	levmax=levout
	flag=flag+(b-x0)/(b-a)
	goto 70
62	flag=flag+1.
70	result=result+qnow
	errest=errest+esterr
	cor11=cor11+qdiff/1023.
72	if(nim.eq.2*(nim/2)) goto 75
	nim=nim/2
	lev=lev-1
	goto 72
75	nim=nim+1
	if(lev.le.0) goto 80
	qprev=qright(lev)
	x0=x(16)
	f0=f(16)
	do 78 i=1,8
	f(2*i)=fsave(i,lev)
	x(2*i)=xsave(i,lev)
78	continue
	goto 30
80	result=result+cor11
	if(errest.eq.0.) return
82	temp=abs(result)+errest
	if(temp.ne.abs(result))return
	errest=2.*errest
	goto 82
	end
