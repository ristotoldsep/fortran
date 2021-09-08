! Failist test5.in lugemine
real a,b,c 

open(10,file="test5.in") !10 = faili nimi/number, andmete lugemiseks

open(11, file="test5.out") ! 11 faili nimi/number, andmete kirjutamiseks (fail luuakse automaatselt)

read (10,*) a,b  ! 2 kirjutatakse a kohale, 3 kirjutatakse b kohale

print *,'Failist tulid: ',a,'&',b

write (11, *) 'Failist tulid: ',a,'&',b ! Läks faili test5.out

stop
end
