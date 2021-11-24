! Seitsme reaalarvu liitmine

real v(7), sum ! Sum peab olema reaalarv, sest liidetakse reaalarvulisi numbreid
 integer i, n ! i deklareeritud 2x, fortran pole case sensitive, ühe (n2iteks suurema) võib eemaldada. n peab olema countimiseks integer tyypi 
 data v/1.,2.,3.,4.,5.,4.,3./ ! massiivis peab olema oige arv numbreid (nii palju kui deklareeritud on)
 n = 7     ! n peab olema sama suur kui massiiv, kuna kui suurem siis hakkab suvalisi tyhjasid m2luaadresse liitma ja tekib NaN, samas v6ib olla v2iksem kui massiivis arve on, siis liidetakse lihtsalt selle arvuni
 sum = 0. ! Summa v22rtustatakse enne tsykli algust nulliga

 do i = 1, n ! counter i v22rtust tostetakse iga kord kuni n-ni 
 sum = sum + v(i) ! Summale liidetakse iga iteratsioon j2rgmine massiivi arv
 enddo

 print *,"sum=",sum ! V2ljastatakse arvutatud summa
 stop
 end