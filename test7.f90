! DÜNAAMILINE ARVUTUS FAILIST LOETUD MASIIVIGA

real, allocatable::massiiv(:) ! Nii ei pea kohe massiivi pikkust määrama
real sum
integer n,i

open(10, file="test7.in")
read(10, *) n

allocate(massiiv(n)) !Määrame massiivile suuruse failist
read(10, *) massiiv ! iga "read" operaator võtab failist infot automaatselt uuelt realt

sum = 0
do i = 1,n
    sum = sum + massiiv(i)
end do

print *, sum/n

stop
end
