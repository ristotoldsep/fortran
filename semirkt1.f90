real arv, uus_arv
integer arv_int
print '("Sisestage taisarv="$)'
read *,arv
arv_int = int(arv)
uus_arv = arv - arv_int
if (uus_arv /= 0) then
  print *, "Sisestatud arv ei ole taisarv"
end if
stop
end
