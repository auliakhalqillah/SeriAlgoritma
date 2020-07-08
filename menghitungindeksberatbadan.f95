program hitungbmi
implicit none

character(len=50), dimension(3) :: nama, klasifikasi
integer, dimension(3) :: usia
real, dimension(3) :: beratbadan, tinggi, bmi
integer :: i

do i = 1,3
write(*,'(20a)',advance='no') "Masukan Nama Anda:"
read*, nama(i)
write(*,'(20a)',advance='no') "Masukan Usia Anda:"
read*, usia(i)
write(*,'(20a)',advance='no') "Masukan Berat Badan Anda (Kg):"
read*, beratbadan(i)
write(*,'(20a)',advance='no') "Masukan Tinggi Badan Anda (cm):"
read*, tinggi(i)

write(*,*)""
end do

! Menyimpan variabel ke dalam file dengan format kolom
open(unit=2, file='data2.txt', status='replace')
do i = 1,3
! Konversi satuan tinggi dari cm ke meter
tinggi(i) = tinggi(i)/100

! Htiung BMI
bmi(i) = beratbadan(i)/(tinggi(i)**2)

! Klasifikasi
if (bmi(i) < 15) then
klasifikasi(i) = 'Sangat-sangat kurus'
elseif (bmi(i) > 15 .and. bmi(i) < 16) then
klasifikasi(i) = 'Sangat kurus'
elseif (bmi(i) > 16 .and. bmi(i) < 18.5) then
klasifikasi(i) = 'Kurang berat badan'
elseif (bmi(i) > 18.5 .and. bmi(i) < 25) then
klasifikasi(i) = 'Berat badan normal'
elseif (bmi(i) > 25 .and. bmi(i) < 30) then
klasifikasi(i) = 'Kelebihan berat badan'
elseif (bmi(i) > 30 .and. bmi(i) < 35) then
klasifikasi(i) = 'Obesitas kelas 1'
elseif (bmi(i) > 35 .and. bmi(i) < 40) then
klasifikasi(i) = 'Obesitas kelas 2'
elseif (bmi(i) > 40) then
klasifikasi(i) = 'Obesitas kelas 3'
end if


write(*,'(a20,i10,f10.2,f10.2,f10.2,5x,a20)') nama(i), usia(i), beratbadan(i), tinggi(i), bmi(i), klasifikasi(i)
write(2,'(a20,i10,f10.2,f10.2,f10.2,5x,a20)') nama(i), usia(i), beratbadan(i), tinggi(i), bmi(i), klasifikasi(i)
end do
close(2)


end program
