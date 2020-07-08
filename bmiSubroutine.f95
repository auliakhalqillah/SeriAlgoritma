! Program sederhana untuk menghitung niali Indeks Berat Badan/Body Mass Index (BMI)
! Ditulis oleh: Aulia Khalqillah (8 Juli 2020)
! email: personal@auliakhalqillah.com
!----------------------------------------------------------------------------------
program hitungbmi
implicit none

character(len=50) :: nama, klasifikasi
integer :: usia
real :: beratbadan, tinggi, bmi, tinggim

write(*,'(20a)',advance='no') "Masukan nama Anda:"
read*, nama
write(*,'(20a)',advance='no') "Masukan usia Anda:"
read*, usia
write(*,'(20a)',advance='no') "Masukan Berat Badan Anda (Kg):"
read*, beratbadan
write(*,'(20a)',advance='no') "Masukan tinggi Badan Anda (cm):"
read*, tinggi


! Memanggil Subroutine
call hitbmi(beratbadan,tinggi,tinggim,bmi,klasifikasi)

! Menyimpan variabel ke dalam file dengan format kolom
open(unit=2, file='data3.txt', status='replace')
write(*,'(a20,i10,f10.2,f10.2,f10.2,5x,a20)') nama, usia, beratbadan, tinggim, bmi, klasifikasi
write(2,'(a20,i10,f10.2,f10.2,f10.2,5x,a20)') nama, usia, beratbadan, tinggim, bmi, klasifikasi
close(2)

end program


subroutine hitbmi(beratbadan,tinggi,tinggi_meter,bmi,kelasbmi)
implicit none
real, intent(in) :: beratbadan,tinggi
real, intent(out) :: tinggi_meter, bmi
character(len=50), intent(out):: kelasbmi

! Konversi satuan tinggi dari cm ke meter
tinggi_meter = tinggi/100

! Htiung bmi
bmi= beratbadan/(tinggi_meter**2)

! klasifikasi
if (bmi < 15) then
kelasbmi = 'Sangat-sangat kurus'
elseif (bmi > 15 .and. bmi < 16) then
kelasbmi = 'Sangat kurus'
elseif (bmi > 16 .and. bmi < 18.5) then
kelasbmi = 'Kurang berat badan'
elseif (bmi > 18.5 .and. bmi < 25) then
kelasbmi = 'Berat badan normal'
elseif (bmi > 25 .and. bmi < 30) then
kelasbmi = 'Kelebihan berat badan'
elseif (bmi > 30 .and. bmi < 35) then
kelasbmi = 'Obesitas kelas 1'
elseif (bmi > 35 .and. bmi < 40) then
kelasbmi = 'Obesitas kelas 2'
elseif (bmi > 40) then
kelasbmi = 'Obesitas kelas 3'
end if

end subroutine

