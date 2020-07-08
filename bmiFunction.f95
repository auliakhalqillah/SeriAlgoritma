! Program sederhana untuk menghitung niali Indeks Berat Badan/Body Mass Index (BMI)
! Ditulis oleh: Aulia Khalqillah (8 Juli 2020)
! email: personal@auliakhalqillah.com
!
! Catatan:
! Karena dalam program ini menghasilkan dua output, yaitu nilai bmi dan klasifikasi
! maka penggunaan function harus ditulis dua kali dengan algoritma dasar yang berbeda.
! Function pertama untuk menghitung nilai BMI dan Function kedua untuk mengklasifikasi
! berat badan berdasarkan nilai BMI.
!----------------------------------------------------------------------------------
program hitungbmi
implicit none

character(len=50) :: nama, klasifikasi, kelasbmi
integer :: usia
real :: beratbadan, tinggi, bmi, hitbmi

write(*,'(20a)',advance='no') "Masukan nama Anda:"
read*, nama
write(*,'(20a)',advance='no') "Masukan usia Anda:"
read*, usia
write(*,'(20a)',advance='no') "Masukan Berat Badan Anda (Kg):"
read*, beratbadan
write(*,'(20a)',advance='no') "Masukan tinggi Badan Anda (cm):"
read*, tinggi

! Konversi satuan tinggi dari cm ke meter
tinggi = tinggi/100

! Memanggil fungsi "hitbmi" untuk menghitung nilai bmi
bmi = hitbmi(beratbadan,tinggi)

! Memanggil fungsi "kelasbmi" untuk mengklasifikasi nilai bmi
klasifikasi = kelasbmi(bmi)

! Menyimpan variabel ke dalam file dengan format kolom
open(unit=2, file='data3.txt', status='replace')
write(*,'(a20,i10,f10.2,f10.2,f10.2,5x,a20)') nama, usia, beratbadan, tinggi, bmi, klasifikasi
write(2,'(a20,i10,f10.2,f10.2,f10.2,5x,a20)') nama, usia, beratbadan, tinggi, bmi, klasifikasi
close(2)

end program

function hitbmi(beratbadan,tinggi)
implicit none
real :: beratbadan,tinggi,hitbmi

! Htiung bmi
hitbmi= beratbadan/(tinggi**2)
return
end function

function kelasbmi(bmi)
implicit none
real ::bmi
character(len=50):: kelasbmi

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

end function

