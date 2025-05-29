program PROVA3
use ROTINAS
implicit none

character(len=100) :: nome, dia
real*8 :: K1, d, N, L, X, M1, M2, M3, M4, M5, M6, M7
integer :: b, aux
open(20, file='OUT3.TXT',STATUS='UNKNOWN',ACTION='WRITE')
call DADOS(nome, dia, K1, d, N, L, X)

! (a) Escrever dados
write(20,'(A)') nome ! Nome
write(20,'(A)') dia  ! Dia
write(20,'(E11.9)') K1     ! K1
write(20,'(E11.9)') d      ! d
write(20,'(E11.9)') N      ! N
write(20,'(E11.9)') L      ! L
write(20,'(E11.9)') X      ! X

! (b) M1 = exp(K1)
M1 = dexp(K1)
write(20,'(E11.9)') M1 ! M1

! (c) M2 = log(d)
M2 = dlog(d)
write(20,'(E11.9)') M2 ! M2

! (d) M3 = cosh(N)
M3 = dcosh(N)
write(20,'(E11.9)') M3 ! M3

! (e) Calcular M4
M4 = 0.d0
do b = 0, int(d)
  M4 = M4 + (4.d0*(b-1)*(2*b+1))/(2*b+1)
end do
write(20,'(E11.9)') M4 ! M4

! (f) Calcular M5
M5 = 0.d0
do b = 0, int(d), 2
  M5 = M5 + 5.d0/factorial(b)
end do
write(20,'(E11.9)') M5 ! M5

! (g) Calcular M6 e M7
call SERIES(int(d), int(X), M6, M7)
write(20,'(E11.9)') M6 ! M6
write(20,'(E11.9)') M7 ! M7
close(20)
CALL SYSTEM("start notepad OUT3.TXT")
end program PROVA3
									   