program PROVA3
use ROTINAS
implicit none

character(len=100) :: nome, dia
real*8 :: K1, d, N, L, X, M1, M2, M3, M4, M5, M6, M7
integer :: b, aux

call DADOS(nome, dia, K1, d, N, L, X)

 ! Impressão para conferência na consola
  write(*,100) Nome
  write(*,101) Dia
  write(*,102) K1
  write(*,103) d
  write(*,104) N  ! Corrigido para usar o formato 104
  write(*,105) L  ! Corrigido para usar o formato 105
  write(*,106) X  ! Corrigido para usar o formato 106



! (b) M1 = exp(K1)
M1 = dexp(K1)
! (c) M2 = log(d)
M2 = dlog(d)
! (d) M3 = cosh(N)
M3 = dcosh(N)
! (e) Calcular M4
M4 = 0.d0
do b = 0, int(d)
  M4 = M4 + (4.d0*(b-1)*(2*b+1))/(2*b+1)
end do


! (f) Calcular M5
M5 = 0.d0
do b = 0, int(d), 2
  M5 = M5 + 5.d0/factorial(b)
end do
! (g) Calcular M6 e M7
call SERIES(int(d), int(X), M6, M7)
close(20)

open(9, file='OUT3.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 
 ! Escrita dos dados de entrada no ficheiro
  write(9,100) Nome
  write(9,101) Dia
  write(9,102) K1
  write(9,103) d
  write(9,104) N
  write(9,105) L
  write(9,106) 20

  write(9,100) Nome
  write(9,101) Dia
  write(9,102) K1
  write(9,103) d
  write(9,104) N
  write(9,105) L
  write(9,106) X

CALL SYSTEM("start notepad OUT3.TXT")

! FORMATs obrigatórios
100 format('Nome = ', A50)
101 format('Dia = ', A50)
102 format('K1 = ', 1PE20.10)
103 format('d = ', I10)
104 format('N = ', 1PE20.10)  ! Formato para N
105 format('L = ', 1PE20.10)  ! Formato para L
106 format('X = ', I10)      ! Formato para X
110 format('M1 = ', 1PE20.10)
111 format('M2 = ', 1PE20.10)
112 format('M3 = ', 1PE20.10)
113 format('M4 = ', 1PE20.10)
114 format('M5 = ', 1PE20.10)
115 format('M6 = ', 1PE20.10)
116 format('M7 = ', 1PE20.10)

end program PROVA3

									   