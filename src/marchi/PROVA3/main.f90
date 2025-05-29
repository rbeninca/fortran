program PROVA3
   use portlib
   use ROTINAS
   implicit none

   character(len=50) :: nome, dia
   real*8 :: K1, N, L
   real*8 :: M1, M2, M3, M4, M5, M6, M7
   integer :: b, d, X, aux
   real*8 :: fat


 ! Chamada � sub-rotina para obter os dados de entrada
   call DADOS(nome, dia, K1, d, N, L)

   ! Impressão para conferência na consola
   write(*,100) Nome
   write(*,101) Dia
   write(*,102) K1
   write(*,103) d
   write(*,104) N  ! Corrigido para usar o formato 104
   write(*,105) L  ! Corrigido para usar o formato 105
   write(*,106) X  ! Corrigido para usar o formato 106



! C�lculos matem�ticos
   M1 = dexp(K1)
   M2 = dlog(d)   ! Converte 'd' para double precision antes do logaritmo
   M3 = dcosh(N)

! C�lculo de M4 (S�rie)
   M4 = 0.d0
   do b = 0, int(d)
      M4 = M4 + (4.d0*(b-1)*(2*b+1))/(2*b+1)
   end do


! (f) Calcular M5
   M5 = 0.d0
   do b = 0, int(d), 2	  !problema ? para aqui
      M5 = M5 + 5.d0/factorial(b)
   end do
! (g) Calcular M6 e M7
   call SERIES(int(d), int(X), M6, M7)
   close(20)

! Abrir ficheiro de saída
  open(9, file="OUT3.TXT")

  ! Escrita dos dados de entrada no ficheiro
  write(9,100) Nome
  write(9,101) Dia
  write(9,102) K1
  write(9,103) d
  write(9,104) N
  write(9,105) L
  write(9,106) X

  ! Escrita dos resultados no ficheiro
  write(9,110) M1
  write(9,111) M2
  write(9,112) M3
  write(9,113) M4
  write(9,114) M5
  write(9,115) M6
  write(9,116) M7
  ! Fechar ficheiro
  close(9)
  ! Tenta abrir o ficheiro de saída com o notepad (específico para Windows)
  aux=SYSTEM("notepad OUT3.TXT")

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


