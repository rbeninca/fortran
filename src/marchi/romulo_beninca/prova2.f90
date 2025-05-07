program prova2
   implicit none
   ! VARIAVEIS
   character(len=100) :: TITULO, MES
   integer :: S, i
   real :: N1, N2, N3, N4, N5, soma
   real, dimension(:), allocatable :: K
   LOGICAL  ::debug=.TRUE.
   !se o debug estiver ativo, o programa vai usar os valores de teste

   ! Abrir arquivo de saida
   open(unit=10, file='out2.txt', status='replace', action='readwrite')

   ! inicializacao dados o dos dados (item 11 do enunciado)
   WRITE (*,*) "Digite o nome completo do aluno:"
   IF (.NOT. debug )  READ (*,*) TITULO
   IF (debug)TITULO = "ROMULO DE AGUIAR BENINCA"

   WRITE (*,*) "Digite o mes e ano (ex: 6 de Maio de 2025):"
   IF (.NOT. debug) READ (*,"(A)") MES
   IF (debug) MES = "6 de Maio de 2025"

   WRITE (*,*) "Digite o numero de elementos do vetor K:"
   IF (.NOT. debug)  READ (*,*) S
   IF (debug) S = 10

   allocate(K(S))

   !LE VALORES DO VETOR K
   WRITE (*,*) "Digite os valores do vetor K:"
   do i = 1, S
      WRITE (*,*) "K(", i, ") = "
      IF (.NOT. debug)  READ (*,*) K(i)
      IF (debug) k = (/ 3E-2, -2.5, 4.7E-1, 0.9, 1.6, -1.0, 4E-2, 2.13E1, 5.7, -12.8 /)
   end do

   ! Escrever TITULO, MES e S
   write(10, '(A,A)') "TITULO=",TITULO
   IF (debug)  write(*, '(A,A)') "TITULO=",TITULO
   write(10, '(A,A)') "MES=",MES
   IF (debug)  write(*, '(A,A)') "MES=",MES
   write(10, '(A,I4)')  "S=",S
   IF (debug)  write(*, '(A,I4)')  "S=",S

   ! Escrever os valores de K
   do i = 1, S
      write(10, '(A,I2,A,2X,ES10.3)') "K(",i,")=", K(i) 
      if (debug) then
         write(*, '(A,I2,A,2X,ES10.3)') " K(",i,")=", K(i) 
      end if
   end do

   ! Calcular media dos valores do conjunto K (N1)
   soma = sum(K)
   N1 = soma / S
   write(10, '(A,ES10.3)') "N1=", N1
   if (debug) write(*, '(A,ES10.3)') "N1=", N1


   !Percorrendo o vetor K e soma os valores negativos 
   N2 = 0.0
   do i = 1, S
      if (K(i) < 0.0) then
         N2 = N2 + K(i)
      end if
   end do
   write(10, '(A,ES10.3)') "N2=", N2
   if (debug)  write(*, '(A,ES10.3)') "N2=", N2
   

   !Percorre o vetor K e soma os valores no intervalo [-1, 20]
   N3 = 0.0
   do i = 1, S
      if (K(i) >= -1.0 .and. K(i) <= 20.0) then
         N3 = N3 + K(i)
      end if
   end do
   !soma_intervalo = sum(K, mask = (K >= -1 .and. K <= 20))
   write(10, '(A,ES10.3)')"N3=", N3
   if (debug)  write(*, '(A,ES10.3)')"N3=", N3
   
   !percorrendo o vetor K e soma os quadrados
   N4 = 0.0
   DO i = 1, S
      N4 = N4 + K(i)**2
   END DO
   !soma_quadrados = sum(K**2)
   N4 = sqrt(N4)
   write(10, '(A,ES10.3)') "N4=",N4
   if (debug)  write(*, '(A,ES10.3)') "N4=",N4
   

   ! Chamar subrotina para calcular desvio padrão (N5)
   call DESVIO(K, S, N1)

   close(10)
   CALL SYSTEM("start notepad out2.txt")

contains

   subroutine DESVIO(K, S, N1)
      implicit none
      integer, intent(in) :: S
      real, intent(in) :: K(:), N1
      real :: soma_desvios

      soma_desvios = sum((K - N1)**2)
      N5 = sqrt(soma_desvios / (S - 1))
      write(10, '(A,ES10.3)') "N5=",N5
      write(*, '(A,ES10.3)') "N5=",N5
   end subroutine DESVIO

end program prova2
