program prova2
    implicit none
    ! VARIAVEIS
    character(len=100) :: TITULO, MES
    integer :: S, i
    real :: N1, N2, N3, N4, N5, soma
    real, dimension(:), allocatable :: K
    
    ! Abrir arquivo de saida
    open(unit=10, file='out2.txt', status='replace', action='write')
  
    ! inicializacao dados o dos dados (item 11 do enunciado)
    WRITE (*,*) "Digite o nome completo do aluno:"
    READ (*,*) TITULO
    !TITULO = "ROMULO DE AGUIAR BENINCA"
    WRITE (*,*) "Digite o mes e ano (ex: 6 de Maio de 2025):"
    READ (*,"(A)") MES
    !MES = "6 de Maio de 2025"    
    WRITE (*,*) "Digite o numero de elementos do vetor K:"
    READ (*,*) S
    !S = 10
    
    allocate(K(S))
    
    !LE VALORES DO VETOR K
    WRITE (*,*) "Digite os valores do vetor K:"
    do i = 1, S
        WRITE (*,*) "K(", i, ") = "
        READ (*,*) K(i)
    end do

   ! inicializacao para teste durante a prova
    
   ! K(1) = 3E-2
   ! K(2) = -2.5
   ! K(3) = 4.7E-1
   ! K(4) = 0.9
   ! K(5) = 1.6
   ! K(6) = -1.0
   ! K(7) = 4E-2
   ! K(8) = 2.13E1
   ! K(9) = 5.7
   ! K(10) = -12.8
  
    ! Escrever TITULO, MES e S
    write(10, '(A,A)') "TITULO=",TITULO 
    write(10, '(A,A)') "MES=",MES 
    write(10, '(A,I4)')  "S=",S
  
    ! Escrever os valores de K 
    do i = 1, S 
      write(10, '(A,I2,A,2X,ES10.3)') "K(",i,")=", K(i) ! K(i)
      !write(*, '(A,I2,A,2X,ES10.3)') "K(",i,")=", K(i) ! K(i)
    end do
  
    ! Calcular media dos valores do conjunto K (N1)
    soma = sum(K)
    N1 = soma / S
    write(10, '(A,ES10.3)') "N1=", N1 
    !write( *, '(A,ES10.3)') "N1=", N1 
  
    
    !Percorrendo o vetor K e soma os valores negativos ! Calcular soma dos valores negativos (N2)
    N2 = 0.0 
    do i = 1, S
        if (K(i) < 0.0) then
            N2 = N2 + K(i)
        end if
    end do
    write(10, '(A,ES10.3)') "N2=", N2
    !write(*, '(A,ES10.3)') "N2=", N2
  
    !Percorre o vetor K e soma os valores no intervalo [-1, 20]
    N3 = 0.0
    do i = 1, S
        if (K(i) >= -1.0 .and. K(i) <= 20.0) then
            N3 = N3 + K(i)
        end if
    end do
    !soma_intervalo = sum(K, mask = (K >= -1 .and. K <= 20))
    write(10, '(A,ES10.3)')"N3=", N3 
    !write(*, '(A,ES10.3)')"N3=", N3 
  
    
    
    !percorrendo o vetor K e soma os quadrados 
    N4 = 0.0
    DO i = 1, S
        N4 = N4 + K(i)**2
    END DO
    !soma_quadrados = sum(K**2)
    N4 = sqrt(N4)
    write(10, '(A,ES10.3)') "N4=",N4 
    !write(*, '(A,ES10.3)') "N4=",N4 
  
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