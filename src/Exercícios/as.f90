program prova2
    implicit none
  
    ! Declaração de variáveis
    character(len=100) :: TITULO, MES
    integer :: S, i
    real :: N1, N2, N3, N4, N5, soma, soma_neg, soma_intervalo, soma_quadrados
    real, dimension(:), allocatable :: K
  
    ! Abrir arquivo de saída
    open(unit=10, file='out2.txt', status='replace', action='write')
  
    ! Inicialização dos dados (item 11 do enunciado)
    TITULO = "Nome Completo do Aluno"
    MES = "6 de Maio de 2025"
    S = 10
    allocate(K(S))
    K(1) = 3E-2
    K(2) = -2.5
    K(3) = 4.7E-1
    K(4) = 0.9
    K(5) = 1.6
    K(6) = -1.0
    K(7) = 4E-2
    K(8) = 2.13E1
    K(9) = 5.7
    K(10) = -12.8
  
    ! Escrever TITULO, MES e S
    write(10, '(A,A)') "TITULO=", TITULO ! TITULO
    write(10, '(A,A)') "MES=", MES        ! MES
    write(10, '(A,I4)') "S=", S           ! S
  
    ! Escrever os valores de K com comentários
    do i = 1, S
      write(10, '(A,I2,A,ES10.3)') "K(", i, ")=", K(i) ! K(i)
    end do
  
    ! Calcular média dos valores do conjunto K (N1)
    soma = sum(K)
    N1 = soma / S
    write(10, '(A,ES10.3)') "N1=", N1 ! N1
  
    ! Calcular soma dos valores negativos (N2)
    soma_neg = sum(K, mask = K < 0)
    write(10, '(A,ES10.3)') "N2=", soma_neg ! N2
  
    ! Calcular soma dos valores no intervalo [-1, 20] (N3)
    soma_intervalo = sum(K, mask = (K >= -1 .and. K <= 20))
    write(10, '(A,ES10.3)') "N3=", soma_intervalo ! N3
  
    ! Calcular raiz quadrada da soma dos quadrados (N4)
    soma_quadrados = sum(K**2)
    N4 = sqrt(soma_quadrados)
    write(10, '(A,ES10.3)') "N4=", N4 ! N4
  
    ! Chamar subrotina para calcular desvio padrão (N5)
    call DESVIO(K, S, N1)
  
    ! Fechar o arquivo
    close(10)
  
  contains
  
    subroutine DESVIO(K, S, N1)
      implicit none
      integer, intent(in) :: S
      real, intent(in) :: K(:), N1
      real :: soma_desvios, N5
  
      soma_desvios = sum((K - N1)**2)
      N5 = sqrt(soma_desvios / (S - 1))
      write(10, '(A,ES10.3)') "N5=", N5 ! N5
    end subroutine DESVIO
  
  end program prova2
  