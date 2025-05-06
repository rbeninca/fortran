PROGRAM PROVA2_TM226B
    IMPLICIT NONE
  
    ! Declaração de variáveis (Item 2)
    REAL, ALLOCATABLE :: L(:)        ! Conjunto L (valores reais)
    INTEGER           :: A            ! Quantidade de elementos em L
    INTEGER           :: I, J, K, V   ! Variáveis de controle do ciclo
    REAL              :: M2, N, M6    ! Resultados dos cálculos
    INTEGER           :: i_loop       ! Índice para laços
    INTEGER           :: sum_cycle    ! Soma dos valores do ciclo
    INTEGER           :: io_status    ! Status para alocação e I/O
    CHARACTER(LEN=100):: nome_aluno  ! Nome completo do aluno
  
    ! Leitura do nome do aluno
    WRITE(*, *) 'Digite seu nome completo: '
    !READ(*, '(A)') nome_aluno
    nome_aluno="Romulo de Aguiar Beninca"
  
    ! Leitura de I, J, K, V, A (Item 2)
    WRITE(*, *) 'Digite os valores de I, J, K, V e A: '
    !READ(*, *) I, J, K, V, A
    I = -5 
    J = 22 
    K = 3 
    V = 30 
    A = 7

    ! Verificação de A positivo para alocação
    IF (A <= 0) THEN
      WRITE(*, *) 'Erro: A deve ser positivo'
      STOP
    END IF
  
    ! Alocação do conjunto L
    ALLOCATE(L(A), STAT=io_status)
    IF (io_status /= 0) THEN
      WRITE(*, *) 'Erro na alocação de L'
      STOP
    END IF
  
    ! Leitura dos elementos de L
    WRITE(*, *) 'Digite os ', A, ' elementos de L: '
    DO i_loop = 1, A
      !READ(*, *) L(i_loop)
    END DO
    !L = 1      -3.7   4E-2     2.13x101 5.7 12.8 5.6 x101
    L(1) = 1.0
    L(2) = -3.7
    L(3) = 0.04
    L(4) = 2.13E1
    L(5) = 5.7
    L(6) = -12.8
    L(7) = 5.6E-1
    
    L(1) = 1.0 
    L(2) = -3.7 
    L(3) = 4.0E-2 
    L(4) = 2.13E1 
    L(5) = 5.7 
    L(6) = -12.8 
    L(7) = 5.6E1
  
    ! Criação do arquivo de saída (Item 3)
    CALL ARQUIVO2()
  
    ! Escrita dos dados iniciais no arquivo (Item 4)
    CALL DADOS2(nome_aluno, I, J, K, V, A, L)
  
    ! Ciclo com condições especificadas (Item 5)
    sum_cycle = 0
    M2 = 0.0
    DO i_loop = I, J, K
      sum_cycle = sum_cycle + i_loop
      M2 = M2 + REAL(i_loop)
      IF (sum_cycle > V) EXIT
    END DO
  
    ! Cálculo da média dos valores assumidos pelo ciclo (Item 6)
    IF (i_loop > I) THEN
      M2 = M2 / REAL((i_loop - I + K - 1) / K)
    ELSE
      M2 = 0.0
    END IF
  
    ! Cálculo da soma dos cubos dos elementos de L (Item 7)
    N = 0.0
    DO i_loop = 1, A
      N = N + L(i_loop)**3
    END DO
  
    ! Cálculo da soma dos elementos de L entre 1 e 6 (Item 8)
    M6 = 0.0
    DO i_loop = 1, A
      IF (L(i_loop) >= 1.0 .AND. L(i_loop) <= 6.0) THEN
        M6 = M6 + L(i_loop)
      END IF
    END DO
  
    ! Escrita dos resultados e abertura do arquivo (Item 9)
    CALL ESCREVE2(M2, N, M6)
  
  END PROGRAM PROVA2_TM226B
  
  ! Sub-rotina para criar o arquivo de saída (Item 3)
  SUBROUTINE ARQUIVO2()
    IMPLICIT NONE
    INTEGER :: io_status
    OPEN(UNIT=10, FILE='Out2.txt', STATUS='REPLACE', IOSTAT=io_status)
    IF (io_status /= 0) THEN
      WRITE(*, *) 'Erro ao criar Out2.txt'
      STOP
    END IF
  END SUBROUTINE ARQUIVO2
  
  ! Sub-rotina para escrever dados iniciais no arquivo (Item 4)
  SUBROUTINE DADOS2(nome, I, J, K, V, A, L)
    IMPLICIT NONE
    CHARACTER(LEN=100), INTENT(IN) :: nome
    INTEGER, INTENT(IN)            :: I, J, K, V, A
    REAL, INTENT(IN)               :: L(A)
    INTEGER                        :: controle
    WRITE(10, *) 'Nome do aluno: ', TRIM(nome)
    WRITE(10, *) 'I  = ', I
    ! Valor de I
    WRITE(10, *) 'J  = ', J
    ! Valor de J
    WRITE(10, *) 'K  = ', K
    ! Valor de K
    WRITE(10, *) 'V  = ', V
    ! Valor de V
    WRITE(10, *) 'A  = ', A
    ! Quantidade de elementos em L
    DO controle = 1, A
      WRITE(10, *) 'L(', controle, ') = ', L(controle)
      ! Elemento i de L
    END DO
  END SUBROUTINE DADOS2
  ! Sub-rotina para escrever resultados e abrir o arquivo (Item 9)
  SUBROUTINE ESCREVE2(M2, N, M6)
    IMPLICIT NONE
    REAL, INTENT(IN) :: M2, N, M6
    WRITE(10, *) 'M2 = ', M2
    ! Média dos valores do ciclo
    WRITE(10, *) 'N  = ', N
    ! Soma dos cubos dos elementos de L
    WRITE(10, *) 'M6 = ', M6
    ! Soma dos elementos de L entre 1 e 6
    CLOSE(10)
    ! Abertura do arquivo no Notepad (Windows)
    CALL SYSTEM('notepad Out2.txt')
  END SUBROUTINE ESCREVE2