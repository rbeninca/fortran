PROGRAM programa_matrix

    ! Define uma matriz real de tamanho a ser definido pelo usuário
    IMPLICIT NONE
    INTEGER ::  n, m
    REAL, ALLOCATABLE, DIMENSION(:,:) :: matriz

    ! Solicita ao usuário o tamanho da matriz
    WRITE(*,*) "Entre com o numero de linhas da matriz (inteiro)"
    READ(*,*) n
    WRITE(*,*) "Entre com o numero de colunas da matriz (inteiro)"
    READ(*,*) m

    ! Aloca a matriz com base nas dimensões fornecidas
    ALLOCATE(matriz(n, m))

    ! Inicializa o gerador de números aleatórios
    CALL random_seed()

    ! Preenche a matriz com números aleatórios
    CALL popula_matriz(matriz)

    ! Imprime a matriz na tela
    CALL imprime_matriz(matriz)

CONTAINS

    SUBROUTINE imprime_matriz(matriz)
        ! Imprime a matriz na tela como matriz
        INTEGER :: i, j
        REAL, DIMENSION(:,:) :: matriz  ! Removido ALLOCATABLE

        WRITE(*,*) "Matriz: "
        DO i = 1, SIZE(matriz, 1)
            WRITE(*,*) (matriz(i, j), j = 1, SIZE(matriz, 2))
        END DO
    END SUBROUTINE

    SUBROUTINE popula_matriz(matriz)
        ! Preenche a matriz com números aleatórios
        INTEGER :: i, j
        REAL, DIMENSION(:,:) :: matriz  ! Removido ALLOCATABLE

        DO i = 1, SIZE(matriz, 1)
            DO j = 1, SIZE(matriz, 2)
                CALL random_number(matriz(i, j))  ! Preenche com números aleatórios
                matriz(i, j) = matriz(i, j) * 100.0  ! Multiplica por 100 para valores entre 0 e 100
            END DO
        END DO
    END SUBROUTINE

END PROGRAM programa_matrix
