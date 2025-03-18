!Em Fortran, números reais são representados com o sinal de ponto para separar a parte inteira da decimal; isto será visto com detalhes no Capítulo 3.
!A tentativa de ler um número real, para atribuí-lo a uma variável do tipo inteiro, gera erro de execução do programa. Por exemplo, executar novamente o programa03.f90, versão B, usando A = 1.5 e B = 0.4.

PROGRAM programa03_f90
IMPLICIT NONE
    REAL A, B, C, D, E, F, G
    WRITE(*,*) "Entre com o valor de A"
    READ(*,*) A
    WRITE(*,*) "Entre com o valor de B"
    READ(*,*) B
    C = A + B
    D = B - A
    E = A * B
    F = A / B
    G = A ** B
    WRITE(*,*) "A = ", A
    WRITE(*,*) "B = ", B
    WRITE(*,*) "C = A + B = ", C
    WRITE(*,*) "D = B - A = ", D
    WRITE(*,*) "E = A * B = ", E
    WRITE(*,*) "F = A / B = ", F
    WRITE(*,*) "G = A ** B = ", G
END PROGRAM programa03_f90