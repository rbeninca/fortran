    PROGRAM PROGRAMA13_2
        IMPLICIT NONE
    INTEGER D

    REAL A, H
    REAL*4 B
    REAL(4) C
    REAL*8 E, I
    REAL(8) F
    DOUBLE PRECISION G
    WRITE(*,*) "Entre com o valor de A (real simples)"
    READ(*,*) A
    D = A
    B = 1 / D
    C = 1.0 / D
    E = A
    F = 1 / D
    G = 1.0d0 / D
    H = 1 + 1.0e-10
    I = 1 + 1.0d-10
    WRITE(*,*)
    WRITE(*,*) "inteiro"
    WRITE(*,*) "D = ", D
    WRITE(*,*)
    WRITE(*,*) "reais simples"
    WRITE(*,*) "A = ", A
    WRITE(*,*) "B = ", B
    WRITE(*,*) "C = ", C
    WRITE(*,*) "H = ", H
    WRITE(*,*)
    WRITE(*,*) "reais duplas"
    WRITE(*,*) "E = ", E
    WRITE(*,*) "F = ", F
    WRITE(*,*) "G = ", G
    WRITE(*,*) "I = ", I
END PROGRAM PROGRAMA13_2
