PROGRAM exercicio03f90
    !VARS
    REAL A, C, B, D
    REAL H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12
    WRITE(*,*) "Entre com os valores de A, C, B e D"
    READ(*,*) A, C, B, D
    WRITE(*,*) "A = ", A

    WRITE(*,*) "B = ", B
    WRITE(*,*) "C = ", C
    WRITE(*,*) "D = ", D
    WRITE(*,*)
    H1 = A + B - C
    H2 = A + B / C
    H3 = (A + B) / C
    H4 = A + B / C * D
    H5 = A + B / (C * D)
    H6 = A + B * C ** D
    H7 = A + (B * (C ** D))
    H8 = 1.0E10 + 1.0E20
    H9 = C ** 3 ** 2
    H10 = (1.0 / 3 ) * ( (B**3) ** (1/ 2.0) )
    H11 = (1.0 / 3.0) * ( (B**3) ** (1.0 / 2.0) )
    H12 = (1    / 3) * ( (B**3) ** (1/ 2) )
    WRITE(*,*) "H1= ", H1
    WRITE(*,*) "H2= ", H2
    WRITE(*,*) "H3= ", H3
    WRITE(*,*) "H4= ", H4
    WRITE(*,*) "H5= ", H5
    WRITE(*,*) "H6= ", H6
    WRITE(*,*) "H7= ", H7
    WRITE(*,*) "H8= ", H8
    WRITE(*,*) "H9= ", H9
    WRITE(*,*) "H10 = ", H10
    WRITE(*,*) "H11 = ", H11
    WRITE(*,*) "H12 = ", H12
END PROGRAM exercicio03f90