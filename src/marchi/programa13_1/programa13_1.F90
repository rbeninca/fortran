PROGRAM PROGRAMA13_1
   !USE ISO_FORTRAN_ENV não suportado no powerstation
   IMPLICIT NONE  
   REAL A, C
   REAL(8) B, D, E, F
   WRITE(*,*) "Entre com o valor de A (real simples)"
   !READ(*,*) A
   A= 2/3.0;
   WRITE(*,*) "Entre com o valor de B (real dupla)"
   !READ(*,*) B
   B = 2/3.0d0

   C = 2/3.0
   D = 2/3.0
   E = 2/3.0e0
   F = 2/3.0d0
   WRITE(*,*)
   WRITE(*,*) "reais simples"
   WRITE(*,*) "A = ", A
   WRITE(*,*) "C = ", C
   WRITE(*,*)
   WRITE(*,*) "reais duplas"
   WRITE(*,*) "B = ", B
   WRITE(*,*) "D = ", D
   WRITE(*,*) "E = ", E
   WRITE(*,*) "F = ", F

END PROGRAM PROGRAMA13_1
