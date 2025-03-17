!Exercício 2.1
!Executar novamente o programa03.f90, versão B, com A = 2 e B = 1. Em seguida, analisar cada resultado,
!comparando-o com o valor esperado obtido de um cálculo mental, especialmente o caso da potenciação, cujo resultado deverá ser zero.
!O programa03.f90, versão B, é o seguinte:
PROGRAM programa03_f90
IMPLICIT NONE
INTEGER A, B, C, D, E, F, G
REAL H
A = 2
B = -1

C = A + B
D = B - A
E = A * B
F = A / B
G = A ** B
H = REAL(A) ** REAL(B)
WRITE(*,*) "A = ", A
WRITE(*,*) "B = ", B
WRITE(*,*) "C = A + B = ", C
WRITE(*,*) "D = B - A = ", D
WRITE(*,*) "E = A * B = ", E
WRITE(*,*) "F = A / B = ", F
WRITE(*,*) "G = A ** B = ", G
!Como o calculo é com inteiro, o resultado é inteiro, e não real como deveria ser
!Para corrigir, é necessário converter A e B para real antes de fazer a potenciação 
WRITE(*,*) "H = A ** B = ", H
END PROGRAM programa03_f90

!