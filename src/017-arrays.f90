PROGRAM exemplo_array
  IMPLICIT NONE

  INTEGER :: i
  INTEGER, DIMENSION(5) :: array1
  INTEGER, DIMENSION(3,3) :: matriz

  ! Inicializa o vetor
  array1 = (/ 10, 20, 30, 40, 50 /)

  ! Inicializa a matriz
  matriz = 0

  ! Mostra o vetor
  DO i = 1, 5
     PRINT *, 'array1(', i, ') = ', array1(i)
  END DO

  ! Atualiza matriz(2,3)
  matriz(2,3) = 99
  PRINT *, 'matriz(2,3) =', matriz(2,3)

END PROGRAM exemplo_array
