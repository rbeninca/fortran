! Avaliação 1
PROGRAM prova1
  IMPLICIT NONE
  ! 2) Criar arquivo de saída
  OPEN (UNIT=10, FILE='./tmp/out1.txt', STATUS='UNKNOWN', ACTION='WRITE')

  CLOSE (10)
END PROGRAM prova1
