! Avaliação 1
PROGRAM prova1
  IMPLICIT NONE

  ! VARIÁVEIS
  CHARACTER(LEN=50) :: Aluno
  CHARACTER(LEN=50) :: Dia
  INTEGER :: L1, L2, L3


  ! 2) Criar arquivo de saída
  OPEN (UNIT=10, FILE='./tmp/out1.txt', STATUS='UNKNOWN', ACTION='WRITE')

  ! 3) Leitura dos dados
  WRITE(*,*) 'Digite o nome completo do aluno:'
  READ(*,'(A)') Aluno

  WRITE(*,*) 'Digite a data de hoje (dia, mês por extenso e ano com 4 algarismos):'
  READ(*,'(A)') Dia

  WRITE(*,*) 'Digite os valores de L1, L2 e L3:'
  READ(*,*) L1, L2, L3

  CLOSE (10)
END PROGRAM prova1
