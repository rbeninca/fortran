PROGRAM teste1a;



IMPLICIT NONE
!4) Definir as seguintes variáveis: A, B, C e D do tipo inteiro; e todas as outras variáveis necessárias ao programa. 
INTEGER :: A, B, C, D
REAL(8) :: K, M, N, J
!Variavel para definir o nome do arquivo e data
CHARACTER(LEN=100) :: NOME
!Variavel para definir a data hora minuto
INTEGER :: DATA_HORA(8)




!5)Ler os DADOS do programa, que são os valores das variáveis A, B, C, D.
PRINT *, 'Digite os valores de A, B, C e D:'
!READ *, A, B, C, D
A = 3
B = 10
C = 2
D = 7

!6) [10 pontos] Criar o arquivo de saída chamado SAIDA1a.TXT e escrever nele o seu nome completo e data de hoje

OPEN(UNIT=10, FILE='./tmp/SAIDA1a.TXT', STATUS='UNKNOWN', ACTION='READWRITE')
write (10,*) 'AVALIACAO 1 - FORTRAN'
WRITE (10,*) 'Nome: ', 'ROMULO DE AGUIAR BENINCA'
CALL date_and_time(VALUES=DATA_HORA)
WRITE(10,"(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)") &
'Data: ', DATA_HORA(1), '/', DATA_HORA(2), '/', DATA_HORA(3), ' - ', DATA_HORA(4), ':', DATA_HORA(5), ':', DATA_HORA(6)


!7) [10 pontos] Escrever no arquivo de saída os valores lidos no item 5 de todos os dados do programa, juntamente com comentários para distinguir cada um deles.
WRITE(10,*) 'Valores lidos:'
WRITE(10,*) 'A = ', A
WRITE(10,*) 'B = ', B
WRITE(10,*) 'C = ', C
WRITE(10,*) 'D = ', D


!8) Nos itens abaixo, escrever significa escrever o resultado de um cálculo ou operação no arquivo de saída juntamente com um comentário para identificá-lo
!9) [10 pontos] Escrever no arquivo de saída o resultado da soma de A + B + C + D
K = 1.0 / (C**(B-A) - 1.0/D)
 
!Calcular a raiz quadrada  A^2+1/(1/(C**(1/D)))
M = (1.0 / (3.0 - B)) * SQRT(A**2 + 1.0 / (C**(1.0 / D)))
WRITE(10,*) 'K = ', K
WRITE(10,*) 'M = ', M

!11) [15 pontos] Calcular e escrever a exponencial do número 2.5 e atribuir o resultado à variável real N

N = EXP(2.5)
WRITE(10,*) 'N = ', N

!12) [15 pontos] Calcular e escrever a média aritmética da soma dos valores dos 4 dados (A, B, C e D) e atribuir o resultado à variável real J.

J = (A + B + C + D) / 4.0
WRITE(10,*) 'J = ', J


!13) [10 pontos] Calcular e escrever a média geométrica da soma dos valores dos 4 dados (A, B, C e D) e atribuir o resultado à variável real J.
J = ((A * B * C * D)**(1.0/4.0))
WRITE(10,*) 'J = ', J

!14) Executar o programa usando: A = 3; B = 10; C = 2; D = 7





END PROGRAM teste1a