PROGRAM  parcial01
IMPLICIT NONE

!b) valor (10) Defina as seguintes variáveis: inteiras (b1, b2, b3, b4); reais (k1, k2, k3, k4, k5, k6,k7); caracter, comprimento 50 (p, q); caracter, comprimento 150 (r). 
INTEGER :: b1, b2, b3, b4
REAL(8) :: k1, k2, k3, k4, k5, k6, k7
CHARACTER(LEN=50) :: p, q , r
INTEGER :: DATA_HORA(8)


!c) valor (10) Leia o conteúdo das variáveis b1, b2, b3, k1, p, q, com comentários para identificálas.
PRINT *, 'Digite os valores de b1 (INTEGER), b2 (INTEGER), b3 (INTEGER), k1 (REAL), p(CHAR(50)) e q(CHAR(50):'
!READ *, b1, b2, b3, k1, p, q
!b1 = –2; b2 = 4; b3 = 7; k1 = -2,73; p = "seu nome"; q = 'seu sobrenome' 
b1 = -2
b2 = 4
b3 = 7
k1 = -2.73
p = 'Romulo'
q = 'Beninca'

!d) valor (20) O conteúdo da variável r deve ser obtido através da concatenação do comentário "Programação II - " com o conteúdo da variável p com um espaço em branco seguido pela concatenação com a variável q. Utilize os comandos adequados para retirar todos os espaços em branco existentes na variável p durante a concatenação. Caso elementos extras, não requeridos, sejam concatenados à variável r, o item será desconsiderado para critérios de correção.

r = TRIM('Programação II - ') // TRIM(p) // ' ' // TRIM(q)

!e) valor (42) Empregue as seguintes expressões para obter os resultados para as variáveis b4, k2, k3, k4, k5, k6, k7: 
! Calcular b4 como o número de caracteres efetivamente empregados em r
b4 = LEN_TRIM(r)

!Expressão  k2 
k2 = LOG(ABS(1.0 * (b2 - b3 + b1))) + SINH((ABS(1.0 * k1))**(1.0 / 3.0))


!Expressão  k3
k3 = ((1.0 * b1 * b3 + b2)**(1.0 / 3.0)) + (b1 / (2.0 * b2 * b3))

!Expressão  k4
k4 = LOG(ABS(1.0 * (b1 * b3) / b2)) + EXP(1.0 * (b1 + b2))

!Expressão  k5
k5= cos(1.0*(b1/b2)) + ABS(b3)**(b2/b3);



!k6 é o módulo da parte decimal da divisão entre b2 e b3. 
k6=ABS((1.0*b2)/(1.0*b3)) - (ABS((1.0*b2)/(1.0*b3)));

!k7 é o mínimo entre os valores absolutos de b1, b2, b3, k1, k2, k3. 
k7=MIN(ABS(b1), ABS(b2), ABS(b3), (ABS(int(k1))), ABS(int(k2)), ABS(int(k3)));

!f) valor (18) Crie um arquivo de saída de dados chamado "dados_saida.txt", escrevendo nele os valores/conteúdos das variáveis b1, b2, b3, b4, k1, k2, k3, k4, k5, k6, k7, p, q, r, com comentários para identificá-las. Os resultados numéricos das variáveis reais devem ser apresentados em formato científico com 4 dígitos decimais. Faça a abertura automática do arquivo de saída de dados. 
OPEN(UNIT=10, FILE='./tmp/dados_saida.txt', STATUS='UNKNOWN', ACTION='READWRITE')
WRITE(10,*) 'AVALIACAO 1 - FORTRAN'
WRITE(10,*) 'Nome: ', 'ROMULO DE AGUIAR BENINCA'
CALL date_and_time(VALUES=DATA_HORA)
WRITE(10,"(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)") &
'Data: ', DATA_HORA(1), '/', DATA_HORA(2), '/', DATA_HORA(3), ' ', DATA_HORA(4), ':', DATA_HORA(5), ':', DATA_HORA(6)
WRITE(10,*) 'Valores lidos:'
WRITE(10, '(A, ES12.4)') 'b1 = ', REAL(b1)
WRITE(10, '(A, ES12.4)') 'b2 = ', REAL(b2)
WRITE(10, '(A, ES12.4)') 'b3 = ', REAL(b3)
WRITE(10, '(A, ES12.4)') 'b4 = ', REAL(b4)
WRITE(10, '(A, ES12.4)') 'k1 = ', k1
WRITE(10, '(A, ES12.4)') 'k2 = ', k2
WRITE(10, '(A, ES12.4)') 'k3 = ', k3
WRITE(10, '(A, ES12.4)') 'k4 = ', k4
WRITE(10, '(A, ES12.4)') 'k5 = ', k5
WRITE(10, '(A, ES12.4)') 'k6 = ', k6
WRITE(10, '(A, ES12.4)') 'k7 = ', k7
WRITE(10, '(A)',ADVANCE="NO") 'p = ', p
WRITE(10, '(A)' ,ADVANCE="NO") 'q = ', q
WRITE(10, '(A)' ,ADVANCE="NO") 'r = ', r


END PROGRAM parcial01
