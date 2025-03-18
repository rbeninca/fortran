!Exercício 2.7
!1) Editar um programa-fonte em FORTRAN para executar o seguinte algoritmo (passos):
!a) ler o primeiro valor (inteiro) de uma progressão aritmética (P.A.), denotado por A1
!b) ler a diferença (número inteiro) entre dois termos subseqüentes da P.A., denotada por D
!c) ler o número (inteiro) de termos da P.A., denotado por N
!d) calcular o valor (inteiro) do último termo da P.A., denotado por AN, com AN = A1 + (N–1)*D
!e) calcular o valor (inteiro) da soma de todos os termos da P.A., denotado por SN, com a seguinte expressão:
!SN = (A1+AN)*D/2
!f) escrever os três valores lidos e os dois calculados juntamente com comentários para identificá-los, como na Figura 2.9
!2) Compilar o programa-fonte
!3) Gerar o programa-executável
!4) Executar o programa para A1 = 1, D = 3 e N = 5. Os resultados devem ser AN = 13 e SN = 35.

PROGRAM exercicio_2_7
!VARS 
   INTEGER A1,N,D, AN, SN
!a) ler o primeiro valor (inteiro) de uma progressão aritmética (P.A.), denotado por A1
WRITE (*,'(A)', ADVANCE="NO") "Informe o primeiro valor da P.A definindo o valor de A1:"
READ (*,*) A1
!b) ler a diferença (número inteiro) entre dois termos subseqüentes da P.A., denotada por D
WRITE (*,'(A)',ADVANCE="NO") "Informe a distancia entre dois termos da P.A definindo D:"
READ (*,*) D
!c) ler o número (inteiro) de termos da P.A., denotado por N
WRITE (*,'(A)', ADVANCE="NO") "Informe o número de termos da P.A definindo N:"
READ (*,*) N
PRINT *, "---------------------------------------------"
WRITE (*,'(A,I0,/A,I0,/A,I0)') "A1=", A1, "D=",D,   "N=", N
!d) calcular o valor (inteiro) do último termo da P.A., denotado por AN, com AN = A1 + (N–1)*D
AN = A1 + ((N-1)*D)
WRITE (*,'(A,I0)') "AN = A1 + (N–1)*D=", AN 
!e) calcular o valor (inteiro) da soma de todos os termos da P.A., denotado por SN, com a seguinte expressão:
!a formula correta é (SN = (A1 + AN) * N / 2)   e não  SN = (A1 + AN) * D / 2
SN = (A1+AN)* N/2
WRITE (*,'(A,I0)') "SN = (A1+AN)*N/2=", SN

END PROGRAM exercicio_2_7 