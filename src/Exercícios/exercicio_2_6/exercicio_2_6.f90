!Exercício 2.6
!1) Editar um programa-fonte em FORTRAN para executar o seguinte algoritmo (passos):
!a) ler três números inteiros
!b) somar os três números e atribuir o resultado à variável SOMA
!c) calcular a média aritmética dos três números ao dividir SOMA por 3
!d) escrever os valores lidos, a SOMA e o valor da média aritmética juntamente com comentários para identificá-los, como na Figura 2.9
!2) Compilar o programa-fonte
!3) Gerar o programa-executável
!4) Executar o programa com os valores 1, 2 e 3. Em seguida, analisar o resultado da média fornecido pelo programa comparando-o com o valor esperado obtido por um cálculo mental. Resultado esperado para a média= 2.
!5) Repetir o item 4 para os valores 1, 1 e 2. Resultado esperado para a média = 1.
!6) Repetir o item 4 para os valores 1, 1 e 0. Resultado esperado para a média = 0.
PROGRAM exercicio_2_6
IMPLICIT NONE

!VARS
INTEGER :: A, B , C , SOMA,MEDIA
!a) ler três números inteiros
PRINT *, "Informe os valores de A  B e C"
WRITE (*, '(A)', ADVANCE="NO") "A:"
READ (*,*)  A 
WRITE (*, '(A)', ADVANCE="NO") "B:"
READ (*,*)  B 
WRITE (*, '(A)', ADVANCE="NO") "C:"
READ (*,*)  C

!b) somar os três números e atribuir o resultado à variável SOMA
SOMA=A+B+C
!c) calcular a média aritmética dos três números ao dividir SOMA por 3
MEDIA=SOMA/3
!d) escrever os valores lidos, a SOMA e o valor da média aritmética juntamente com comentários para identificá-los, como na Figura 2.9
WRITE (*,'(A,I0,/A,I0,/A,I0, /A,I0,/A,I0)' )  "A=",A ,  "B=", B , "C=",C , "SOMA=A+B+C=", SOMA, "MEDIA=((A+B+C)/3)=" , MEDIA
!5) Repetir o item 4 para os valores 1, 1 e 2. Resultado esperado para a média = 1.
    !Como se trata de uma divisão inteira o resultado de 4/3  seria 1.3 sendo que a parte fracionária é desconsiderada ficando 1.
!6) Repetir o item 4 para os valores 1, 1 e 0. Resultado esperado para a média = 0.
    !Como se tratata de uma divisão inteira que seria 2/3=0.6868  tem a parte fracionária desconsiderada então fica 0

END PROGRAM exercicio_2_6