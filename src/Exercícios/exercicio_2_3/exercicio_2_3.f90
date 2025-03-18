!Editar um programa-fonte em FORTRAN para executar o seguinte algoritmo (passos):
!a) definir como inteiras as variáveis VAR1, VAR2 e SOMA
!b) ler os valores das variáveis VAR1 e VAR2
!c) calcular a soma de VAR1 e VAR2 e atribuir o resultado à variável SOMA
!d) escrever os valores lidos de VAR1 e VAR2 juntamente com comentários para identificá-los, como na
!Figura 2.9
!e) escrever o resultado da variável SOMA juntamente com um comentário para identificá-lo
!2) Compilar o programa-fonte
!3) Gerar o programa-executável
!4) Executar o programa para VAR1 = 3 e VAR2 = 4. O resultado esperado é SOMA = 7.

PROGRAM exercicio_2_3
    IMPLICIT NONE
    INTEGER :: VAR1, VAR2, SOMA
    !b) ler os valores das variáveis VAR1 e VAR2
    PRINT *, 'Digite o valor de VAR1 e em seguida o valor de VAR2'
    READ *, VAR1 , VAR2

    !c) calcular a soma de VAR1 e VAR2 e atribuir o resultado à variável SOMA
    SOMA= VAR1 + VAR2
    !d) escrever os valores lidos de VAR1 e VAR2 juntamente com comentários para identificá-los
    WRITE (*,"(A,I0,A,A,I0)") 'VAR1= ', VAR1  , " e " , 'VAR2= ', VAR2
    !e) escrever o resultado da variável SOMA juntamente com um comentário para identificá-lo
    WRITE (*,"(A,I0)") 'SOMA=VAR1+VAR2= ', SOMA
    
END PROGRAM exercicio_2_3