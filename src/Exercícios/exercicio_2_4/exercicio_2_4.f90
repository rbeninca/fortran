!1) Editar um programa-fonte em FORTRAN para executar o seguinte algoritmo (passos):
!a) definir como inteiras as variáveis VAR1, VAR2 e X
!b) ler os valores das variáveis VAR1 e VAR2
!c) calcular o produto de VAR1 e VAR2 e atribuir o resultado à variável X
!d) escrever o seu nome completo
!e) escrever os valores lidos de VAR1 e VAR2 juntamente com comentários para identificá-los, como na Figura 2.9
!f) escrever o resultado da variável X juntamente com um comentário para identificá-lo
!2) Compilar o programa-fonte
!3) Gerar o programa-executável
!Executar o programa para VAR1 = 3 e VAR2 = 4. O resultado esperado é X = 12.
PROGRAM exercicio_2_4
    IMPLICIT NONE
!VARS
    !a) definir como inteiras as variáveis VAR1, VAR2 e X
    INTEGER :: VAR1=3, VAR2=4 , X
    CHARACTER (LEN=50) ::nome="Romulo de Aguiar Beninca"

!BEGIN PROGRAM

    !c) calcular o produto de VAR1 e VAR2 e atribuir o resultado à variável X
    X=VAR1*VAR2
    !d) escrever o seu nome completo
    WRITE (*,*) nome
    !e) escrever os valores lidos de VAR1 e VAR2 juntamente com comentários para identificá-los, como na Figura 2.9
    WRITE (*,'(A,I0)') "VAR1=",VAR1
    WRITE (*,'(A,I0)') "VAR2=",VAR2
    WRITE (*,'(A,I0)')  "X=VAR1*VAR2=", X

END PROGRAM exercicio_2_4