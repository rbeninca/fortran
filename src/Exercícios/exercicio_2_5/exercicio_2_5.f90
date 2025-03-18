!Exercício 2.5
!1) Editar um programa-fonte em FORTRAN para executar o seguinte algoritmo (passos):
!a) definir como inteiras as variáveis VAR1, VAR2, P e D
!b) ler os valores das variáveis VAR1 e VAR2
!c) calcular a divisão de VAR1 por VAR2 e atribuir o resultado à variável D
!d) calcular VAR1 elevado à potência VAR2 e atribuir o resultado à variável P
!e) escrever o seu nome completo
!f) escrever os valores lidos de VAR1 e VAR2 juntamente com comentários para identificá-los, como na Figura 2.9
!g) escrever os resultados das variáveis D e P juntamente com comentários para identificá-los
!2) Compilar o programa-fonte
!3) Gerar o programa-executável
!4) Executar o programa para VAR1 = 9 e VAR2 = 3. O resultado esperado é D = 3 e P = 729.
PROGRAM exercicio_2_5
IMPLICIT NONE
!VARS 
!a) definir como inteiras as variáveis VAR1, VAR2, P e D
    INTEGER ::VAR1,VAR2,P,D
!BEGIN CODE
!b) ler os valores das variáveis VAR1 e VAR2
    WRITE (*,*) "Informe o valor de VAR1 e VAR2"
    READ (*,*) VAR1, VAR2
 !c) calcular a divisão de VAR1 por VAR2 e atribuir o resultado à variável D
    D=VAR1/VAR2
 !d) calcular VAR1 elevado à potência VAR2 e atribuir o resultado à variável P
    P=VAR1**VAR2
 !e) escrever o seu nome completo
    PRINT *, "Romulo de Aguiar Beninca"
!f) escrever os valores lidos de VAR1 e VAR2 juntamente com comentários para identificá-los, como na Figura 2.9
    WRITE (*,'(A,I0)')  "VAR1=", VAR1
    WRITE (*,'(A,I0)')  "VAR2=", VAR2
!g) escrever os resultados das variáveis D e P juntamente com comentários para identificá-los
    WRITE (*,'(A,I0)')  "D=VAR1/VAR2=" ,D 
    WRITE (*,'(A,I0)')  "P=VAR1**VAR2=" ,P 
END PROGRAM exercicio_2_5
