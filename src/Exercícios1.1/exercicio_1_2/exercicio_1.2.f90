!Programa que imprime uma mensagem na tela  em linhas diferentes nome de uma pessoa, nome cidade e numero de telefone
PROGRAM exercicio_1_2
!VARS
    CHARACTER(LEN=50)::nome="Romulo de Aguiar Beninca",cidade="Maringá" ,telefone="(44) 99999-9999"

    PRINT *, " -----------------------------"
    WRITE (*,'(A,A)') "Nome:" , nome
    WRITE (*,'(A,A)') "Cidade:", cidade
    WRITE (*,'(A,A)') "Telefone:", telefone

!BEGIN PROGRAM
END PROGRAM exercicio_1_2