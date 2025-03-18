!Escrever com um único comando de escrito o nome de uma pessoa, cidade e telefone em uma mesma linha
PROGRAM exercicio_1_3
!VARS
    CHARACTER(LEN=50)::nome="Romulo de Aguiar Beninca",cidade="Maringá" ,telefone="(44) 99999-9999"
!BEGIN PROGRAM
    WRITE (*,'(A,A,A,A,A,A,A)') "Nome:" , trim(nome)," - ","Cidade:", trim(cidade)," - ", "Telefone:", trim(telefone)
END PROGRAM exercicio_1_3
