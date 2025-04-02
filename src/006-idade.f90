program calcula_idade

    IMPLICIT NONE

    INTEGER :: data_nascimento(3),data_atual(8),idade;


    PRINT *, "Informe sua data de nascimento no formato dd mm aaaa"
    READ (*,'(I2,1X,I2,1X, I4)') data_nascimento(1), data_nascimento(2), data_nascimento(3) 
    !READ (*,*) data_nascimento(1), data_nascimento(2), data_nascimento(3)

    CALL DATE_AND_TIME(values=data_atual)
    idade = data_atual(1) - data_nascimento(3)

    IF  ( (data_nascimento(2) > data_atual(2)) .or.&
    ( (data_nascimento(2) == data_atual(2)) .and. & 
    (data_nascimento(1) > data_atual(3)) ) ) THEN
        idade = idade - 1
    ELSE IF (data_nascimento(2) == data_atual(2)) THEN
        IF (data_nascimento(1) > data_atual(1)) THEN
            idade = idade - 1
        END IF
    END IF
    WRITE (*, '(A,I2.2,A,I2.2,A,I4.4)') "Você nasceu em:",&
               data_nascimento(1), "/", data_nascimento(2), "/", data_nascimento(3)
    PRINT  *, "Você tem ", idade, " anos de idade."

END PROGRAM calcula_idade
