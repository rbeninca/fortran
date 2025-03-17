program calcula_idade

    implicit none

    INTEGER :: data_nascimento(3),data_atual(8),idade;


    PRINT *, "Informe sua data de nascimento no formato dd mm aaaa"
    READ (*,'(I2,1X,I2,1X, I4)') data_nascimento(1), data_nascimento(2), data_nascimento(3) 
    !READ (*,*) data_nascimento(1), data_nascimento(2), data_nascimento(3)

    call date_and_time(values=data_atual)
    idade = data_atual(1) - data_nascimento(3)

    if (data_nascimento(2) > data_atual(2)) then
        idade = idade - 1
    else if (data_nascimento(2) == data_atual(2)) then
        if (data_nascimento(1) > data_atual(1)) then
            idade = idade - 1
        end if
    end if
    WRITE (*, '(A,I2.2,A,I2.2,A,I4.4)') "Você nasceu em:",&
               data_nascimento(1), "/", data_nascimento(2), "/", data_nascimento(3)
    print *, "Você tem ", idade, " anos de idade."

end program calcula_idade
