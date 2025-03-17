PROGRAM programa_dias_de_vida
    IMPLICIT NONE

    INTEGER :: data_nascimento(3), data_atual(8)
    INTEGER :: idade, dias_de_vida   

    PRINT *, "Informe sua data de nascimento no formato dd mm aaaa:"
    READ (*,'(I2,1X,I2,1X,I4)') data_nascimento(1), data_nascimento(2), data_nascimento(3)

    CALL DATE_AND_TIME(values=data_atual)

    ! Chamada para a função RENOMEADA
    dias_de_vida = calcula_dias_de_vida(data_nascimento, data_atual)
    idade = calcula_idade(data_nascimento, data_atual)

    WRITE (*,'(A, I2.2, A, I2.2, A, I4.4)') "Você nasceu em: ", &
        data_nascimento(1), "/", data_nascimento(2), "/", data_nascimento(3)

    PRINT *, "Você tem ", idade, " anos de idade."
    PRINT *, "Você viveu ", dias_de_vida, " dias."

CONTAINS
!-----------------------------------------------------
! Função que calcula calcula a idade levando em consideração o mês e o dia de nascimento
    INTEGER FUNCTION calcula_idade(dt_nasc, date)
        IMPLICIT NONE
        !args de entrada
        INTEGER, INTENT(IN) :: dt_nasc(3), date(8)
        !variável local
        INTEGER :: idade_calc

        idade_calc = date(1) - dt_nasc(3)

        IF ( (dt_nasc(2) > date(2)) .OR. &
             ( (dt_nasc(2) == date(2)) .AND. (dt_nasc(1) > date(3)) ) ) THEN
            idade_calc = idade_calc - 1
        END IF

        calcula_idade = idade_calc
    END FUNCTION calcula_idade
!-----------------------------------------------------
! Função que calcula o numero de dias de vida 
    INTEGER FUNCTION calcula_dias_de_vida(dt_nasc, date)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: dt_nasc(3), date(8)
        INTEGER :: anos, meses, dias

        anos  = date(1) - dt_nasc(3)
        meses = date(2) - dt_nasc(2)
        dias  = date(3) - dt_nasc(1)

        IF (dias < 0) THEN
            dias = dias + 30
            meses = meses - 1
        END IF

        IF (meses < 0) THEN
            meses = meses + 12
            anos = anos - 1
        END IF

        calcula_dias_de_vida = anos * 365 + meses * 30 + dias
    END FUNCTION calcula_dias_de_vida

END PROGRAM programa_dias_de_vida
