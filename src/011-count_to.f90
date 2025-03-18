!PROGRAMA QUE RECEBE UM ARGUMENTO DO SYSTEMA DUANTE O LOAD E FAZ A CONTAGEM ATÉ O VALOR DO ARGUMENTO
PROGRAM cont_to_arg
IMPLICIT NONE 
!VARS 
    CHARACTER (LEN=20) ::arg1
    INTEGER :: I=0,N , ARGC
    
    ARGC=COMMAND_ARGUMENT_COUNT()

    IF (ARGC>0) THEN 
        CALL GET_COMMAND_ARGUMENT(1,arg1)
        !Converterndo para inteiro a string ascii recebida
        READ(arg1, '(I8)') N
        WRITE (*,'(A,I0)') "N=",N
        DO WHILE (I<=N)
            WRITE (*,'(I8,1X)',ADVANCE="NO")  I
            IF (I==N)THEN 
                WRITE (*,'(/)', ADVANCE="NO") 
            END IF 
            I=I+1
        END DO
    ELSE 
        PRINT *, "Nenhum parametro informado"
    END IF 
END PROGRAM cont_to_arg