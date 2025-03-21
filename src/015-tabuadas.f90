!PROGRAMA imprime a tabuala  de forma a caber nas linhas do terminal
PROGRAM tabuadas_screen_ajust

    !VARS 
    INTEGER numero_colunas
    CHARACTER (LEN=30) ::linha_arquivo

    !BEGIN PROGRAM
    !"Calcula o numero de colunas do terminal" 
    numero_colunas = get_num_cols()

    
        call imprime_tabuada(get_num_cols())
    


    CONTAINS 

    INTEGER FUNCTION get_num_cols()
        !VARS
        INTEGER num_col
        CALL EXECUTE_COMMAND_LINE("tput cols > temp_cols.txt", WAIT=.TRUE.)
        OPEN (10,FILE="temp_cols.txt",STATUS='OLD', ACTION='READWRITE')!"SCRATCH"
        READ (10,'(I5)') num_col
        CLOSE(UNIT=10,STATUS='DELETE')  
        get_num_cols=num_col
    END FUNCTION get_num_cols
    INTEGER FUNCTION get_comprimento( n)
    !calculo comprimento de um numero
    !VAR 
        INTEGER ,INTENT(IN) :: n
        REAL        :: aux;
        IF (n .NE. 0) THEN 
            aux=n
        ELSE 
            aux=1.0
        END IF 
        get_comprimento=INT(LOG10(ABS(aux)))+1
    END FUNCTION get_comprimento

    SUBROUTINE imprime_expressao(a,b,lenght_num,num_expressions)
        INTEGER  ,INTENT(IN)  :: a,b,lenght_num,num_expressions
        CHARACTER   (LEN=100):: fmt 
        !configurando string de formatação fmt para tabuada
         WRITE(fmt, '(A,I0,A,I0,A,I0,A)') '(A,I', lenght_num, ',A,I', lenght_num, ',A,I', lenght_num, 'A)'
        !imprimindo
        IF (a==10 .OR.  mod(a,num_expressions)+1==num_expressions) THEN
            WRITE (*,fmt) "|",a,"*",b,"=",a*b,"|";                   
        ELSE 
            WRITE (*,fmt,ADVANCE="NO") "|",a,"*",b,"=",a*b,"|";
        END IF
    END SUBROUTINE 

   SUBROUTINE imprime_tabuada(col)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: col
    INTEGER :: a, b, max_len_expression
    INTEGER :: max_expressoes_linha, tabuadas_impressas
    max_expressoes_linha = col / 12 ! |10*10=100|"
    tabuadas_impressas = 0
    DO WHILE (tabuadas_impressas <= 10)
        DO b = 0, 10
            DO a = tabuadas_impressas, MIN(tabuadas_impressas + max_expressoes_linha-1, 10) ! for a to ()
                max_len_expression = get_comprimento(a*10+1)
                CALL imprime_expressao(a, b, max_len_expression, max_expressoes_linha)
            END DO
        END DO
        tabuadas_impressas = tabuadas_impressas + max_expressoes_linha
        PRINT *
    END DO
END SUBROUTINE imprime_tabuada
END PROGRAM tabuadas_screen_ajust
