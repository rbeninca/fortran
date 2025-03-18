!Programa faz o calculo da tabuada de um número  recebido pelo usuário
PROGRAM tabuada
IMPLICIT NONE

!VARS
    INTEGER :: num, i ,width_number
    CHARACTER (LEN=20) :: argumento , fmt
    REAL numreal
    WRITE (*,"(T10,A)") "-------------------------"
!a) ler um número inteiro caso não tenha recebido na chamada do programa
    if (command_argument_count() == 0 )  then 
        WRITE (*,'(A)', ADVANCE="NO") "Digite um número inteiro para calcular a tabuada:"
        READ (*,*) num
    else
        CALL GET_COMMAND_ARGUMENT(1, argumento)
        READ (argumento,'(I8)') num
    end if
!a
    numreal=num
    WRITE (*,*) "O COMPRIMENO do Número é ",INT(LOG10(ABS(numreal))) + 1
    width_number=INT(LOG10(ABS(numreal)))+1
    WRITE (fmt,'(A,      I0      , A   ,     I0      ,   A  ,   I0       ,A)') &
               "(I",width_number,",A,I",width_number ,",A,I",width_number,")"
    PRINT *, fmt 
!b) calcular a tabuada do númer
    i=0
  
    WRITE (*,'(A,I8)') "Tabuada do " ,num
    DO WHILE (i<=10)
        WRITE (*,fmt) num,"*",i,"=",i*num
        i=i+1;
    END DO

    
END PROGRAM tabuada